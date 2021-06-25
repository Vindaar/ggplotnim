import sequtils, tables, sets
import ggplot_types, ggplot_styles, ggplot_scales
import colormaps / viridisRaw
import datamancer
import ginger

iterator enumerateData(geom: FilledGeom): (Value, GgStyle, seq[GgStyle], DataFrame) =
  ## yields the pairs of continuous styles for the current discrete style and
  ## its data from `yieldData`
  for (label, tup) in pairs(geom.yieldData):
    yield (label, tup[0], tup[1], tup[2])

proc drawStackedPolyLine(view: var Viewport,
                         prevVals: seq[float],
                         linePoints: seq[Point],
                         style: Style): GraphObject =
  ## used both for `gkLine` as well as `gkPolyLine`!
  # It tries to take care of drawing separate poly lines for each "unconnected" line, i.e.
  # each line disconnected by more than 1 empty bin
  # This is somewhat complicated.
  var polyLines: seq[seq[Point]]
  let nElems = linePoints.len
  for i, p in linePoints:
    let (x, y) = p
    # add the current value to the current bin value
    let binVal = prevVals[i] + y
    if y > 0 or # has data int it, add
       binVal == 0 or # nothing in the bin yet, add
       polyLines[^1].len == 0 or # current polyLine is empty, add
      (polyLines[^1].len > 0 and i > 0 and # sanity checks
        (linePoints[i - 1].y > 0 and y == 0) # this element is empty, but last
                                             # was not, so add to draw back to 0
      ):
      polyLines[^1].add (x: x, y: binVal)
    elif polyLines[^1].len > 0 and i != nElems - 1 and linePoints[i + 1].y == 0:
      # only create new seq, if has content and next element is 0
      polyLines.add newSeq[Point]()
  # now create the poly lines from the data
  for line in polyLines:
    if line.len > 0:
      result = view.initPolyLine(line, some(style))

template getXY(view, df, xT, yT, fg, i, theme, xORK, yORK: untyped,
               xMaybeString: static bool = true): untyped =
  ## this template retrieves the current x and y values at index `i` from the `df`
  ## taking into account the view's scale and theme settings.
  # x may be a string! TODO: y at some point too.
  var x = xT[i]
  var y = yT[i]
  x = if x.kind == VNull: %~ 0.0 else: x
  y = if y.kind == VNull: %~ 0.0 else: y
  # modify according to ranges of viewport (may be different from real data ranges, assigned
  # to `FilledGeom`, due to user choice!
  # NOTE: We use templates here so that we can easily inject a `continue` to skip a data point!
  template maybeChange(cond, ax, axMarginRange, axORK: untyped): untyped =
    if cond:
      case axORK
      of orkDrop: continue
      of orkClip: ax = %~ axMarginRange
      of orkNone: discard # leave as isp
  if fg.dcKindX == dcContinuous:
    maybeChange(smallerOrFalse(x, view.xScale.low), x, theme.xMarginRange.low, xORK)
    maybeChange(largerOrFalse(x, view.xScale.high), x, theme.xMarginRange.high, xORK)
  if fg.dcKindY == dcContinuous:
    maybeChange(smallerOrFalse(y, view.yScale.low), y, theme.yMarginRange.low, yORK)
    maybeChange(largerOrFalse(y, view.yScale.high), y, theme.yMarginRange.high, yORK)
  (x, y)

proc readOrCalcBinWidth(df: DataFrame, idx: int,
                        dataCol: string,
                        dcKind: DiscreteKind,
                        col = "binWidths"): float =
  ## either reads the bin width from the DF for element at `idx`
  ## from the bin widths `col` or calculates it from the distance
  ## to the next bin in the `dataCol`.
  ## NOTE: Cannot be calculated for the last element of the DataFrame
  ## DataFrame. So make sure the DF contains the right bin edge
  ## (we assume bins are actually left edge) is included in the DF.
  case dcKind
  of dcContinuous:
    if col in df:
      result = df[col, idx].toFloat(allowNull = true)
    elif idx < df.high:
      let highVal = df[dataCol, idx + 1]
      case highVal.kind
      of VNull:
        # use idx - 1
        doAssert idx > 0
        result = (df[dataCol, idx].toFloat - df[dataCol, idx - 1].toFloat)
      else:
        result = (highVal.toFloat - df[dataCol, idx].toFloat)
  of dcDiscrete:
    # default width. TODO: Have to use argument / theme!
    result = 0.8

proc moveBinPosition(x: var Value, bpKind: BinPositionKind, binWidth: float) =
  ## moves `x` by half the bin width, if required by `bpKind`
  case bpKind
  of bpLeft, bpNone:
    # bpLeft requires no change, since bins are assumed to be left edge based
    # or bpNone if data is not bin like
    discard
  of bpCenter:
    # since our data is given as `bpLeft`, move half to right
    x = x + (%~ (binWidth / 2.0))
  of bpRight:
    x = x + (%~ binWidth)

proc readErrorData(df: DataFrame, idx: int, fg: FilledGeom):
  tuple[xMin, xMax, yMin, yMax: Option[float]] =
  ## reads all error data available
  ## TODO: we have to move this to `postprocess` to handle these properly
  ## Need to assign fields to `FilledGeom` depending on `geomKind`
  ## Here we then just have to check which field it is, which will tell us
  ## the column of `df`, then we just write
  ## `result.xMin = some(df[fg.xMin][idx, float])`
  if fg.xMin.isSome:
    result.xMin = some(df[fg.xMin.unsafeGet][idx, float])
  if fg.xMax.isSome:
    result.xMax = some(df[fg.xMax.unsafeGet][idx, float])
  if fg.yMin.isSome:
    result.yMin = some(df[fg.yMin.unsafeGet][idx, float])
  if fg.yMax.isSome:
    result.yMax = some(df[fg.yMax.unsafeGet][idx, float])

proc readWidthHeight(df: DataFrame, idx: int, fg: FilledGeom):
  tuple[width, height: float] =
  ## reads all error data available
  if fg.width.isSome:
    result.width = df[fg.width.unsafeGet][idx, float]
  else:
    result.width = 1.0
  if fg.height.isSome:
    result.height = df[fg.height.unsafeGet][idx, float]
  else:
    result.height = 1.0

proc readText(df: DataFrame, idx: int, fg: FilledGeom): string =
  ## reads all error data available
  result = df[fg.text][idx, string]

proc getOrDefault[T](val: Option[T], default: T = default(T)): T =
  result = if val.isSome: val.unsafeGet else: default

template colsRows(fg: FilledGeom): (int, int) =
  var
    cols = 1
    rows = 1
  if fg.dcKindX == dcDiscrete:
    cols = fg.xLabelSeq.len
  if fg.dcKindY == dcDiscrete:
    rows = fg.yLabelSeq.len
  (cols, rows)

proc prepareViews(view: var Viewport, fg: FilledGeom, theme: Theme) =
  ## prepares the required viewports in `view` for `fg` to be drawn in
  ## In each axis x,y will create N children viewports for the number
  ## of discrete labels along that axis. For continuous data no further
  ## children are created.
  var (cols, rows) = colsRows(fg)
  # modify if discretized
  # view.layout(cols, rows) # TODO: extend for discrete rows
  let discrMarginOpt = theme.discreteScaleMargin
  var discrMargin = quant(0.0, ukRelative)
  if discrMarginOpt.isSome:
    discrMargin = discrMarginOpt.unsafeGet
  var
    widths: seq[Quantity]
    heights: seq[Quantity]
  if cols > 1:
    let indWidths = toSeq(0 ..< cols).mapIt(quant(0.0, ukRelative))
    cols += 2
    widths = concat(@[discrMargin],
                    indWidths,
                    @[discrMargin])
  if rows > 1:
    let indHeights = toSeq(0 ..< rows).mapIt(quant(0.0, ukRelative))
    rows += 2
    heights = concat(@[discrMargin],
                     indHeights,
                     @[discrMargin])
  view.layout(cols, rows,
              colWidths = widths,
              rowHeights = heights)

proc calcViewMap(fg: FilledGeom): Table[(Value, Value), int] =
  ## maps a given label (`Value`) of a discrete axis to an `int` index,
  ## which corresponds to the `Viewport` the label has to be drawn to
  result = initTable[(Value, Value), int]()
  let (cols, rows) = colsRows(fg)
  if cols == 1 and rows == 1: return # nothing discrete, empty table
  elif rows == 1 and cols > 1:
    let y = Value(kind: VNull)
    for j in 0 ..< cols:
      let x = fg.xLabelSeq[j]
      result[(x, y)] = j + 1
  elif cols == 1 and rows > 1:
    let x = Value(kind: VNull)
    for i in 0 ..< rows:
      let y = fg.yLabelSeq[i]
      result[(x, y)] = i + 1
  else:
    for i in 0 ..< rows:
      let y = fg.yLabelSeq[i]
      for j in 0 ..< cols:
        let x = fg.xLabelSeq[j]
        # skip first row `(i + 1)`, respect margin columns `(cols + 2)` and
        # skip first column in current row `j + 1`
        result[(x, y)] = (i + 1) * (cols + 2) + (j + 1)

func getDiscreteHisto(fg: FilledGeom, width: float,
                      axKind: AxisKind): Coord1D =
  case axKind
  of akX:
    # calc left side of bar based on width, since we want the bar to be centered
    let left = (1.0 - width) / 2.0
    result = c1(left)
  of akY:
    # calc top side of bar based on width, since we want the bar to be centered
    let top = 1.0
    result = c1(top, ukRelative)

func getDiscretePoint(fg: FilledGeom, axKind: AxisKind): Coord1D =
  # discrete points are...
  result = c1(0.5, ukRelative)

func getDiscreteLine(view: Viewport, axKind: AxisKind): Coord1D =
  # discrete points are...
  case axKind
  of akX:
    result = c1(view.getCenter()[0], ukRelative)
  of akY:
    result = c1(view.getCenter()[1], ukRelative)

func getContinuous(view: Viewport, fg: FilledGeom, val: Value,
                   axKind: AxisKind): Coord1D {.inline.} =
  case axKind
  of akX:
    result = Coord1D(pos: val.toFloat, kind: ukData,
                     axis: akX,
                     scale: view.xScale)
  of akY:
    result = Coord1D(pos: val.toFloat, kind: ukData,
                     axis: akY,
                     scale: view.yScale)

proc getDrawPosImpl(
  view: Viewport, fg: FilledGeom, val: Value,
  width: float,
  dcKind: DiscreteKind, axKind: AxisKind): Coord1D =
  case dcKind
  of dcDiscrete:
    case fg.geomKind
    of gkPoint, gkErrorBar:
      result = getDiscretePoint(fg, axKind)
    of gkLine, gkFreqPoly:
      result = view.getDiscreteLine(axKind)
    of gkHistogram, gkBar:
      result = getDiscreteHisto(fg, width, axKind)
    of gkTile:
      result = getDiscreteHisto(fg, width, axKind)
    of gkRaster:
      result = getDiscreteHisto(fg, 1.0, axKind)
    of gkText:
      result = getDiscretePoint(fg, axKind)
  of dcContinuous:
    case fg.geomKind
    of gkPoint, gkErrorBar:
      result = view.getContinuous(fg, val, axKind)
    of gkLine, gkFreqPoly:
      result = view.getContinuous(fg, val, axKind)
    of gkHistogram, gkBar:
      result = view.getContinuous(fg, val, axKind)
    of gkTile, gkRaster:
      result = view.getContinuous(fg, val, axKind)
    of gkText:
      result = view.getContinuous(fg, val, axKind)

proc getDrawPos[T](view: Viewport, viewIdx: int,
                   fg: FilledGeom,
                   p: tuple[x: Value, y: Value],
                   binWidths: tuple[x, y: float], # bin widths
                   df: DataFrame, idx: int,
                   prevVals: var T): Coord =
  const CoordsFlipped = false # placeholder. Will be part of Theme
                              # if true, a discrete stacking / bar plot
                              # will be done parallel to x axis instead
  case fg.geom.position
  of pkIdentity:
    # ignore `prevVals`
    var mp = p
    if fg.geomKind == gkBar or (fg.geomKind == gkHistogram and fg.hdKind == hdBars):
      when not CoordsFlipped:
        mp.y = %~ 0.0
      else:
        mp.x = %~ 0.0
    result.x = view.getDrawPosImpl(fg, mp.x, binWidths.x, fg.dcKindX, akX)
    result.y = view.getDrawPosImpl(fg, mp.y, binWidths.y, fg.dcKindY, akY)
  of pkStack:
    var curStack: Value
    when T is Table[int, float]:
      if viewIdx notin prevVals:
        prevVals[viewIdx] = 0.0
      # if we have a histogram with `hdOutline` need to add `p.y` already
      if fg.geom.kind == gkHistogram and fg.geom.hdKind == hdOutline:
        curStack = p.y + %~ prevVals[viewIdx]
      else:
        curStack = %~ prevVals[viewIdx]
    elif T is seq[float]:
      if prevVals.len < df.len:
        prevVals = newSeq[float](df.len)
      # if we have a histogram with `hdOutline` need to add `p.y` already
      if fg.geom.kind == gkHistogram and fg.geom.hdKind == hdOutline:
        curStack = p.y + %~ prevVals[idx]
      else:
        curStack = %~ prevVals[idx]
    else:
      curStack = p.y
    when not CoordsFlipped:
      # stacking / histograms along the Y axis
      result.x = view.getDrawPosImpl(fg, p.x, binWidths.x, fg.dcKindX, akX)
      result.y = view.getDrawPosImpl(fg, curStack, binWidths.y, fg.dcKindY, akY)
    else:
      # stacking / histograms along the X axis
      result.x = view.getDrawPosImpl(fg, curStack, binWidths.x, fg.dcKindX, akX)
      result.y = view.getDrawPosImpl(fg, p.y, binWidths.y, fg.dcKindY, akY)
    # TODO: extend to coord flipped!
    when T is Table[int, float]:
      prevVals[viewIdx] += p.y.toFloat(allowNull = true)
    elif T is seq[float]:
      prevVals[idx] += p.y.toFloat(allowNull = true)
  else:
    doAssert false, "not implemented yet"

proc drawErrorBar(view: var Viewport, fg: FilledGeom,
                  pos: Coord,
                  df: DataFrame, idx: int, style: Style): GraphObject =
  # need the min and max values
  let (xMin, xMax, yMin, yMax) = readErrorData(df, idx, fg)
  if xMin.isSome or xMax.isSome:
    template toC1(val: float): Coord1D =
      Coord1D(pos: val,
              scale: view.xScale,
              axis: akX,
              kind: ukData)
    result = initErrorBar(view, pos,
                          errorUp = toC1(xMax.getOrDefault(0.0)),
                          errorDown = toC1(xMin.getOrDefault(0.0)),
                          axKind = akX,
                          ebKind = style.errorBarKind,
                          style = some(style))
  if yMin.isSome or yMax.isSome:
    template toC1(val: float): Coord1D =
      Coord1D(pos: val,
              scale: view.yScale,
              axis: akY,
              kind: ukData)
    result = initErrorBar(view, pos,
                          errorUp = toC1(yMax.getOrDefault(0.0)),
                          errorDown = toC1(yMin.getOrDefault(0.0)),
                          axKind = akY,
                          ebKind = style.errorBarKind,
                          style = some(style))

proc drawRaster(view: var Viewport, fg: FilledGeom, df: DataFrame) =
  let
    maxXCol = fg.xScale.high
    minXCol = fg.xScale.low
    maxYCol = fg.yScale.high
    minYCol = fg.yScale.low
    (wv, hv) = readWidthHeight(df, 0, fg)
  let height = (maxYCol - minYCol + hv)
  let width = (maxXCol - minXCol + wv)
  # compute number of elements in each dimension
  let
    numX = (width / wv).round.int
    numY = (height / hv).round.int
  var drawCb = proc(): seq[uint32] =
    result = newSeq[uint32](df.len)
    let xT = df[fg.xCol].toTensor(float)
    let yT = df[fg.yCol].toTensor(float)
    let zT = df[fg.fillCol].toTensor(float)
    let zScale = fg.fillDataScale
    for idx in 0 ..< df.len:
      let (x, y) = (((xT[idx] - minXCol) / wv).round.int,
                    ((yT[idx] - minYCol) / hv).round.int)
      var colorIdx = (255.0 * ((zT[idx] - zScale.low) /
                      (zScale.high - zScale.low))).round.int
      colorIdx = max(0, min(255, colorIdx))
      let cVal = ViridisRaw[colorIdx]
      template to256(x: float): int = (x * 256.0).int
      result[((numY - y - 1) * numX) + x] = (255 shl 24 or
                                             to256(cVal[0]) shl 16 or
                                             to256(cVal[1]) shl 8 or
                                             to256(cVal[2])).uint32

  template dataC1(at: float, ax: AxisKind): untyped =
    Coord1D(pos: at, kind: ukData, axis: ax,
            scale: if ax == akX: view.xScale else: view.yScale)
  template cData(px, py: float): untyped =
    Coord(x: dataC1(px, akX), y: dataC1(py, akY))
  view.addObj view.initRaster(cData(minXCol, maxYCol + hv),
                              quant(width, ukData),
                              quant(height, ukData),
                              numX = numX,
                              numY = numY,
                              drawCb = drawCb)

proc draw(view: var Viewport, fg: FilledGeom, pos: Coord,
          y: Value, # the actual y value, needed for height of a histogram / bar!
          binWidths: tuple[x, y: float],
          df: DataFrame,
          idx: int,
          style: Style) =
  case fg.geomKind
  of gkPoint: view.addObj view.initPoint(pos, style)
  of gkErrorBar: view.addObj view.drawErrorBar(fg, pos, df, idx, style)
  of gkHistogram, gkBar:
    let binWidth = readOrCalcBinWidth(df, idx, fg.xcol, dcKind = fg.dcKindX)
    doAssert binWidth == binWidths.x
    view.addObj view.initRect(pos,
                              quant(binWidth, ukData),
                              quant(-y.toFloat(allowNull = true), ukData),
                              style = some(style))
  of gkLine, gkFreqPoly:
    doAssert false, "Already handled in `drawSubDf`!"
  of gkTile:
    view.addObj view.initRect(pos,
                              quant(binWidths.x, ukData),
                              quant(-binWidths.y, ukData),
                              style = some(style))
  of gkRaster:
    doAssert false, "Already handled in `drawSubDf`!"
  of gkText:
    view.addObj view.initText(pos,
                              text = readText(df, idx, fg),
                              textKind = goText,
                              alignKind = style.font.alignKind,
                              font = some(style.font))

proc calcBinWidths(df: DataFrame, idx: int, fg: FilledGeom): tuple[x, y: float] =
  const CoordFlipped = false
  case fg.geomKind
  of gkHistogram, gkBar, gkPoint, gkLine, gkFreqPoly, gkErrorBar, gkText:
    when not CoordFlipped:
      result.x = readOrCalcBinWidth(df, idx, fg.xcol, dcKind = fg.dcKindX)
    else:
      result.y = readOrCalcBinWidth(df, idx, fg.ycol, dcKind = fg.dcKindY)
  of gkTile, gkRaster:
    (result.x, result.y) = readWidthHeight(df, idx, fg)

func moveBinPositions(p: var tuple[x, y: Value],
                      binWidths: tuple[x, y: float],
                      fg: FilledGeom) =
  const CoordFlipped = false
  case fg.geomKind
  of gkTile:
    p.x.moveBinPosition(fg.geom.binPosition, binWidths.x)
    p.y.moveBinPosition(fg.geom.binPosition, binWidths.y)
  else:
    when not CoordFlipped:
      p.x.moveBinPosition(fg.geom.binPosition, binWidths.x)
    else:
      p.y.moveBinPosition(fg.geom.binPosition, binWidths.y)

func getView(viewMap: Table[(Value, Value), int], p: tuple[x, y: Value], fg: FilledGeom): int =
  let px = if fg.dcKindX == dcDiscrete: p.x else: Value(kind: VNull)
  let py = if fg.dcKindY == dcDiscrete: p.y else: Value(kind: VNull)
  result = viewMap[(px, py)]

proc extendLineToAxis(linePoints: var seq[Coord], axKind: AxisKind,
                      df: DataFrame, fg: FilledGeom) =
  ## extends the given `linePoints` down to the axis given by `axKind` or
  ## simply to the extension of the full bin width to left and right.
  ## This makes sure all lines start at the main axis and on the outside edges
  ## of the edge bins.
  ## `axKind` refers to what is the ``main`` axis (default is `akX`)!
  ## so that the line does not start "in the air".
  var lStart = linePoints[0]
  var lEnd = linePoints[^1]
  case axKind
  of akX:
    # normal case, main axis is x
    # get bin width and add extension to left
    lStart.y.pos = 0.0
    var binWidth: float
    if fg.geomKind == gkFreqPoly: # extend by bin width for freq poly
      binWidth = readOrCalcBinWidth(df, 0, fg.xcol, dcKind = fg.dcKindX)
      lStart.x.pos = lStart.x.pos - binWidth
    # insert at beginning
    linePoints.insert(lStart, 0)
    # now get last bin  width and extend to right
    lEnd.y.pos = 0.0
    if fg.geomKind == gkFreqPoly: # extend by bin width for freq poly
      binWidth = readOrCalcBinWidth(df, df.high - 1, fg.xcol, dcKind = fg.dcKindX)
      lEnd.x.pos = lEnd.x.pos + binWidth
    # add at the end
    linePoints.add(lEnd)
  of akY:
    # inverted case
    # get bin width, extend to top
    var binWidth: float
    lStart.x.pos = 0.0
    if fg.geomKind == gkFreqPoly: # extend by bin width for freq poly
      binWidth = readOrCalcBinWidth(df, 0, fg.ycol, dcKind = fg.dcKindY)
      lStart.y.pos = lStart.y.pos - binWidth
    # insert at beginning
    linePoints.insert(lStart, 0)
    # now to bottom
    lEnd.x.pos = 0.0
    if fg.geomKind == gkFreqPoly: # extend by bin width for freq poly
      binWidth = readOrCalcBinWidth(df, df.high - 1, fg.ycol, dcKind = fg.dcKindY)
      lEnd.y.pos = lEnd.y.pos + binWidth
    # add at the end
    linePoints.add(lEnd)

proc convertPointsToHistogram(df: DataFrame, fg: FilledGeom,
                              linePoints: seq[Coord]): seq[Coord] =
  ## converts a given set of coordinates describing a histogram to a line,
  ## which describes the outline of a histogram. This is a useful way of
  ## drawing histograms as an alternative to drawing individual bars, because
  ## it does not suffer from the following issues:
  ## - bars that touch don't "actually" touch due to aliasing in vector
  ##   graphics rendering
  ## - drawing outlines of histograms can now be accomplished without having
  ##   visible bars between each bin (i.e. using alpha on histogram fills).
  ##   Setting alpha of outline to alpha inside does not work, due to overlapping
  ##   outlines of neighboring bars.

  result = newSeq[Coord]()
  var
    p: Coord = linePoints[0]
    curP: Coord
    curX: float = p.x.pos
    curY: float = 0.0
  var binWidth = readOrCalcBinWidth(df, 0, fg.xcol, dcKind = fg.dcKindX)
  p.x.pos = curX
  p.y.pos = curY
  result.add p
  curY = linePoints[0].y.pos
  p.y.pos = curY
  result.add p
  curX = curX + binWidth
  p.x.pos = curX
  result.add p
  for idx in 1 ..< linePoints.len:
    binWidth = readOrCalcBinWidth(df, 0, fg.xcol, dcKind = fg.dcKindX)
    curP = linePoints[idx]
    curY = curP.y.pos
    p.y.pos = curY
    result.add p
    curX = curX + binWidth
    p.x.pos = curX
    result.add p

proc drawSubDf[T](view: var Viewport, fg: FilledGeom,
                  viewMap: Table[(Value, Value), int],
                  df: DataFrame,
                  prevVals: var T,
                  styles: seq[GgStyle],
                  theme: Theme) =
  ## draws the given sub df
  # get behavior for elements outside the plot range
  let xOutsideRange = if theme.xOutsideRange.isSome: theme.xOutsideRange.unsafeGet else: orkClip
  let yOutsideRange = if theme.yOutsideRange.isSome: theme.yOutsideRange.unsafeGet else: orkClip
  # needed for histogram
  var
    style = mergeUserStyle(styles[0], fg.geom.userStyle, fg.geomKind)
    locView = view # current view, either child of `view` or `view` itself
    viewIdx = 0
    p: tuple[x, y: Value]
    pos: Coord
    binWidths: tuple[x, y: float]
  let needBinWidth = (fg.geomKind in {gkBar, gkHistogram, gkTile, gkRaster} or
                      fg.geom.binPosition in {bpCenter, bpRight})

  var linePoints: seq[Coord]
  if fg.geomKind notin {gkRaster}:
    linePoints = newSeqOfCap[Coord](df.len)
    var xT = df[$fg.xcol].toTensor(Value)
    var yT = df[$fg.ycol].toTensor(Value)
    # determine last position to draw. If `binPosition` is `bpNone` we're not interpreting data as
    # bins, use last. Else last is right bin edge, drop it
    let lastElement = if fg.geom.binPosition == bpNone: df.high
                      else: df.high - 1
    for i in 0 .. lastElement:
      if styles.len > 1:
        style = mergeUserStyle(styles[i], fg.geom.userStyle, fg.geomKind)
      # get current x, y values, possibly clipping them
      p = getXY(view, df, xT, yT, fg, i, theme, xOutsideRange,
                yOutsideRange, xMaybeString = true)
      if viewMap.len > 0:
        # get correct viewport if any is discrete
        viewIdx = getView(viewMap, p, fg)
        locView = view[viewIdx]
      if needBinWidth:
        # potentially move the positions according to `binPosition`
        binWidths = calcBinWidths(df, i, fg)
        moveBinPositions(p, binWidths, fg)
      pos = getDrawPos(locView, viewIdx,
                       fg,
                       p = p,
                       binWidths = binWidths,
                       df, i,
                       prevVals)
      case fg.geom.position
      of pkIdentity:
        case fg.geomKind
        of gkLine, gkFreqPoly, gkRaster: linePoints.add pos
        of gkHistogram:
          if fg.hdKind == hdOutline: linePoints.add pos
          else: locView.draw(fg, pos, p.y, binWidths, df, i, style)
        else: locView.draw(fg, pos, p.y, binWidths, df, i, style)
      of pkStack:
        case fg.geomKind
        of gkLine, gkFreqPoly, gkRaster: linePoints.add pos
        of gkHistogram:
          if fg.hdKind == hdOutline: linePoints.add pos
          else: locView.draw(fg, pos, p.y, binWidths, df, i, style)
        else: locView.draw(fg, pos, p.y, binWidths, df, i, style)
      of pkDodge:
        discard
      of pkFill:
        discard
      if viewMap.len > 0:
        view[viewIdx] = locView
  if viewMap.len == 0:
    view = locView

  # return ``early`` if histogram, cause bar histogram already drawn
  if fg.geomKind == gkHistogram and fg.hdKind == hdBars: return
  elif fg.geomKind == gkHistogram:
    linepoints = df.convertPointsToHistogram(fg, linepoints)


  # for `gkLine`, `gkFreqPoly` now draw the lines
  case fg.geomKind
  of gkLine, gkFreqPoly, gkHistogram:
    if styles.len == 1:
      let style = mergeUserStyle(styles[0], fg.geom.userStyle, fg.geomKind)
      # connect line down to axis, if fill color is not transparent
      if style.fillColor != transparent or fg.geomKind == gkFreqPoly:
        ## TODO: check `CoordFlipped` so that we know where "down" is!
        linePoints.extendLineToAxis(akX, df, fg)
      view.addObj view.initPolyLine(linePoints, some(style))
    else:
      # since `ginger` doesn't support gradients on lines atm, we just draw from
      # `(x1/y1)` to `(x2/y2)` with the style of `(x1/x2)`. We could build the average
      # of styles between the two, but we don't atm!
      echo "WARNING: using non-gradient drawing of line with multiple colors!"
      if style.fillColor != transparent or fg.geomKind == gkFreqPoly:
        ## TODO: check `CoordFlipped` so that we know where "down" is!
        linePoints.extendLineToAxis(akX, df, fg)
      for i in 0 ..< styles.high: # last element covered by i + 1
        let style = mergeUserStyle(styles[i], fg.geom.userStyle, fg.geomKind)
        view.addObj view.initPolyLine(@[linePoints[i], linePoints[i+1]], some(style))
  of gkRaster:
    ## TODO: currently ignores the `linepoints` completely. These would include the
    ## position of the centers of each pixel. But since it's a rectilinear grid, we can
    ## calculate the shift once and apply it to the position of the bitmap instead!
    view.drawRaster(fg, df)
  else: discard

proc createGobjFromGeom*(view: var Viewport,
                         fg: FilledGeom,
                         theme: Theme,
                         labelVal = none[Value]()) =
  ## performs the required conversion of the data from the data
  ## frame according to the given `geom`
  view.prepareViews(fg, theme)
  # if discretes, calculate mapping from labels to viewport
  var viewMap = calcViewMap(fg)
  var prevValsCont = newSeq[float]()
  var prevValsDiscr = initTable[int, float]()
  let anyDiscrete = if viewMap.len == 0: false else: true
  for (lab, baseStyle, styles, subDf) in enumerateData(fg):
    if labelVal.isSome:
      if labelVal.unsafeGet notin lab:
        # skip this label
        continue
    if fg.geom.position == pkStack and anyDiscrete:
      view.drawSubDf(fg, viewMap, subDf,
                     prevValsDiscr,
                     styles, theme)
    elif fg.geom.position == pkStack and not anyDiscrete:
      view.drawSubDf(fg, viewMap, subDf,
                     prevValsCont,
                     styles, theme)
    else:
      # no stacking, prevVals arg given as float
      # TODO: find better data type
      var dummy = 0.0
      view.drawSubDf(fg, viewMap, subDf,
                     dummy,
                     styles, theme)
