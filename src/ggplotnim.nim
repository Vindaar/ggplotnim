import sequtils, tables, sets, algorithm, strutils
import ginger
import parsecsv, streams, strutils

import random

import seqmath

import macros

import persvector

import ggplotnim / formula
export formula

export sets

import chroma
export chroma

type
  Aesthetics = object
    # In principle `x`, `y` are `Scale(scKind: scLinearData)`!
    # possibly `scTranformedData`.
    x: Option[string]
    y: Option[string]
    # Replace these by e.g. `color: Option[Scale]` and have `Scale` be variant
    # type that stores its kind `kind: scColor` and `key: string`.
    color: Option[Scale] # classify by color
    size: Option[Scale] # classify by size
    shape: Option[Scale] # classify by shape

  ScaleKind = enum
    scLinearData, scTransformedData, scColor, scShape, scSize

  DiscreteKind = enum
    dcDiscrete, dcContinuous

  ScaleValue = object
    case kind: ScaleKind
    of scLinearData:
      # just stores a data value
      val: Value
    of scTransformedData:
      # data under some transformation. E.g. log, tanh, ...
      rawVal: Value
      # where `trans` is our assigned transformation function
      trans: proc(v: Value): Value
    of scColor:
      # stores a color
      color: Color
    of scShape:
      # a marker kind
      marker: MarkerKind
    of scSize:
      # a size of something, e.g. a marker
      size: float

  # TODO: should not one scale belong to only one axis?
  # But if we do that, how do we find the correct scale in the seq[Scale]?
  # Replace seq[Scale] by e.g. Table[string, Scale] where string is some
  # static identifier we can calculate to retrieve it?
  # e.g. `xaxis`, `<name of geom>.xaxis` etc.?
  Scale = object
    # the column which this scale corresponds to
    col: string
    scKind: ScaleKind
    case kind: DiscreteKind
    of dcDiscrete:
      # For discrete data this is a good solution. How about continuous data?
      valueMap: OrderedTable[Value, ScaleValue]
      # seq of labels to access via index
      labelSeq: seq[Value]
    of dcContinuous:
      # For continuous we might want to add a `Scale` in the ginger sense
      dataScale: ginger.Scale
      # with this we can calculate on the fly the required values given the
      # data

  Facet = object
    discard

  # helper object to compose `ggsave` via `+` with `ggplot`
  Draw = object
    fname: string

  GeomKind = enum
    gkPoint, gkBar, gkHistogram, gkFreqPoly, gkTile, gkLine
  Geom = object
    style: Option[Style] # if set, apply this style instead of parent's
    aes: Aesthetics # a geom can have its own aesthetics. Needs to be part of
                    # the `Geom`, because if we add it to `GgPlot` we lose track
                    # of which geom it corresponds to
    case kind: GeomKind
    of gkHistogram, gkFreqPoly:
      bins: int # number of bins
      binWidth: float # width of bins in terms of the data
    else:
      discard

  GgPlot[T] = object
    data: T
    title: string
    subtitle: string
    # GgPlot can only contain a single `aes` by itself. Geoms may contain
    # seperate ones
    aes: Aesthetics
    numXticks: int
    numYticks: int
    facets: seq[Facet]
    geoms: seq[Geom]

proc getValue(s: Scale, label: Value): ScaleValue =
  ## returns the `ScaleValue` of the given Scale `s` for `label`
  result = s.valueMap[label]

proc getLabelKey(s: Scale, at: int): Value =
  ## returns the key at index `at` for the Scale `s`
  result = s.labelSeq[at]

iterator enumerateLabels*(s: Scale): Value =
  for k in s.labelSeq:
    yield k

iterator enumerateLabelPairs*(s: Scale): (int, Value) =
  for i, k in s.labelSeq:
    yield (i, k)

iterator pairs*(s: Scale): (Value, ScaleValue) =
  for k, v in s.valueMap:
    yield (k, v)

iterator values*(s: Scale): ScaleValue =
  for v in values(s.valueMap):
    yield v

iterator keys*(s: Scale): Value =
  ## NOTE: for clarity use `enumerateLabels` instead!
  for k in keys(s.valueMap):
    yield k

iterator enumerateScales(p: GgPlot, geom: Geom): Scale =
  ## yields each scale that is not `none` found in the `GgPlot` or
  ## `geom`. If the same scale is found in both `GgPlot` and the `geom`
  ## the `geom` Scale overrides the `GgPlot` scale!
  let
    paes = p.aes
    gaes = geom.aes
  # color Scale
  if gaes.color.isSome:
    yield gaes.color.unsafeGet
  elif paes.color.isSome:
    yield paes.color.unsafeGet
  # shape Scale
  if gaes.shape.isSome:
    yield gaes.shape.unsafeGet
  elif paes.shape.isSome:
    yield paes.shape.unsafeGet
  # size scale
  if gaes.size.isSome:
    yield gaes.size.unsafeGet
  elif paes.size.isSome:
    yield paes.size.unsafeGet

proc guessType(s: seq[Value]): ValueKind =
  ## returns a ``guess`` (!) of the data type stored in `s`.
  ## We check a subset of 100 elements of the seq (or the whole if
  ## < 100 elements) and see if they match a single ValueKind.
  ## If they do match, return it, else return `VNull`
  var r = initRand(299792458) # for now just set a local state
  let idxNum = min(99, s.high)
  let randIdx = toSeq(0 .. idxNum).mapIt(r.rand(s.high))
  result = VNull
  var resultSet = false
  for i in randIdx:
    if not resultSet:
      result = s[i].kind
      resultSet = true
    else:
      if result != s[i].kind:
        return VNull

proc isDiscreteData(s: seq[Value]): bool =
  ## returns an ``estimate`` (!) of whether the given sequence of
  ## data is most likely discrete or continuous. First determine
  ## most probable type, then check for discreteness
  ## - if Values are strings: discrete
  ## - if float / int: generate set of first 100 elements, check
  ##   if cardinality of set > 50: continuous, else discrete
  ## - if bool: discrete
  let guessedT = s.guessType
  case guessedT
  of VString:
    result = true
  of VFloat, VInt:
    # same approach as in `guessType`
    var r = initRand(42) # for now just set a local state
    let idxNum = min(99, s.high)
    let randIdx = toSeq(0 .. idxNum).mapIt(r.rand(s.high))
    let elements = randIdx.mapIt(s[it]).toHashSet
    if elements.card > (idxNum.float / 2.0).round.int:
      result = false
    else:
      result = true
  of VBool:
    result = true
  of VNull:
    result = false
  of VObject:
     raise newException(Exception, "A VObject can neither be discrete nor continuous!")

proc mapDataToScale(refVals: seq[Value], val: Value, scale: Scale): ScaleValue =
  let isDiscrete = refVals.isDiscreteData
  if isDiscrete:
    case scale.scKind
    of scColor, scSize, scShape:
      result = scale.getValue(val)
    of scLinearData:
      # that's just the value itself
      result = ScaleValue(kind: scLinearData, val: val)
    else:
      raise newException(Exception, "`mapDataToScale` not yet implemented for " &
        "discrete data of kind " & $scale.scKind)
  else:
    case scale.scKind
    of scLinearData:
      # again, that's still just the value itself
      result = ScaleValue(kind: scLinearData, val: val)
    of scSize:
      # TODO: regardless of continuous or discrete data, bin the
      # data and determine size by binning!
      const numSizes = 5
      const minSize = 2.0
      const maxSize = 7.0
      const stepSize = (maxSize - minSize) / numSizes.float
      let dataRange = (low: min(refVals), high: max(refVals))
      # calc the actual size for contiuous data
      result = ScaleValue(kind: scSize, size: minSize + (maxSize - minSize) *
        (val.toFloat - dataRange.low.toFloat) / (dataRange.high.toFloat - dataRange.low.toFloat))
    else:
      raise newException(Exception, "`mapDataToScale` not yet implemented for " &
        "continuous data of kind " & $scale.scKind)

proc dataTo[T: Table | OrderedTable | DataFrame; U](
  df: T,
  col: string,
  outType: typedesc[U]): seq[U]

proc fillScale(scaleOpt: Option[Scale], p: GgPlot,
               scKind: static ScaleKind): Option[Scale] =
  ## fills the `Scale` of `scKind` kind of the `aes`
  if not scaleOpt.isSome:
    return none[Scale]()
  let scale = scaleOpt.unsafeGet

  var res = Scale(scKind: scKind, col: scale.col)
  # get the data column we scale by
  var data: seq[Value]
  if scale.col in p.data:
    data = p.data.dataTo(scale.col, Value)
  else:
    data = @[Value(kind: VString, str: scale.col)]

  let isDiscrete = data.isDiscreteData
  if isDiscrete:
    # generate a discrete `Scale`
    res = Scale(scKind: scKind, col: scale.col, kind: dcDiscrete)
    # convert to set to filter duplicates, back to seq and sort
    # TODO: we could also use `sequtils.deduplicate` here
    res.labelSeq = data.toHashSet.toSeq.sorted
    var valueMap = initOrderedTable[Value, ScaleValue]()
    case scKind
    of scColor:
      let colorCs = ggColorHue(res.labelSeq.len)
      for i, k in res.labelSeq:
        valueMap[k] = ScaleValue(kind: scColor, color: colorCs[i])
    of scSize:
      let numSizes = min(res.labelSeq.len, 5)
      const minSize = 2.0
      const maxSize = 7.0
      let stepSize = (maxSize - minSize) / numSizes.float
      for i, k in res.labelSeq:
        valueMap[k] = ScaleValue(kind: scSize, size: minSize + i.float * stepSize)
    else:
      raise newException(Exception, "`fillScale` not implemented for " & $scKind)
    res.valueMap = valueMap
  result = some(res)

proc fillAes(p: GgPlot, aes: Aesthetics): Aesthetics =
  # TODO: we must estimate whether the given column is "continuous like" or
  # "discrete like"
  # If continuous like in case of
  # - color: create colorbar
  # - shape: raise exception not possible
  # - size: bin the data and use bins as fixed sizes
  result = aes
  result.color = aes.color.fillScale(p, scColor)
  # not implemented yet:
  #result.shape = aes.shape.fillScale(p, scShape)
  result.size = aes.size.fillScale(p, scSize)

proc addAes(p: var GgPlot, aes: Aesthetics) =
  ## adds the aesthetics to the plot. This is non trivial, because
  ## an aestetics encodes information that may have to be calculated
  p.aes = fillAes(p, aes)

proc ggplot*[T](data: T, aes: Aesthetics): GgPlot[T] =
  result = GgPlot[T](data: data,
                     numXticks: 10,
                     numYticks: 10)
  result.addAes aes
  # TODO: fill others with defaults

proc orNone(s: string): Option[string] =
  ## returns either a `some(s)` if s.len > 0 or none[string]()
  if s.len == 0: none[string]()
  else: some(s)

proc orNoneScale(s: string, scKind: static ScaleKind): Option[Scale] =
  ## returns either a `some(Scale)` of kind `ScaleKind` or `none[Scale]` if
  ## `s` is empty
  if s.len > 0:
    result = some(Scale(scKind: scKind, col: s))
  else:
    result = none[Scale]()

proc aes*(x = "", y = "", color = "", shape = "", size = ""): Aesthetics =
  result = Aesthetics(x: x.orNone, y: y.orNone,
                      color: color.orNoneScale(scColor),
                      shape: shape.orNoneScale(scShape),
                      size: size.orNoneScale(scSize))

proc aes*(x: FormulaNode, color = "", shape = "", size = ""): Aesthetics =
  result = Aesthetics(x: none[string](), y: none[string](),
                      color: color.orNoneScale(scColor),
                      shape: shape.orNoneScale(scShape),
                      size: size.orNoneScale(scSize))

func geom_point*(aes: Aesthetics = aes(),
                 color: Color = black,
                 size: float = 3.0): Geom =
  result = Geom(kind: gkPoint,
                style: some(Style(color: color,
                                  size: size)),
                aes: aes)

func geom_bar(): Geom =
  result = Geom(kind: gkBar)

func geom_line*(aes: Aesthetics = aes(),
                color: Color = grey20,
                size: float = 1.0,
                lineType: LineType = ltSolid): Geom =
  result = Geom(kind: gkLine,
                style: some(Style(color: color,
                                  lineWidth: size,
                                  lineType: lineType,
                                  fillColor: transparent)),
                aes: aes)


func geom_histogram*(binWidth = 0.0, bins = 0,
                     color: Color = grey20, # color of the bars
                    ): Geom =
  let style = Style(lineType: ltSolid,
                    lineWidth: 1.0, # draw 1 pt wide black line to avoid white pixels
                                    # between bins at size of exactly 1.0 bin width
                    color: color, # default color
                    fillColor: color)
  result = Geom(kind: gkHistogram,
                style: some(style))

func geom_freqpoly*(color: Color = grey20, # color of the line
                    size: float = 1.0, # line width of the line
                    lineType: LineType = ltSolid,
                   ): Geom =
  let style = Style(lineType: lineType,
                    lineWidth: size,
                    color: color,
                    fillColor: transparent)
  result = Geom(kind: gkFreqPoly,
                style: some(style))

proc geom_tile*(): Geom =
  result = Geom(kind: gkTile)

proc ggtitle*(title: string, subtitle = ""): (string, string) = (title, subtitle)

proc createLegend(view: var Viewport,
                  cat: Scale,
                  markers: seq[GraphObject]) =
  ## creates a full legend within the given viewport based on the categories
  ## in `cat` with a headline `title` showing data points of `markers`
  let startIdx = view.len
  case cat.kind
  of dcDiscrete:
    view.layout(1, rows = cat.valueMap.len + 1)
  of dcContinuous:
    # for now 5 sizes...
    view.layout(1, rows = 5 + 1)

  # iterate only over added children, skip first, because we actual legend first
  var j = 0
  for i in startIdx + 1 ..< view.len:
    # create rectangle showing style of data points
    var ch = view[i]
    let viewRatio = ch.hView.val / ch.wView.val
    let sizeY = ch.height.toPoints(some(ch.hView))
    let style = Style(lineType: ltSolid,
                      lineWidth: 1.0,
                      color: color(1.0, 1.0, 1.0),
                      fillColor: grey92)
    var rect = ch.initRect(c(0.0, 0.0),
                           quant(ch.height.val * viewRatio, ukRelative),
                           quant(1.0, ukRelative),
                           style = some(style))
    rect.name = "markerRectangle"
    # add marker ontop of rect
    # TODO: the markers given must contain all information already, that is:
    # - marker kind
    # - marker size
    # - marker color
    # then here we should just pop those in. Also need to differentiate between legends
    # made of markers and color bars!
    let point = ch.initPoint(Coord(x: c1(ch.height.val / 2.0 * viewRatio),
                                   y: c1(0.5)),
                             marker = markers[j].ptMarker,
                             size = markers[j].ptSize,
                             color = markers[j].ptColor)
    var labelText = ""
    case cat.scKind
    of scColor, scShape, scSize:
      labelText = $cat.getLabelKey(j)
    else:
      raise newException(Exception, "`createLegend` unsupported for " & $cat.scKind)

    let label = ch.initText(
      Coord(
        x: c1(ch.height.val * viewRatio) +
           c1(quant(0.3, ukCentimeter).toRelative(some(ch.wImg)).val),
        y: c1(0.5)),
      labelText,
      alignKind = taLeft
    )
    ch.addObj [rect, point, label]
    view[i] = ch
    inc j
  # get the first viewport for the header
  var header = view[startIdx]
  var label = header.initText(
    Coord(x: header.origin.x,
          y: header.origin.y + c1(header.height.val * 3.0 * header.hView.val / header.wView.val)),
    cat.col,
    alignKind = taLeft)
  # set to bold
  label.txtFont.bold = true
  header.addObj label
  view[startIdx] = header

proc `+`*(p: GgPlot, geom: Geom): GgPlot =
  ## adds the given geometry to the GgPlot object
  result = p
  # fill the aesthetics of the geom
  var mgeom = geom
  mgeom.aes = fillAes(p, geom.aes)
  result.geoms.add mgeom

proc `+`*(p: GgPlot, facet: Facet): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.facets.add facet

proc `+`*(p: GgPlot, scale: Scale): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.scales.add scale

proc `+`*(p: GgPlot, aes: Aesthetics): GgPlot =
  ## adds the given aesthetics to the GgPlot object
  result = p
  result.addAes aes

proc `+`*(p: GgPlot, titleTup: (string, string)): GgPlot =
  ## adds the given title / subtitle to the GgPlot object
  result = p
  result.title = titleTup[0]
  result.subtitle = titleTup[1]

proc requiresLegend(p: GgPlot): bool =
  ## returns true if the plot requires a legend to be drawn
  if p.aes.color.isSome or
     p.aes.size.isSome or
     p.geoms.anyIt(it.aes.color.isSome) or
     p.geoms.anyIt(it.aes.size.isSome):
    result = true
  else:
    result = false

proc plotLayoutWithLegend(view: var Viewport) =
  ## creates a layout for a plot in the current viewport that leaves space
  ## for a legend. Important indices of the created viewports:
  ## - main plot: idx = 4
  ## - legend: idx = 5
  view.layout(3, 3, colwidths = @[quant(2.0, ukCentimeter),
                                  quant(0.0, ukRelative),
                                  quant(5.0, ukCentimeter)],
              rowheights = @[quant(1.25, ukCentimeter),
                             quant(0.0, ukRelative),
                             quant(2.0, ukCentimeter)])

proc plotLayoutWithoutLegend(view: var Viewport) =
  ## creates a layout for a plot in the current viewport without a legend
  ## Main plot viewport will be:
  ## idx = 4
  view.layout(3, 3, colwidths = @[quant(2.0, ukCentimeter),
                                  quant(0.0, ukRelative),
                                  quant(1.0, ukCentimeter)],
              rowheights = @[quant(1.0, ukCentimeter),
                             quant(0.0, ukRelative),
                             quant(2.0, ukCentimeter)])

proc dataTo[T: Table | OrderedTable | DataFrame; U](
  df: T,
  col: string,
  outType: typedesc[U]): seq[U] =
  ## reads the column `col` from the Table / DataFrame and converts
  ## it to `outType`, returns it as a `seq[outType]`
  ## NOTE: This proc may also be used as a means to extract a column from
  ## a `DataFrame` as a `seq[Value]`, although that is identical to just
  ## calling `toSeq(df[column])`.
  ## NOTE: For now we just assume that a Table will be of kind
  ## `Table[string, seq[string]]`!
  when type(T) is Table or type(T) is OrderedTable:
    # well we just assume that the data is a string of numbers
    # so parse to float and then convert to out type
    # for proper support of string types etc., need the `Value` type
    when outType is SomeNumber:
      result = df[col].mapIt(it.parseFloat.outType)
    elif outType is string:
      result = df[col]
  else:
    # make a check of the type of the first element of the seq
    # TODO: replace by call to `guessType`
    let dkind = df[col][0].kind
    when outType is SomeNumber:
      case dkind
      of VInt:
        result = df[col].toSeq.mapIt(it.num.outType)
      of VFloat:
        result = df[col].toSeq.mapIt(it.fnum.outType)
      else: discard
    elif outType is string:
      case dkind
      of VString:
        result = df[col].toSeq.mapIt(it.str.outType)
      else: discard
    elif outType is bool:
      case dking
      of VBool:
        result = df[col].toSeq.mapIt(it.bval.outType)
      else: discard
    elif outType is Value:
      result = toSeq(df[col])
    else:
      case dkind
      of VObject:
        doAssert false, "there cannot be a column with object type!"
      of VNull:
        raise newException(Exception, "Column " & $col & " has no data!")
      else: discard

proc changeStyle(s: Style, scVal: ScaleValue): Style =
  ## returns a modified style with the appropriate field replaced
  result = s
  case scVal.kind
  of scColor:
    result.color = scVal.color
  of scSize:
    result.size = scVal.size
  else:
    raise newException(Exception, "Setting style of " & $scVal.kind & " not " &
      "supported at the moment!")

proc createPointGobj(view: var Viewport, p: GgPlot, geom: Geom): seq[GraphObject] =
  ## creates the GraphObjects for a `gkPoint` geom
  ## TODO: we could unify the code in the `create*Gobj` procs, by
  ## - making all procs in ginger take a `Style`
  ## - just build the style in the same way we do here (in a separate proc)
  ##   and create the `GraphObject`
  doAssert geom.kind == gkPoint
  doAssert geom.style.isSome
  var style = geom.style.unsafeGet
  var marker = mkCircle
  var any = false
  for scale in enumerateScales(p, geom):
    any = true
    var data: seq[Value]
    # TODO: we do not actually make use of `data`!
    if scale.col in p.data:
      data = p.data.dataTo(scale.col, Value)
    else:
      data = toSeq(0 ..< p.data.len).mapIt(Value(kind: VString, str: scale.col))
    for label, val in scale:
      when type(p.data) is DataFrame:
        let df = p.data.filter(f{scale.col == label})
      else:
        let df = toDf(p.data).filter(f{scale.col == label})
      # now get the labeled data
      let xData = df.dataTo(p.aes.x.get, float)
      let yData = df.dataTo(p.aes.y.get, float)
      # create all data points
      for i in 0 ..< xData.len:
        case scale.scKind
        of scShape:
          # Marker is not encoded in `ginger.Style`, hence get retrieve manually
          marker = scale.getValue(label).marker
        else:
          style = changeStyle(style, val)
        result.add initPoint(view, (x: xData[i], y: yData[i]),
                             marker = marker,
                             color = style.color,
                             size = style.size)
  if not any:
    let xData = p.data.dataTo(p.aes.x.get, float)
    let yData = p.data.dataTo(p.aes.y.get, float)
    # create points needed for polyLine
    for i in 0 ..< xData.len:
      result.add initPoint(view, (x: xData[i], y: yData[i]),
                           marker = marker,
                           color = style.color,
                           size = style.size)

proc createLineGobj(view: var Viewport,
                    p: GgPlot,
                    geom: Geom): seq[GraphObject] =
  ## creates the `goPolyLine` objects for the given geom
  doAssert geom.kind == gkLine
  doAssert geom.style.isSome
  var style = geom.style.unsafeGet
  var any = false
  for scale in enumerateScales(p, geom):
    any = true
    var data: seq[Value]
    # TODO: we do not actually make use of `data`!
    if scale.col in p.data:
      data = p.data.dataTo(scale.col, Value)
    else:
      data = toSeq(0 ..< p.data.len).mapIt(Value(kind: VString, str: scale.col))
    for label, val in scale:
      when type(p.data) is DataFrame:
        let df = p.data.filter(f{scale.col == label})
      else:
        let df = toDf(p.data).filter(f{scale.col == label})
      # now get the labeled data
      let xData = df.dataTo(p.aes.x.get, float)
      let yData = df.dataTo(p.aes.y.get, float)
      # create points needed for polyLine
      var points = newSeq[Point](xData.len)
      for i in 0 ..< xData.len:
        points[i] = (x: xData[i], y: yData[i])
      style = changeStyle(style, val)
      points = points.sortedByIt(it.x)
      result.add view.initPolyLine(points, some(style))
  if not any:
    let xData = p.data.dataTo(p.aes.x.get, float)
    let yData = p.data.dataTo(p.aes.y.get, float)
    # create points needed for polyLine
    var points = newSeq[Point](xData.len)
    for i in 0 ..< xData.len:
      points[i] = (x: xData[i], y: yData[i])
    # sort the points to that the lines are not connected arbitrarily
    points = points.sortedByIt(it.x)
    result.add view.initPolyLine(points, geom.style)

proc createHistFreqPolyGobj(view: var Viewport, p: GgPlot, geom: Geom): seq[GraphObject] =
  #for aes in p.aes:
  let aes = p.aes
  # before performing a calculation for the histogram viewports, get the
  # new xScale, by calling calcTickLocations with it
  # Note: we don't have to assign it to the `view` viewport, since that will
  # happen when calculation of the actual ticks will be done later on
  let (newXScale, _, _) = calcTickLocations(view.xScale, p.numXTicks)

  # generate the histogram itself
  let rawDat = p.data.dataTo(aes.x.get, float)
  const nbins = 30
  let binWidth = (newXScale.high - newXScale.low).float / nbins.float

  # TODO: if aes.colos.isSome we have to group the data we histogram first
  # and calculate histograms for each

  let (hist, _) = rawDat.histogram(bins = nbins, range = (newXScale.low, newXScale.high))

  # given the histogram, we can now deduce the base yScale we need
  let yScaleBase = (low: 0.0, high: hist.max.float)
  # however, this is not the final one, since we still have to calculate
  # the tick locations, which might change it again
  let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)

  # create viewports showing the bars
  view.yScale = newYScale

  var style: Option[Style]
  if geom.style.isSome:
    style = geom.style
  else:
    # TODO: inherit from parent somehow?
    discard
  if geom.kind == gkHistogram:
    view.layout(nbins, 1)
    var i = 0
    for p in mitems(view):
      doAssert p.yScale.high >= hist.max.float
      let yPos = 1.0 - quant(hist[i].float, ukData).toRelative(scale = some(p.yScale)).val
      let r = p.initRect(c(0.0, yPos), # bottom left
                         quant(1.0, ukRelative),
                         quant(hist[i].float, ukData),#.toRelative(scale = some(p.yScale)),
                         style = style)
      p.addObj r
      inc i
  else:
    doAssert geom.kind == gkFreqPoly
    # only single viewport will be used
    # calculate bin centers
    let binCenters = linspace(newXScale.low + binWidth / 2.0, newXScale.high - binWidth / 2.0, nbins)
    # build data points for polyLine
    var points = newSeq[Point](nbins)
    for i in 0 ..< nbins:
      points[i] = (x: binCenters[i], y: hist[i].float)

    # TODO: rewrite code such that user hands style, but we convert to
    # `Style` in geom?
    var style: Option[Style]
    if geom.style.isSome:
      style = geom.style
    else:
      # TODO: somehow inherit something from GgPlot, maybe via themes?
      discard
    # have to update the scale of our viewport! `initPolyLine` depends on it
    # However: we could change the `ginger` code to accept a seq[Coord] instead and
    # use the `newXScale, newYScale` as the ukData scale instead!
    view.xScale = newXScale
    result.add view.initPolyLine(points, style)

proc createGobjFromGeom(view: var Viewport, p: GgPlot, geom: Geom): seq[GraphObject] =
  ## performs the required conversion of the data from the data
  ## frame according to the given `geom`
  case geom.kind
  of gkPoint:
    result = view.createPointGobj(p, geom)
  of gkHistogram, gkFreqPoly:
    result = view.createHistFreqPolyGobj(p, geom)
  of gkLine:
    result = view.createLineGobj(p, geom)
  else:
    discard

proc generateLegendMarkers(plt: Viewport, aes: Aesthetics,
                           kind: ScaleKind): seq[GraphObject] =
  ## generate the required Legend Markers for the given `aes`
  case kind
  of scColor:
    if aes.color.isSome:
      let scale = aes.color.unsafeGet
      doAssert scale.scKind == scColor
      for i in 0 ..< scale.valueMap.len:
        let color = scale.getValue(scale.getLabelKey(i)).color
        result.add initPoint(plt,
                             (0.0, 0.0), # dummy coordinates
                             marker = mkCircle,
                             color = color) # assign same marker as above
  of scShape:
    if aes.shape.isSome:
      let scale = aes.shape.unsafeGet
      doAssert scale.scKind == scShape
      for i in 0 ..< scale.valueMap.len:
        result.add initPoint(plt,
                             (0.0, 0.0), # dummy coordinates
                             marker = scale.getValue(scale.getLabelKey(i)).marker)
  of scSize:
    if aes.size.isSome:
      let scale = aes.size.unsafeGet
      doAssert scale.scKind == scSize
      for i in 0 ..< scale.valueMap.len:
        let size = scale.getValue(scale.getLabelKey(i)).size
        result.add initPoint(plt,
                             (0.0, 0.0), # dummy coordinates
                             marker = mkCircle,
                             size = size)

  else:
    raise newException(Exception, "`createLegend` unsupported for " & $kind)


proc ggsave*(p: GgPlot, fname: string) =
  # check if any aes
  #doAssert p.aes.len > 0, "Needs at least one aesthetics!"
  const
    numXTicks = 10
    numYTicks = 10

  var
    xScale: ginger.Scale
    yScale: ginger.Scale
    colorsCat: OrderedTable[string, Color]
    colors: seq[string]
  #for aes in p.aes:
  # determine min and max scales of aes
  let xdata = p.data.dataTo(p.aes.x.get, float)
  # TODO: Check if `xdata.isDiscreteData` and handle discrete cases (possibly
  # also `string` data, after reading `xdata` not into `float`, but into `Value`.
  # TODO2: For latter we must make sure that reading `xdata` as `Value` actually
  # gives us `VFloat` values, instead of `VString` with floats as string
  # For a `DataFrame` this should work, but for a `Table` it won't (as it's
  # `seq[string]` internally).
  let
    minX = xdata.min
    maxX = xdata.max
  xScale = (low: minX, high: maxX)
  var
    minY: float
    maxY: float
  if p.aes.y.isSome:
    let ydata = p.data.dataTo(p.aes.y.get, float)
    minY = ydata.min
    maxY = ydata.max
    yScale = (low: minY, high: maxY)

  # create the plot
  var img = initViewport(xScale = some(xScale),
                         yScale = some(yScale))

  # NOTE: the question if ``not`` whether a `PLOT` requires a legend
  # but rather whether an `AESTHETIC` with an attached `SCALE` requires
  # legend! So check for each `aes` whether we have to draw a legend!
  # Assembly of the plot layout thus has to happen at the very end
  let drawLegend = p.requiresLegend
  if drawLegend:
    img.plotLayoutWithLegend()
  else:
    img.plotLayoutWithoutLegend()

  # get viewport of plot
  var plt = img[4]
  plt.background()
  for geom in p.geoms:
    # for each geom, we create a child viewport of `plt` covering
    # the whole viewport, which will house the data we just created.
    # Due to being a child, if will be drawn *after* its parent. This way things like
    # ticks will be below the data.
    # On the other hand this allows us to draw several geoms in on a plot and have the
    # order of the function calls `geom_*` be preserved
    var pChild = plt.addViewport()

    let data = pChild.createGobjFromGeom(p, geom)

    # add the data to the child
    pChild.addObj data
    # add the data viewport to the plt
    plt.children.add pChild

    # for these types there is no y data scale attached to the image so far,
    # thus we assign `pChild`'s data scale to it
    # TODO: solve this more elegantly!

    # potentially the creation of the graph objects have altered the scales
    # of the viewport. We have to make sure that the parents receive an updated
    # scale too
    plt.yScale = pChild.yScale
    img.yScale = pChild.yScale

  let
    xticks = plt.xticks(numXTicks)
    yticks = plt.yticks(numYTicks)
    xtickLabels = plt.tickLabels(xticks)
    ytickLabels = plt.tickLabels(yticks)
    xlabel = plt.xlabel(p.aes.x.get)
    grdlines = plt.initGridLines(some(xticks), some(yticks))
  var ylabel: GraphObject
  case p.geoms[0].kind
  of gkPoint:
    ylabel = plt.ylabel(p.aes.y.get)
  of gkHistogram:
    ylabel = plt.ylabel("count")
  else: discard
  plt.addObj concat(xticks, yticks, xtickLabels, ytickLabels, @[xlabel, ylabel, grdLines])
  img[4] = plt

  # draw legends
  for aes in concat(@[p.aes], p.geoms.mapIt(it.aes)):
    var lg = img[5]
    lg.height = quant(img[4].height.val / 2.0, ukRelative) #quant(0.5, ukRelative)
    lg.origin.y = lg.origin.y + c1(img[4].height.val / 8.0)
    lg.origin.x = lg.origin.x + img.c1(0.5, akX, ukCentimeter)
    var markers: seq[GraphObject]
    # TODO: The following currently creates stacked legends for each Scale that
    # requires one. Need to create a `seq[Viewport]` or something to first build
    # all legends and then calculate the sizes required.
    if aes.color.isSome:
      # handle color legend
      markers = lg.generateLegendMarkers(aes, scColor)
      let color = aes.color.unsafeGet
      lg.createLegend(color, markers)
    if aes.shape.isSome:
      # handle shape legend
      markers = lg.generateLegendMarkers(aes, scShape)
      let shape = aes.shape.unsafeGet
      lg.createLegend(shape, markers)
    if aes.size.isSome:
      # handle size legend
      markers = lg.generateLegendMarkers(aes, scSize)
      let size = aes.size.unsafeGet
      lg.createLegend(size, markers)

    img[5] = lg

  if p.title.len > 0:
    var titleView = img[1]
    let font = Font(family: "sans-serif",
                    size: 16.0,
                    color: black)
    let title = titleView.initText(c(0.0, 0.5),
                                   p.title,
                                   taLeft,
                                   font = some(font))
    titleView.addObj title
    img[1] = titleView
  img.draw(fname)

proc ggsave*(fname: string): Draw = Draw(fname: fname)

proc `+`*(p: GgPlot, d: Draw) =
  p.ggsave(d.fname)

proc readCsv*(fname: string): OrderedTable[string, seq[string]] =
  ## returns a CSV file as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  var s = newFileStream(fname, fmRead)
  if s == nil:
    quit("cannot open the file" & fname)

  var x: CsvParser
  open(x, s, fname)
  var isHeader = true
  x.readHeaderRow()
  result = initOrderedTable[string, seq[string]]()
  for col in items(x.headers):
    result[col.strip] = @[]
  while readRow(x):
    for col in items(x.headers):
      result[col.strip].add x.rowEntry(col).strip


when isMainModule:
  let mpg = readCsv("data/mpg.csv")
  let plt = ggplot(mpg, aes(x = "displ", y = "hwy")) +
    geom_point()
  plt.ggsave("scatter.pdf")
  let df = toDf(mpg)

  df.filter(f{"class" == "suv"}) # comparison via `f{}` macro
    .mutate(ratioHwyToCity ~ hwy / cty # raw untyped template function definition
    ) # <- note that we have to use normal UFCS to hand to `ggplot`!
    .ggplot(aes(x = "ratioHwyToCity", y = "displ", color = "class")) +
    geom_point() +
    ggsave("scatterFromDf.pdf")

  #let re = df.mutate(f{"cty_norm" ~ "cty" / abs("hwy")})
            # f{"displ_ccm" ~ "displ" * "1000.0"}, # displacement in ccm
            # unfortunately this is not yet possible. The `FormulaNode.fkVariable`
            # needs to be converted to type Value before we can do that

  let f = f{"cty_norm" ~ "cty" / mean("cty") * 2.0}
  echo "Func ", f

  let val = 1000
  let key = "cty"
  df.mutate(f{"cty_norm" ~ "cty" / mean(key) * val})
            # f{"displ_ccm" ~ "displ" * "1000.0"}, # displacement in ccm
            # unfortunately this is not yet possible. The `FormulaNode.fkVariable`
            # needs to be converted to type Value before we can do that
    .ggplot(aes(x = "displ", y = "cty_norm", color = "class")) +
    geom_point() +
    ggsave("classVsNormCty.pdf")

  ggplot(mpg, aes(x = "displ", y = "cty", color = "class")) +
    geom_point() +
    ggtitle("ggplotnim - or I Suck At Naming Things™") +
    ggsave("scatterColor.pdf")

  ggplot(mpg, aes("hwy")) +
    geom_histogram() +
    ggsave("simpleHisto.pdf")

  ggplot(mpg, aes("hwy")) +
    geom_freqpoly() +
    ggsave("freqpoly.pdf")

  ggplot(mpg, aes("hwy")) +
    geom_histogram() + # the order of the geom calls decides the order in which
                       # they are drawn! FreqPoly will be drawn on top of histo
    geom_freqpoly(color = parseHex("FD971F"),
                  size = 3.0) +
    ggsave("histoPlusFreqpoly.pdf")


  #let pltCalc = ggplot(mpg, aes(year ~ (displ * hwy + cty), color = "class")) +
  #  geom_point() +
  #  ggtitle("ggplotnim - or I Suck At Naming Things™") +
  #  ggsave("scatterColor.pdf")
