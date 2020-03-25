import sequtils, algorithm, tables

import ggplot_types, formula, ggplot_styles, ggplot_utils
import ginger except Scale

from seqmath import histogram, linspace, round

func getScales(gid: uint16, filledScales: FilledScales,
               yIsNone = false): (Scale, Scale, seq[Scale]) =
  ## Returns the x and y scales individually and the other scales as a
  ## sequence
  template getScale(field: untyped): untyped =
    let moreScale = field.more.filterIt(gid in it.ids)
    doAssert moreScale.len <= 1, "FOUND " & $moreScale & " for gid " & $gid
    if moreScale.len == 1:
      some(moreScale[0])
    elif field.main.isSome:
      field.main
    else:
      none[Scale]()
  template addIfAny(toAdd, arg: untyped): untyped =
    if arg.isSome:
      toAdd.add arg.get

  # just normal x against y
  let xOpt = getScale(filledScales.x)
  let yOpt = getScale(filledScales.y)
  doAssert xOpt.isSome
  result[0] = xOpt.get
  if not yIsNone:
    doAssert yOpt.isSome
    result[1] = yOpt.get
  addIfAny(result[2], getScale(filledScales.color))
  addIfAny(result[2], getScale(filledScales.fill))
  addIfAny(result[2], getScale(filledScales.size))
  addIfAny(result[2], getScale(filledScales.shape))
  addIfAny(result[2], getScale(filledScales.xMin))
  addIfAny(result[2], getScale(filledScales.xMax))
  addIfAny(result[2], getScale(filledScales.yMin))
  addIfAny(result[2], getScale(filledScales.yMax))
  addIfAny(result[2], getScale(filledScales.width))
  addIfAny(result[2], getScale(filledScales.height))
  #addIfAny(result[2], getScale(filledScales.text)) # not required
  addIfAny(result[2], getScale(filledScales.yRidges))

func isEmpty(s: ginger.Scale): bool =
  ## checks if the given scale is empty
  result = s.low == s.high

func mergeScales(s1, s2: ginger.Scale): ginger.Scale =
  ## merges the two data scales and returns a version encompassing both
  if s1.low != s1.high:
    result = (low: min(s1.low, s2.low),
              high: max(s1.high, s2.high))
  else:
    result = s2

proc applyTransformations(df: var DataFrame, scales: seq[Scale]) =
  ## Given a sequence of scales applies all transformations of the `scales`.
  ## That is for each `scTransformedData` scale the according transformation
  ## is applied its column
  # NOTE: This proc is responsible for transforming the input `df` from the
  # way it's defined by the user to one where the columns of type `FormualNode`
  # are applied. This changes the columns that are part of the data frame and
  # the column names. After this procedure we do ``not`` use the columns as
  # `FormulaNode` anymore, but ``only`` as strings, since all possible
  # transformations have been applied!
  var fns: seq[FormulaNode]
  for s in scales:
    if s.scKind == scTransformedData:
      let col = evaluate(s.col)
      let fn = f{ col ~ s.trans( col ) }
      fns.add fn
    else:
      # `s.col` may be pointing to scale which sets constant value or involves
      # a calculation of a column
      fns.add s.col
  # apply transformations using inplace mutate
  df.mutateInplace(fns)

proc separateScalesApplyTrafos(
  df: var DataFrame, gid: uint16,
  filledScales: FilledScales, yIsNone = false):
    tuple[x: Scale, y: Scale, discretes: seq[Scale], cont: seq[Scale]] =
  # NOTE: if `yIsNone = true` the `y` in the next line will be an empty scale,
  # caller has to be aware of that!
  let (x, y, scales) = getScales(gid, filledScales, yIsNone = yIsNone)
  # apply scale transformations
  if not yIsNone:
    df.applyTransformations(concat(@[x, y], scales))
  else:
    df.applyTransformations(concat(@[x], scales))
  # split by discrete and continuous
  let discretes = scales.filterIt(it.dcKind == dcDiscrete)
  let cont = scales.filterIt(it.dcKind == dcContinuous)
  result = (x: x, y: y, discretes: discretes, cont: cont)

proc splitDiscreteSetMap(df: DataFrame,
                         scales: seq[Scale]): (seq[FormulaNode], seq[FormulaNode]) =
  ## splits the given discrete (!) columns by whether they set data (i.e.
  ## arg not a DF column) or map data (arg is column)
  var setDiscCols = newSeq[FormulaNode]()
  var mapDiscCols = newSeq[FormulaNode]()
  for d in scales:
    # for discrete scales, build the continuous (if any) scales
    if d.col.isColumn(df):
      mapDiscCols.add d.col
    else:
      setDiscCols.add d.col
  result = (setDiscCols, mapDiscCols)

proc setXAttributes(fg: var FilledGeom,
                    df: DataFrame,
                    scale: Scale) =
  ## sets the X related attributes taking into account the discrete kind and the current
  ## number of elements. Mainly this means to set the `xScale` either according to
  ## the current DF and the last one (given discrete classification) and determine
  ## the number of elements in X
  case scale.dcKind
  of dcDiscrete:
    # for discrete scales the number of elements is the number of unique elements
    # (consider mpg w/ gkPoint using aes("cyl", "hwy") gives N entries for each "cyl"
    fg.numX = max(fg.numX, scale.col.evaluate(df).unique.len)
    # for a discrete scale an X scale isn't needed and makes it harder to produce
    # gkLine plots with a discrete scale
    fg.xScale = (low: 0.0, high: 1.0)
    # and assign the label sequence
    fg.xLabelSeq = scale.labelSeq
  of dcContinuous:
    fg.numX = max(fg.numX, df.len)

proc applyContScaleIfAny(yieldDf: DataFrame,
                         fullDf: DataFrame,
                         scales: seq[Scale], baseStyle: GgStyle): (seq[GgStyle], DataFrame) =
  ## given continuous `scales` (if any) return the correct scales based
  ## on each of these scales
  ## NOTE: This modifies `yieldDf` adding all continuous scale columns to it
  result[1] = yieldDf
  for c in scales:
    ## TODO: verify this should be `yieldDf`
    result[1][$c.col] = c.col.evaluate(yieldDf) #fullDf)
    case c.scKind
    of scLinearData, scTransformedData:
      # for linear and transformed data we don't change the style
      discard
    else:
      for el in c.mapData(yieldDf):
        result[0].add baseStyle.changeStyle(el)
  if result[0].len == 0:
    result = (@[baseStyle], yieldDf)

proc addCountsByPosition(sumCounts: var DataFrame, df: DataFrame,
                         col: string, pos: PositionKind) =
  ## adds the `df` column `col` elements to the `sumCounts` data frame in the
  ## same column taking into account the geom position kind.
  case pos
  of pkStack:
    if sumCounts.len == 0:
      sumCounts = df
    else:
      for i in 0 ..< df.len:
        sumCounts[col, i] = sumCounts[col, i] + df[col, i]
  of pkIdentity, pkDodge:
    sumCounts = df
  of pkFill: sumCounts[col] = toVector(%~ @[1]) # max for fill always 1.0

proc addBinCountsByPosition(sumHist: var seq[int], hist: seq[int],
                            pos: PositionKind) =
  ## adds the `hist` sequence elements to the `sumHist` sequence taking into
  ## account the geom position kind
  case pos
  of pkStack:
    if sumHist.len == 0:
      sumHist = hist
    else:
      for i in 0 .. sumHist.high:
        sumHist[i] += hist[i]
  of pkIdentity, pkDodge:
    sumHist = hist
  of pkFill: sumHist = @[1] # max for fill always 1.0

proc addZeroKeys(df: var DataFrame, keys: seq[Value], xCol, countCol: string) =
  ## Adds the `keys` columns which have zero count values to the `df`.
  ## This is needed for `count` stats, since the `groups` iterator does not
  ## yield empty subgroups, yet we need those for the plot.
  let existKeys = df[xCol].unique
  let zeroKeys = keys.filterIt(it notin existKeys)
  let zeroVals = zeroKeys.mapIt(0)
  df.add seqsToDf({ xCol: zeroKeys, countCol: zeroVals })

func encompassingDataScale(scales: seq[Scale],
                           axKind: AxisKind,
                           baseScale: ginger.Scale = (low: 0.0, high: 0.0)): ginger.Scale =
  ## calculate the encompassing data scale spanned by all
  ## given `scales` of kind `scLinearData`, `scTransformedData`.
  if not baseScale.isEmpty:
    result = baseScale
  for s in scales:
    if s.scKind in {scLinearData, scTransformedData} and
       s.axKind == axKind:
      result = mergeScales(result, s.dataScale)

proc determineDataScale(s: Scale,
                        additional: seq[Scale], df: DataFrame): ginger.Scale =
  ## returns the data scale given a filled `Scale s` and the corresponding data,
  ## while differentiating between continuous and discrete scales
  case s.dcKind
  of dcContinuous:
    result = if s.datascale.isEmpty: # happens for input DFs with 1-2 elements
              (low: colMin(df, $s.col), high: colMax(df, $s.col))
             else:
               s.datascale
    # now merge with all additional data scales along the same axis
    result = encompassingDataScale(additional, s.axKind, result)
  of dcDiscrete:
    # for discrete case assign default [0, 1] scale
    # TODO: assign somewhere else?
    result = (low: 0.0, high: 1.0)

proc filledIdentityGeom(df: var DataFrame, g: Geom,
                        filledScales: FilledScales): FilledGeom =
  let (x, y, discretes, cont) = df.separateScalesApplyTrafos(g.gid,
                                                             filledScales)
  let contCols = cont.mapIt(it.col)
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  result = FilledGeom(geom: g,
                      xcol: $x.col,
                      ycol: $y.col,
                      dcKindX: x.dcKind,
                      dcKindY: y.dcKind)
  result.xScale = determineDataScale(x, cont, df)
  result.yScale = determineDataScale(y, cont, df)
  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols.mapIt($it))
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      let yieldDf = subDf
      result.setXAttributes(yieldDf, x)
      let styleLabel = StyleLabel(style: style, label: toObject(keys))
      result.yieldData[styleLabel] = applyContScaleIfAny(yieldDf, df, cont, style)
  else:
    let yieldDf = df
    result.setXAttributes(yieldDf, x)
    let styleLabel = StyleLabel(style: style, label: Value(kind: Vnull))
    result.yieldData[styleLabel] = applyContScaleIfAny(yieldDf, df, cont, style)

  case y.dcKind
  of dcDiscrete: result.yLabelSeq = y.labelSeq
  else: discard
  # `numX` == `numY` since `identity` maps `X -> Y`
  result.numY = result.numX

func callHistogram(geom: Geom, data: seq[float], range: ginger.Scale): (seq[int], seq[float], seq[float]) =
  ## calls the `histogram` proc taking into account the `geom` fields for
  ## - numBins
  ## - binWidth
  ## - binEdges
  ## and chooses the correct field for the calculation
  doAssert geom.statKind == stBin, "Can only bin `stBin` geoms!"
  var
    hist: seq[int]
    binEdges: seq[float]
    binWidths: seq[float]
  if geom.binEdges.isSome:
    (hist, binEdges) = histogram(data, bins = geom.binEdges.get, range = (range.low, range.high))
  elif geom.binWidth.isSome:
    let bins = ((range.high - range.low) / geom.binWidth.get).round.int
    (hist, binEdges) = histogram(data, bins = bins, range = (range.low, range.high))
  else:
    (hist, binEdges) = histogram(data, bins = geom.numBins, range = (range.low, range.high))
  for i in 0 ..< binEdges.high:
    binWidths.add binEdges[i+1] - binEdges[i]
  result = (hist, binEdges, binWidths)

proc filledBinGeom(df: var DataFrame, g: Geom, filledScales: FilledScales): FilledGeom =
  const countCol = "count" # do not hardcode!
  const widthCol = "binWidths"
  let (x, _, discretes, cont) = df.separateScalesApplyTrafos(g.gid,
                                                             filledScales,
                                                             yIsNone = true)
  if x.dcKind == dcDiscrete:
    raise newException(ValueError, "For discrete data columns use `geom_bar` instead!")
  let contCols = cont.mapIt(it.col)
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  result = FilledGeom(geom: g,
                      xcol: $x.col,
                      ycol: countCol,
                      dcKindX: x.dcKind,
                      dcKindY: dcContinuous)
  # for histogram data we don't take into account the raw data, because
  # due to custom bin breaks there may be more data than we want to plot
  result.xScale = encompassingDataScale(cont, akX)
  # y scale is not defined yet, only use continuous scales too
  result.yScale = encompassingDataScale(cont, akY)
  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols.mapIt($it))
    # sumHist used to calculate height of stacked histogram
    var sumHist: seq[int]
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      # before we assign calculate histogram
      let (hist, bins, binWidths) = g.callHistogram(x.col.evaluate(subDf).vToSeq.mapIt(it.toFloat),
                                                    range = x.dataScale)
      sumHist.addBinCountsByPosition(hist, g.position)
      let yieldDf = seqsToDf({ $x.col : bins,
                               countCol: hist })
      let styleLabel = StyleLabel(style: style, label: toObject(keys))
      result.yieldData[styleLabel] = applyContScaleIfAny(yieldDf, df, cont, style)
      result.numX = max(result.numX, yieldDf.len)
      result.xScale = mergeScales(result.xScale, (low: bins.min.float,
                                                  high: bins.max.float))
      result.yScale = mergeScales(result.yScale, (low: 0.0,
                                                  high: sumHist.max.float))
  else:
    let (hist, bins, binWidths) = g.callHistogram(x.col.evaluate(df).vToSeq.mapIt(it.toFloat),
                                                  range = x.dataScale)
    let yieldDf = seqsToDf({ $x.col : bins,
                             countCol: hist,
                             widthCol: binWidths})
    let styleLabel = StyleLabel(style: style, label: Value(kind: Vnull))
    result.yieldData[styleLabel] = applyContScaleIfAny(yieldDf, df, cont, style)
    result.numX = yieldDf.len
    result.xScale = mergeScales(result.xScale, (low: bins.min.float, high: bins.max.float))
    result.yScale = mergeScales(result.yScale, (low: 0.0, high: hist.max.float))
  # `numY` for `bin` stat is just max of the y scale. Since `histogram` counts the
  # number of values in a binned continuous scale the maximum value is always an `int`!
  result.numY = result.yScale.high.round.int
  # set the label sequence manually, since we don't use `setXAttributes`
  case x.dcKind
  of dcDiscrete: result.xLabelSeq = x.labelSeq
  else: discard

proc filledCountGeom(df: var DataFrame, g: Geom, filledScales: FilledScales): FilledGeom =
  const countCol = "count" # do not hardcode!
  let (x, _, discretes, cont) = df.separateScalesApplyTrafos(g.gid,
                                                             filledScales,
                                                             yIsNone = true)
  if x.dcKind == dcContinuous:
    raise newException(ValueError, "For continuous data columns use `geom_histogram` instead!")
  let contCols = cont.mapIt(it.col)
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  result = FilledGeom(geom: g,
                      xcol: $x.col,
                      ycol: countCol,
                      dcKindX: x.dcKind,
                      dcKindY: dcContinuous)
  result.xScale = determineDataScale(x, cont, df)
  # y scale is not yet defined, only use encompassing cont. scales
  result.yScale = encompassingDataScale(cont, akY)
  let allClasses = df[$x.col].unique
  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols.mapIt($it))
    # sumCounts used to calculate height of stacked histogram
    # TODO: can be simplified by implementing `count` of `grouped` DFs!
    var sumCounts = DataFrame()
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      var yieldDf = subDf.count($x.col, name = countCol)
      # all values, which are zero still have to be accounted for! Add those keys with
      # zero values
      yieldDf.addZeroKeys(allClasses, $x.col, countCol)
      # now arrange by `x.col` to force correct order
      yieldDf = yieldDf.arrange($x.col)
      sumCounts.addCountsByPosition(yieldDf, countCol, g.position)
      let styleLabel = StyleLabel(style: style, label: toObject(keys))
      result.yieldData[styleLabel] = applyContScaleIfAny(yieldDf, df, cont, style)
      result.setXAttributes(yieldDf, x)
      result.yScale = mergeScales(result.yScale,
                                  (low: 0.0,
                                   high: max(sumCounts[countCol]).toFloat))
  else:
    let yieldDf = df.count($x.col, name = countCol)
    let styleLabel = StyleLabel(style: style, label: Value(kind: Vnull))
    result.yieldData[styleLabel] = applyContScaleIfAny(yieldDf, df, cont, style)
    result.setXAttributes(yieldDf, x)
    result.yScale = mergeScales(result.yScale,
                                (low: 0.0, high: yieldDf[countCol].max.toFloat))

  # `numY` for `count` stat is just max of the y scale. Since this uses `count` the
  # maximum value is always an `int`!
  result.numY = result.yScale.high.round.int
  doAssert result.numX == allClasses.len

proc postProcessScales*(filledScales: var FilledScales, p: GgPlot) =
  ## walk all geoms and create the dataframes required to draw the
  ## geoms
  var xScale: ginger.Scale
  var yScale: ginger.Scale
  for g in p.geoms:
    var df = if g.data.isSome: g.data.get else: p.data
    var filledGeom: FilledGeom
    case g.kind
    of gkPoint, gkLine, gkErrorBar, gkTile, gkText:
      # can be handled the same
      # need x and y data for sure
      case g.statKind
      of stIdentity:
        filledGeom = filledIdentityGeom(df, g, filledScales)
      of stCount:
        filledGeom = filledCountGeom(df, g, filledScales)
      else:
        filledGeom = filledBinGeom(df, g, filledScales)
    of gkHistogram, gkFreqPoly:
      case g.statKind
      of stIdentity:
        # essentially take same data as for point
        filledGeom = filledIdentityGeom(df, g, filledScales)
        # still a geom, make sure bottom is still at 0!
        filledGeom.yScale = (low: 0.0, high: filledGeom.yScale.high)
      of stBin:
        # calculate histogram
        filledGeom = filledBinGeom(df, g, filledScales)
      of stCount:
        raise newException(Exception, "For discrete counts of your data use " &
          "`geom_bar` instead!")
    of gkBar:
      case g.statKind
      of stIdentity:
        # essentially take same data as for point
        filledGeom = filledIdentityGeom(df, g, filledScales)
      of stCount:
        # count values in classes
        filledGeom = filledCountGeom(df, g, filledScales)
      of stBin:
        raise newException(Exception, "For continuous binning of your data use " &
          "`geom_histogram` instead!")

    if not xScale.isEmpty:
      xScale = mergeScales(xScale, filledGeom.xScale)
      yScale = mergeScales(yScale, filledGeom.yScale)
    else:
      xScale = filledGeom.xScale
      yScale = filledGeom.yScale
    filledScales.geoms.add filledGeom
  let (finalXScale, _, _) = calcTickLocations(xScale, p.numXTicks)
  let (finalYScale, _, _) = calcTickLocations(yScale, p.numYTicks)

  filledScales.xScale = finalXScale
  filledScales.yScale = finalYScale
  # With the final scales in place, update all geoms to know about it
  for g in mitems(filledScales.geoms):
    g.xScale = finalXScale
    g.yScale = finalyScale
