import sequtils, algorithm, tables

import ggplot_types, ggplot_styles, ggplot_utils, ggplot_scales
when defined(defaultBackend):
  import dataframe/fallback/formula
else:
  import dataframe/arraymancer_backend
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
  addIfAny(result[2], getScale(filledScales.text)) # not required
  addIfAny(result[2], getScale(filledScales.yRidges))
  addIfAny(result[2], getScale(filledScales.width))
  # finally add facets
  result[2].add filledScales.facets

func isEmpty(s: ginger.Scale): bool =
  ## checks if the given scale is empty
  result = s.low == s.high

func mergeScales(s1, s2: ginger.Scale): ginger.Scale =
  ## merges the two data scales and returns a version encompassing both
  ## TODO: think about how we might allow (0.0, 0.0) for cases where the
  ## input data has N elements all 0. This is useful for e.g.
  ## having 0 data, but assigning only a yMin = -1. That results in a
  ## useable (-1, 0) data scale! Currently would not work!
  if not (s1.isEmpty and s1.low == 0):
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
      when defined(defaultBackend):
        let col = evaluate(s.col)
        let fn = f{col ~ s.trans( col ) }
        fns.add fn
      else:
        # create a closureScope to capture the value of `s` and `col` instead
        # of the reference
        closureScope:
          let col = evaluate(s.col)
          let colStr = getColName(s)
          # make a copy of `s` which we hand to the closure
          let ms = s
          let fn = f{float: colStr ~ ms.trans( df[col][idx] ) }
          fns.add fn
    else:
      # `s.col` may be pointing to scale which sets constant value or involves
      # a calculation of a column
      fns.add s.col
  # apply transformations using inplace mutate
  ## NOTE: mutating th DF in place means that we ``will`` end up with problems,
  ## if a user sets one aesthetic to some tranformation of a column, assigns
  ## it to the same column name again and tries to plot multiple geoms, which
  ## all access this same aesthetic, since the transformation will be applied
  ## several times! A solution would be to clone the DF when assigning
  ## `var df = if ...` in `postProcessScales`. However, this has a significant
  ## performance cost for plots with many geoms (e.g. `benchmarks/bench_many_geoms.nim`
  ## incurs a performance regression of 50% if cloned).
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
                         scales: seq[Scale]): (seq[FormulaNode], seq[string]) =
  ## splits the given discrete (!) columns by whether they set data (i.e.
  ## arg not a DF column) or map data (arg is column)
  var setDiscCols = newSeq[FormulaNode]()
  var mapDiscCols = newSeq[string]()
  for d in scales:
    # for discrete scales, build the continuous (if any) scales
    if d.col.isColumn(df):
      mapDiscCols.add getColName(d)
    else:
      # setting columns are returned as is, since they don't refer to a column, but a
      # value, which needs to be used
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
                         scales: seq[Scale], baseStyle: GgStyle,
                         toClone: static bool = false):
                           (GgStyle, seq[GgStyle], DataFrame) =
  ## given continuous `scales` (if any) return the correct scales based
  ## on each of these scales
  ## `toClone` should be `true` for all cases where at least on discrete scale is
  ## involved, becasue in those cases `yieldData` contains more than one element.
  ## If we don't clone in that case, all DataFrames in `yieldData` point to the
  ## last processed DF!
  ## NOTE: This modifies `yieldDf` adding all continuous scale columns to it
  result[0] = baseStyle
  when defined(defaultBackend) or not toClone:
    result[2] = yieldDf
  else:
    result[2] = clone(yieldDf)
  for c in scales:
    ## TODO: verify this should be `yieldDf`
    result[2][getColName(c)] = c.col.evaluate(result[2])
    case c.scKind
    of scLinearData, scTransformedData:
      # for linear and transformed data we don't change the style
      discard
    else:
      for el in c.mapData(result[2]):
        result[1].add baseStyle.changeStyle(el)
  if result[1].len == 0:
    result = (baseStyle, @[baseStyle], result[2])


when defined(defaultBackend):
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
else:
  proc addCountsByPosition(sumCounts: var DataFrame, df: DataFrame,
                           col: string, pos: PositionKind) =
    ## adds the `df` column `col` elements to the `sumCounts` data frame in the
    ## same column taking into account the geom position kind.
    case pos
    of pkStack:
      if sumCounts.len == 0:
        when defined(defaultBackend):
          sumCounts = df
        else:
          sumCounts = clone(df)
      else:
        for i in 0 ..< df.len:
          sumCounts[col, i] = sumCounts[col][i, int] + df[col][i, int]
    of pkIdentity, pkDodge:
      sumCounts = df
    of pkFill: sumCounts[col] = toColumn(@[1]) # max for fill always 1.0


proc addBinCountsByPosition(sumHist: var seq[float], hist: seq[float],
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
  of pkFill: sumHist = @[1.0] # max for fill always 1.0

when defined(defaultBackend):
  proc addZeroKeys(df: var DataFrame, keys: seq[Value], xCol, countCol: string) =
    ## Adds the `keys` columns which have zero count values to the `df`.
    ## This is needed for `count` stats, since the `groups` iterator does not
    ## yield empty subgroups, yet we need those for the plot.
    let existKeys = df[xCol].unique
    let zeroKeys = keys.filterIt(it notin existKeys)
    let zeroVals = zeroKeys.mapIt(0)
    df.add seqsToDf({ xCol: zeroKeys, countCol: zeroVals })
else:
  proc addZeroKeys(df: var DataFrame, keys: Column, xCol, countCol: string) =
    ## Adds the `keys` columns which have zero count values to the `df`.
    ## This is needed for `count` stats, since the `groups` iterator does not
    ## yield empty subgroups, yet we need those for the plot.
    let existKeys = df[xCol].unique
    var dfZero = colsToDf(keys).filter(f{string: `keys` notin existKeys})
    dfZero.transmuteInplace(f{int: countCol ~ 0},
                            f{xCol <- "keys"})
    df.add dfZero

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
  ## NOTE: by this time we should (if the code before is correct) certainly
  ## have the `s.col` column in `df`, since `applyTransformations` should have
  ## created it before!
  assert s.col.isColumn(df)
  case s.dcKind
  of dcContinuous:
    result = if s.dataScale.isEmpty: # happens for input DFs with 1-2 elements
              (low: colMin(df, getColName(s)), high: colMax(df, getColName(s)))
             else:
               s.dataScale
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
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)

  result = FilledGeom(geom: g,
                      xcol: getColName(x),
                      ycol: getColName(y),
                      dcKindX: x.dcKind,
                      dcKindY: y.dcKind,
                      geomKind: g.kind)
  result.xScale = determineDataScale(x, cont, df)
  result.yScale = determineDataScale(y, cont, df)
  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      let yieldDf = subDf
      result.setXAttributes(yieldDf, x)
      result.yieldData[toObject(keys)] = applyContScaleIfAny(yieldDf, cont, style,
                                                             toClone = true)
  else:
    let yieldDf = df
    result.setXAttributes(yieldDf, x)
    let key = ("", Value(kind: VNull))
    result.yieldData[toObject(key)] = applyContScaleIfAny(yieldDf, cont, style)

  case y.dcKind
  of dcDiscrete: result.yLabelSeq = y.labelSeq
  else: discard
  # `numX` == `numY` since `identity` maps `X -> Y`
  result.numY = result.numX

proc callHistogram(geom: Geom,
                   df: DataFrame,
                   scale: Scale,
                   weightScale: Option[Scale],
                   range: ginger.Scale): (seq[float], seq[float], seq[float]) =
  ## calls the `histogram` proc taking into account the `geom` fields for
  ## - numBins
  ## - binWidth
  ## - binEdges
  ## and chooses the correct field for the calculation
  doAssert geom.statKind == stBin, "Can only bin `stBin` geoms!"
  template readTmpl(sc: untyped): untyped =
    when defined(defaultBackend):
      sc.col.evaluate(df).vToSeq.mapIt(it.toFloat)
    else:
      ## TODO: investigate! why is `data` here only a view of the full tensor?
      ## Shouldn't `filter` + `groups` iterator return a new tensor?
      sc.col.evaluate(df).toTensor(float).clone.toRawSeq
  let data = readTmpl(scale)
  var
    hist: seq[float]
    binEdges: seq[float]
    binWidths: seq[float]
  template call(binsArg: untyped): untyped =
    let range = if geom.binBy == bbFull: (range.low, range.high) else: (0.0, 0.0)
    let weightData = if weightScale.isSome: readTmpl(weightScale.unsafeGet)
                     else: newSeq[float]()
    (hist, binEdges) = histogram(data,
                                 weights = weightData,
                                 bins = binsArg, range = range)
  if geom.binEdges.isSome:
    call(geom.binEdges.get)
  elif geom.binWidth.isSome:
    let bins = ((range.high - range.low) / geom.binWidth.get).round.int
    call(bins)
  else:
    call(geom.numBins)
  for i in 0 ..< binEdges.high:
    binWidths.add binEdges[i+1] - binEdges[i]
  # add one element for `hist` with 0 entries to have hist.len == bin_edges.len
  hist.add 0.0
  result = (hist, binEdges, binWidths)

proc filledBinGeom(df: var DataFrame, g: Geom, filledScales: FilledScales): FilledGeom =
  const countCol = "count" # do not hardcode!
  const widthCol = "binWidths"
  let (x, _, discretes, cont) = df.separateScalesApplyTrafos(g.gid,
                                                             filledScales,
                                                             yIsNone = true)
  if x.dcKind == dcDiscrete:
    raise newException(ValueError, "For discrete data columns use `geom_bar` instead!")
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  result = FilledGeom(geom: g,
                      xcol: getColName(x),
                      ycol: countCol,
                      dcKindX: x.dcKind,
                      dcKindY: dcContinuous,
                      geomKind: g.kind)
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
    df = df.group_by(mapDiscCols)
    # sumHist used to calculate height of stacked histogram. Stored as `float`,
    # since histogram may be float (if weights used)
    var sumHist: seq[float]
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      # before we assign calculate histogram
      let (hist, bins, _) = g.callHistogram(df,
                                            x,
                                            weightScale = filledScales.getWeightScale(g),
                                            range = x.dataScale)
      ## TODO: Find a nicer solution than this. In this way the `countCol` will always
      ## be a `colObject` column on the arraymancer backend!
      sumHist.addBinCountsByPosition(hist, g.position)
      let yieldDf = seqsToDf({ getColName(x) : bins,
                               countCol: hist })
      result.yieldData[toObject(keys)] = applyContScaleIfAny(yieldDf, cont, style,
                                                             toClone = true)
      result.numX = max(result.numX, yieldDf.len)
      result.xScale = mergeScales(result.xScale, (low: bins.min.float,
                                                  high: bins.max.float))
      result.yScale = mergeScales(result.yScale, (low: 0.0,
                                                  high: sumHist.max.float))
  else:
    # before we assign calculate histogram
    let (hist, bins, binWidths) = g.callHistogram(
      df,
      x,
      weightScale = filledScales.getWeightScale(g),
      range = x.dataScale
    )
    let yieldDf = seqsToDf({ getColName(x) : bins,
                             countCol: hist,
                             widthCol: binWidths})
    let key = ("", Value(kind: VNull))
    result.yieldData[toObject(key)] = applyContScaleIfAny(yieldDf, cont, style)
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
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  let xCol = getColName(x)
  result = FilledGeom(geom: g,
                      xcol: xCol,
                      ycol: countCol,
                      dcKindX: x.dcKind,
                      dcKindY: dcContinuous,
                      geomKind: g.kind)
  result.xScale = determineDataScale(x, cont, df)
  # y scale is not yet defined, only use encompassing cont. scales
  result.yScale = encompassingDataScale(cont, akY)
  let allClasses = df[xCol].unique
  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    # sumCounts used to calculate height of stacked histogram
    # TODO: can be simplified by implementing `count` of `grouped` DFs!
    var sumCounts = DataFrame()
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      var yieldDf = subDf.count(xCol, name = countCol)
      # all values, which are zero still have to be accounted for! Add those keys with
      # zero values
      yieldDf.addZeroKeys(allClasses, xCol, countCol)
      # now arrange by `x.col` to force correct order
      yieldDf = yieldDf.arrange(xCol)
      sumCounts.addCountsByPosition(yieldDf, countCol, g.position)
      result.yieldData[toObject(keys)] = applyContScaleIfAny(yieldDf, cont, style,
                                                             toClone = true)
      result.setXAttributes(yieldDf, x)
      when defined(defaultBackend):
        result.yScale = mergeScales(result.yScale,
                                    (low: 0.0,
                                     high: max(sumCounts[countCol]).toFloat))
      else:
        result.yScale = mergeScales(result.yScale,
                                    (low: 0.0,
                                     high: max(sumCounts[countCol].toTensor(int)).float))
  else:
    let yieldDf = df.count(xCol, name = countCol)
    let key = ("", Value(kind: VNull))
    result.yieldData[toObject(key)] = applyContScaleIfAny(yieldDf, cont, style)
    result.setXAttributes(yieldDf, x)
    when defined(defaultBackend):
      result.yScale = mergeScales(result.yScale,
                                  (low: 0.0, high: yieldDf[countCol].max.toFloat))
    else:
      result.yScale = mergeScales(result.yScale,
                                  (low: 0.0,
                                   high: max(yieldDf[countCol].toTensor(int)).float))


  # `numY` for `count` stat is just max of the y scale. Since this uses `count` the
  # maximum value is always an `int`!
  result.numY = result.yScale.high.round.int
  doAssert result.numX == allClasses.len

proc fillOptFields(fg: var FilledGeom, fs: FilledScales) =
  template assignIfAny(fg, scale, arg: untyped): untyped =
    if scale.isSome:
      fg.arg = some(getColName(scale.get))
  # TODO: use fg. gid?
  case fg.geom.kind
  of gkErrorBar:
    fg.assignIfAny(getXMinScale(fs, fg.geom), xMin)
    fg.assignIfAny(getXMaxScale(fs, fg.geom), xMax)
    fg.assignIfAny(getYMinScale(fs, fg.geom), yMin)
    fg.assignIfAny(getYMaxScale(fs, fg.geom), yMax)
  of gkTile:
    fg.assignIfAny(getHeightScale(fs, fg.geom), height)
    fg.assignIfAny(getWidthScale(fs, fg.geom), width)
  of gkText:
    fg.text = $getTextScale(fs, fg.geom).col
  else: discard

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
      filledGeom.fillOptFields(filledScales)
    of gkHistogram, gkFreqPoly:
      case g.statKind
      of stIdentity:
        # essentially take same data as for point
        filledGeom = filledIdentityGeom(df, g, filledScales)
        # still a histogram like geom, make sure bottom is still at 0!
        filledGeom.yScale = (low: min(0.0, filledGeom.yScale.low),
                             high: filledGeom.yScale.high)
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
        # still a geom_bar, make sure bottom is still at 0!
        filledGeom.yScale = (low: min(0.0, filledGeom.yScale.low),
                             high: filledGeom.yScale.high)
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
