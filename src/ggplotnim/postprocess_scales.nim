import std / [sequtils, algorithm, tables, sets]

import ggplot_types, ggplot_styles, ggplot_scales
from ggplot_ticks import getXTicks, getYTicks
import datamancer
import ginger except Scale

from seqmath import histogram, linspace, round
import arraymancer / stats / kde

when not defined(nolapack):
  ## If `nolapack` is defined, smoothing functionality is disabled to not depend on
  ## LAPACK
  from scinim / signals import savitzkyGolayFilter
  from polynumeric import polyFit, eval, initPoly

proc getScales(geom: Geom, filledScales: FilledScales,
               yIsNone = false): (Scale, Scale, seq[Scale]) =
  ## Returns the x and y scales individually and the other scales as a
  ## sequence
  let gid = geom.gid
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
  if xOpt.isSome:
    result[0] = xOpt.get
  if yOpt.isSome:
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
  result[2].add filledScales.facets.columns # get the scales of the Facet

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
    let s = s
      # refs https://github.com/nim-lang/Nim/pull/14447
      # alternative would be to use `sugar.capture(s)` instead of `closureScope`
    if s.scKind == scTransformedData:
      # create a closureScope to capture the value of `s` and `col` instead
      # of the reference
      closureScope:
        let col = evaluate(s.col)
        let colStr = getColName(s)
        # make a copy of `s` which we hand to the closure
        let ms = s
        let fn = f{float: colStr ~ ms.trans( idx(col.toStr) ) }
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

  ## TODO: Here is where our constant columns turn object for the first (but not last) time
  df.mutateInplace(fns)

proc separateScalesApplyTrafos(
  df: var DataFrame, geom: Geom,
  filledScales: FilledScales, yIsNone = false):
    tuple[x: Scale, y: Scale, discretes: seq[Scale], cont: seq[Scale]] =
  # NOTE: if `yIsNone = true` the `y` in the next line will be an empty scale,
  # caller has to be aware of that!
  let yIsNone = yIsNone # or geom.kind in {gkErrorBar} # modify here for simplicity
  let (x, y, scales) = getScales(geom, filledScales, yIsNone = yIsNone)
  # split by discrete and continuous
  let discretes = scales.filterIt(it.dcKind == dcDiscrete)
  let cont = scales.filterIt(it.dcKind == dcContinuous)
  # extract all scales that refer to existing columns in the DF. This is to
  let discrCols = discretes.mapIt(getColName(it)).filterIt(it in df).deduplicate
  # group DF by these discrete columns. This allows to apply transformations
  # based on formulas to individual subgroups of the DF
  if discrCols.len > 0:
    df = df.group_by(discrCols, add = true)
  # apply scale transformations
  let xyScales = @[x, y].filterIt(not it.isNil)
  df.applyTransformations(concat(xyScales, scales))
  result = (x: x, y: y, discretes: discretes, cont: cont)

proc splitDiscreteSetMap(df: DataFrame,
                         scales: seq[Scale]): (seq[FormulaNode], seq[string]) =
  ## splits the given discrete (!) columns by whether they set data (i.e.
  ## arg not a DF column) or map data (arg is column)
  var setDiscCols = newSeq[FormulaNode]()
  var mapDiscCols = newSeq[string]()
  for d in scales:
    # for discrete scales, build the continuous (if any) scales
    if d.col.isColumn(df) and not d.col.isConstant(df):
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
  if not scale.isNil:
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
      case fg.geomKind:
      of gkRaster:
        # For raster this is already set in `fillOptFields` for performance reasons
        discard
      else:
        fg.numX = max(fg.numX, df.len)

proc applyContScaleIfAny(yieldDf: DataFrame,
                         scales: seq[Scale], baseStyle: GgStyle,
                         gkKind: GeomKind,
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
  when not toClone:
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
      if gkKind notin {gkRaster}:
        # avoid expensive compution for raster
        let scVals = c.mapData(result[2])
        result[1] = newSeq[GgStyle](scVals.len)
        for i in 0 ..< scVals.len:
          result[1][i] = baseStyle.changeStyle(scVals[i])
  if result[1].len == 0:
    result = (baseStyle, @[baseStyle], result[2])

proc addCountsByPosition(colSum: var Column, col: Column,
                         pos: PositionKind) =
  ## adds the `hist` sequence elements to the `colSum` sequence taking into
  ## account the geom position kind, only if `col` is actually float, int or
  ## Value
  case col.kind
  of colObject, colInt, colFloat:
    case pos
    of pkStack:
      if colSum.len == 0:
        colSum = col
      else:
        for i in 0 .. colSum.high:
          withNativeDtype(col):
            when dtype is float or dtype is int or dtype is Value:
              colSum[i] = colSum[i, dtype] + col[i, dtype]
            # else we leave it unmodified
    of pkIdentity, pkDodge:
      colSum = col
    of pkFill: colSum = toColumn @[1.0] # max for fill always 1.0
  else:
    colSum = col

proc addZeroKeys(df: var DataFrame, keys: Column, xCol, countCol: string) =
  ## Adds the `keys` columns which have zero count values to the `df`.
  ## This is needed for `count` stats, since the `groups` iterator does not
  ## yield empty subgroups, yet we need those for the plot.
  let existKeys = df[xCol].unique
  var dfZero = colsToDf(keys).filter(f{string: `keys` notin existKeys})
    .rename(f{xCol <- "keys"})
  dfZero[countCol] = 0
  df.add dfZero

proc fillOptFields(fg: var FilledGeom, fs: FilledScales, df: var DataFrame) =
  ## TODO: currently we compute the `numX` and `numY` here. But that is not really required
  ## *for raster!*. For tile it is, because we cannot know the number of tiles (different
  ## widths e.g.)
  ## In theory we could have different widths for raster as well, but we don't support that
  ## yet. So improve computation of numX or leave out for raster?

  template assignIfAny(fg, scale, arg: untyped): untyped =
    if scale.isSome:
      fg.arg = some(getColName(scale.get))
  # TODO: use fg. gid?
  case fg.geomKind
  of gkErrorBar:
    fg.assignIfAny(getXMinScale(fs, fg.geom), xMin)
    fg.assignIfAny(getXMaxScale(fs, fg.geom), xMax)
    fg.assignIfAny(getYMinScale(fs, fg.geom), yMin)
    fg.assignIfAny(getYMaxScale(fs, fg.geom), yMax)
  of gkTile:
    let
      hS = getHeightScale(fs, fg.geom)
      wS = getWidthScale(fs, fg.geom)
      xMinS = getXMinScale(fs, fg.geom)
      xMaxS = getXMaxScale(fs, fg.geom)
      yMinS = getYMinScale(fs, fg.geom)
      yMaxS = getYMaxScale(fs, fg.geom)
    if hS.isSome:
      # use height if available
      fg.height = some(getColName(hS.get))
    elif yMinS.isSome and yMaxS.isSome:
      let minName = getColName(yMinS.get)
      let maxName = getColName(yMaxS.get)
      let yColName = getColName(getYScale(fs, fg.geom))
      df = df.mutate(f{float -> float: "height" ~ idx(maxName) - idx(minName)},
                     f{float -> float: yColName ~ idx(minName)})
      fg.height = some("height")
    elif yMinS.isSome or yMaxS.isSome:
      raise newException(AestheticError, "Invalid combination of aesthetics! If no height " &
        "given both an `yMin` and `yMax` has to be supplied for geom_tile!")
    else:
      echo "INFO: using default height of 1 since no tiling height information supplied. " &
        "Add `height` or (`yMin`, `yMax`) as aesthetics for a different values."
      df["height"] = constantColumn(1.0, df.len)
      fg.height = some("height")
    if wS.isSome:
      # use width if available
      fg.width = some(getColName(wS.get))
    elif xMinS.isSome and xMaxS.isSome:
      let minName = getColName(xMinS.get)
      let maxName = getColName(xMaxS.get)
      let xColName = getColName(getXScale(fs, fg.geom))
      df = df.mutate(f{float -> float: "width" ~ idx(maxName) - idx(minName)},
                     f{float -> float: xColName ~ idx(minName)})
      fg.width = some("width")
    elif xMinS.isSome or xMaxS.isSome:
      raise newException(AestheticError, "Invalid combination of aesthetics! If no width " &
        "given both an `xMin` and `xMax` has to be supplied for geom_tile!")
    else:
      echo "INFO: using default width of 1 since no tiling width information supplied. " &
        "Add `width` or (`xMin`, `xMax`) as aesthetics for a different values."
      df["width"] = constantColumn(1.0, df.len)
      fg.width = some("width")
    let fillScale = getFillScale(fs)
    if fillScale.isNone:
      raise newException(AestheticError, "`geom_raster` requires a `fill` aesthetic scale!")
    let fs = fillScale.get
    fg.fillCol = getColName(fs)
    if fillScale.get.dcKind == dcContinuous:
      fg.fillDataScale = fs.dataScale
      fg.colorScale = useOrDefault(fs.colorScale)
  of gkRaster:
    let
      hS = getHeightScale(fs, fg.geom)
      wS = getWidthScale(fs, fg.geom)
      xS = getXScale(fs, fg.geom)
      yS = getYScale(fs, fg.geom)
      xMinS = getXMinScale(fs, fg.geom)
      xMaxS = getXMaxScale(fs, fg.geom)
      yMinS = getYMinScale(fs, fg.geom)
      yMaxS = getYMaxScale(fs, fg.geom)
    if hS.isSome:
      # use height if available
      fg.height = some(getColName(hS.get))
    elif yMinS.isSome and yMaxS.isSome:
      let minName = getColName(yMinS.get)
      let maxName = getColName(yMaxS.get)
      let yColName = getColName(getYScale(fs, fg.geom))
      df = df.mutate(f{float -> float: "height" ~ idx(maxName) - idx(minName)},
                     f{float -> float: yColName ~ idx(minName)})
      fg.height = some("height")
    elif yMinS.isSome or yMaxS.isSome:
      raise newException(AestheticError, "Invalid combination of aesthetics! If no height " &
        "given both an `yMin` and `yMax` has to be supplied for geom_raster!")
    else:
      echo "INFO: using default height of 1 since no raster height information supplied. " &
        "Add `height` or (`yMin`, `yMax`) as aesthetics for a different values."
      let yCol = df[getYScale(fs, fg.geom).getColName].unique
      fg.numY = yCol.len
      df["height"] = constantColumn(abs((yCol[1, float] - yCol[0, float])), df.len)
      fg.height = some("height")
    if wS.isSome:
      # use width if available
      fg.width = some(getColName(wS.get))
    elif xMinS.isSome and xMaxS.isSome:
      let minName = getColName(xMinS.get)
      let maxName = getColName(xMaxS.get)
      let xColName = getColName(xS)
      df = df.mutate(f{float -> float: "width" ~ idx(maxName) - idx(minName)},
                     f{float -> float: xColName ~ idx(minName)})
      fg.width = some("width")

    elif xMinS.isSome or xMaxS.isSome:
      raise newException(AestheticError, "Invalid combination of aesthetics! If no width " &
        "given both an `xMin` and `xMax` has to be supplied for geom_raster!")
    else:
      echo "INFO: using default width of 1 since no raster width information supplied. " &
        "Add `width` or (`xMin`, `xMax`) as aesthetics for a different values."
      let xCol = df[xS.getColName].unique
      fg.numX = xCol.len
      df["width"] = constantColumn(abs((xCol[1, float] - xCol[0, float])), df.len)
      fg.width = some("width")
    let fillScale = getFillScale(fs)
    if fillScale.isNone:
      raise newException(AestheticError, "`geom_raster` requires a `fill` aesthetic scale!")
    let fs = fillScale.get
    fg.fillCol = getColName(fs)
    if fillScale.get.dcKind == dcContinuous:
      fg.fillDataScale = fs.dataScale
      fg.colorScale = useOrDefault(fs.colorScale)
    ## Assign potential transformation
    fg.trans = fs.transC
    fg.invTrans = fs.invTransC
    ## Assign x / y scale of *raster* data
    block RasterScales:
      let xCol = df[xS.getColName]
      if xCol.kind in {colFloat, colInt}:
        fg.rasterXScale = (low: xCol.toTensor(float).min, high: xCol.toTensor(float).max)
      else:
        raise newException(ValueError, "The `x` data aesthetics for the raster plot is neither " &
          "`float` or `int` data. Instead it is: " & $xCol.kind)
      let yCol = df[yS.getColName]
      if yCol.kind in {colFloat, colInt}:
        fg.rasterYScale = (low: yCol.toTensor(float).min, high: yCol.toTensor(float).max)
      else:
        raise newException(ValueError, "The `y` data aesthetics for the raster plot is neither " &
          "`float` or `int` data. Instead it is: " & $yCol.kind)

  of gkText:
    fg.text = $getTextScale(fs, fg.geom).col
  of gkHistogram:
    fg.hdKind = fg.geom.hdKind
  else: discard

proc determineDataScale(s: Scale,
                        additional: seq[Scale], df: DataFrame): ginger.Scale =
  ## returns the data scale given a filled `Scale s` and the corresponding data,
  ## while differentiating between continuous and discrete scales
  ## NOTE: by this time we should (if the code before is correct) certainly
  ## have the `s.col` column in `df`, since `applyTransformations` should have
  ## created it before!
  if s.isNil: return
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

proc maybeFilterUnique(df: var DataFrame, fg: FilledGeom, x, y: string) =
  ## If `fg` is of kind `gkErrorBar` we may filter the data frame to
  ## only unique values in the x/y x/yMin/Max columns
  if fg.geomKind == gkErrorBar:
    var collectCols = newSeq[string]()
    var hasX = false
    var hasY = false
    template addIt(field, isX: untyped): untyped =
      if fg.field.isSome:
        collectCols.add fg.field.get
        if isX: hasX = true
        else: hasY = true
    addIt(xMin, true)
    addIt(xMax, true)
    addIt(yMin, false)
    addIt(yMax, false)
    if hasX: collectCols.add fg.yCol # add y if we have ranges in x
    if hasY: collectCols.add fg.xCol # add x if we have ranges in y
    when false:
      ## It would be ideal if we could trust that constant columns actually remain
      ## constant. But in the processing before this procedure, there are too many
      ## changes that possibly make it an object column. Thus better we just apply
      ## unique regardless
      if collectCols.allIt(df[it].kind == colConstant):
        # all associated columns are constant,
        df = df.unique(collectCols)
    df = df.unique(collectCols)
  else:
    assert x in df and y in df
    if df[x].kind == colConstant and df[y].kind == colConstant:
      df = df.unique([x, y])

proc reversed[A, B](t: OrderedTable[A, B]): OrderedTable[A, B] =
  result = initOrderedTable[A, B]()
  let keys = toSeq(t.keys)
  for k in keys.reversed:
    result[k] = t[k]

proc filledIdentityGeom(df: var DataFrame, g: Geom,
                        filledScales: FilledScales): FilledGeom =
  let (x, y, discretes, cont) = df.separateScalesApplyTrafos(g,
                                                             filledScales)
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)

  result = FilledGeom(geom: g,
                      xCol: getColName(x),
                      yCol: getColName(y),
                      dcKindX: (if filledScales.discreteX: dcDiscrete else: dcContinuous),
                      dcKindY: (if filledScales.discreteY: dcDiscrete else: dcContinuous),
                      geomKind: g.kind)
  result.xScale = determineDataScale(x, cont, df)
  result.yScale = determineDataScale(y, cont, df)
  result.fillOptFields(filledScales, df)

  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    var col = newColumn(colFloat)
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)

      var yieldDf = subDf
      result.setXAttributes(yieldDf, x)
      ## TODO: refactor out `yieldDf` mutations? Can be combined with other `filled*Geom`?
      if g.position == pkStack: # only needed for gkHistogram (hdBars) + gkBar
        yieldDf[PrevValsCol] = if col.len == 0: constantColumn(0.0, yieldDf.len)
                              else: col.clone
      # possibly modify `col` if stacking
      col.addCountsByPosition(subDf[result.yCol], g.position)
      if g.position == pkStack and
         not ((g.kind == gkHistogram and g.hdKind == hdBars) or (g.kind == gkBar)):
        # assign stacked column as new y
        yieldDf[result.yCol] = col

      yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
      result.yieldData[toObject(keys)] = applyContScaleIfAny(yieldDf, cont, style, g.kind,
                                                             toClone = true)
    if g.position == pkStack and result.dcKindY == dcContinuous:
      # only update required if stacking, as we've computed the range beforehand
      result.yScale = mergeScales(result.yScale, (low: result.yScale.low, high: col.toTensor(float).max))
    ## NOTE: reverse in case of:
    ## - gkHistogram
    ## - pkStack
    ## - hdOutline
    ## In this case we have to draw the largest one *first* so that the smaller ones
    ## are drown *on top of* the largest
    if g.kind == gkHistogram and g.position == pkStack and g.hdKind == hdOutline:
      result.yieldData = result.yieldData.reversed
  else:
    var yieldDf = df
    yieldDf[PrevValsCol] = constantColumn(0.0, yieldDf.len)
    yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
    result.setXAttributes(yieldDf, x)
    let key = ("", Value(kind: VNull))
    result.yieldData[toObject(key)] = applyContScaleIfAny(yieldDf, cont, style, g.kind)

  if not y.isNil:
    case y.dcKind
    of dcDiscrete: result.yLabelSeq = y.labelSeq
    else: discard
  # `numX` == `numY` since `identity` maps `X -> Y`
  result.numY = result.numX

proc callSmoother(fg: FilledGeom,
                  df: DataFrame,
                  scale: Scale,
                  range: ginger.Scale): Tensor[float] =
  ## Calls the correct smoother for the `SmoothMethodKind` of `geom`.
  ## Either a Savitzky-Golay filter or a Levenberg-Marquardt fit.
  # compute the window size in points based on the `span` and scale
  let geom = fg.geom
  doAssert geom.statKind == stSmooth, "Needs to be stat 'smooth' to perform smoothing."
  let data = df[getColName(scale), float]
  case geom.methodKind
  of smSVG:
    var windowSize = (df.len.float * geom.span).round.int
    if windowSize mod 2 == 0:
      inc windowSize
    if windowSize < 1 or windowSize > df.len:
      raise newException(ValueError, "The given `span` value of " & $geom.span & " results in a " &
        "Savitzky-Golay filter window of " & $windowSize & " for input data with length " & $df.len & ".")
    when defined(nolapack):
      raise newException(Exception, "You compiled the binary with `-d:nolapack`. `geom_smooth` is not available!")
    else:
      result = savitzkyGolayFilter(data,
                                   windowSize, geom.polyOrder)
  of smPoly:
    # for a polynomial least squares fit we also need the x values
    let xData = df[fg.xCol, float]
    when  defined(nolapack):
      raise newException(Exception, "You compiled the binary with `-d:nolapack`. `geom_smooth` is not available!")
    else:
      let polyCoeff = polyFit(xData, data, geom.polyOrder)
      # `initPoly` takes the coefficients in reversed order (highest order first)
      let p = initPoly(polyCoeff.toSeq1D.reversed)
      result = xData.map_inline(p.eval(x))
  of smLM:
    raise newException(Exception, "Levenberg-Marquardt fitting is not implemented yet.")

func getExceptionMessageDiscrete(ax, col: string): string =
  result = "The input data for the `" & ax & "` scale - column `" & col &
    "` - is discrete. This is incompatible with smoothing statistics (`geom_smooth`, ...)" &
    ". Add a `+ scale_" & ax & "_continuous()` call to the plotting chain to force the" &
    " data to be treated as continuous if the data is fine as is."

proc filledSmoothGeom(df: var DataFrame, g: Geom,
                      filledScales: FilledScales): FilledGeom =
  let (x, y, discretes, cont) = df.separateScalesApplyTrafos(g,
                                                             filledScales)
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)

  if x.dcKind == dcDiscrete: raise newException(ValueError, getExceptionMessageDiscrete("x", $x.col))
  if y.dcKind == dcDiscrete: raise newException(ValueError, getExceptionMessageDiscrete("y", $y.col))

  result = FilledGeom(geom: g,
                      xCol: getColName(x),
                      yCol: SmoothValsCol,
                      dcKindX: dcContinuous, # smoothed data ``must`` be continuous!
                      dcKindY: dcContinuous,
                      geomKind: g.kind)
  result.xScale = determineDataScale(x, cont, df)
  result.yScale = determineDataScale(y, cont, df)
  result.fillOptFields(filledScales, df)

  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    var col = newColumn(colFloat)
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      var yieldDf = subDf
      let smoothed = result.callSmoother(yieldDf,
                                         y, # need the column which contains the data to be smoothed
                                         range = x.dataScale)
      yieldDf[SmoothValsCol] = smoothed
      result.setXAttributes(yieldDf, x)
      ## TODO: refactor out `yieldDf` mutations? Can be combined with other `filled*Geom`?
      if g.position == pkStack: # only needed for gkHistogram (hdBars) + gkBar
        yieldDf[PrevValsCol] = if col.len == 0: constantColumn(0.0, yieldDf.len)
                              else: col.clone
      # possibly modify `col` if stacking
      col.addCountsByPosition(yieldDf[result.yCol], g.position)
      if g.position == pkStack and
         not ((g.kind == gkHistogram and g.hdKind == hdBars) or (g.kind == gkBar)):
        # assign stacked column as new y
        yieldDf[result.yCol] = col

      yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
      result.yieldData[toObject(keys)] = applyContScaleIfAny(yieldDf, cont, style, g.kind,
                                                             toClone = true)
    if g.position == pkStack and result.dcKindY == dcContinuous:
      # only update required if stacking, as we've computed the range beforehand
      result.yScale = mergeScales(result.yScale, (low: result.yScale.low, high: col.toTensor(float).max))
    ## NOTE: reverse in case of:
    ## - gkHistogram
    ## - pkStack
    ## - hdOutline
    ## In this case we have to draw the largest one *first* so that the smaller ones
    ## are drown *on top of* the largest
    if g.kind == gkHistogram and g.position == pkStack and g.hdKind == hdOutline:
      result.yieldData = result.yieldData.reversed
  else:
    var yieldDf = df.shallowCopy()
    let smoothed = result.callSmoother(yieldDf,
                                       y, # need the column which contains the data to be smoothed
                                       range = x.dataScale)
    yieldDf[PrevValsCol] = constantColumn(0.0, yieldDf.len)
    yieldDf[SmoothValsCol] = smoothed
    yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
    result.setXAttributes(yieldDf, x)
    let key = ("", Value(kind: VNull))
    result.yieldData[toObject(key)] = applyContScaleIfAny(yieldDf, cont, style, g.kind)

  # `numX` == `numY` since `identity` maps `X -> Y`
  result.numY = result.numX

proc callHistogram(geom: Geom,
                   df: DataFrame,
                   scale: Scale,
                   weightScale: Option[Scale],
                   range: ginger.Scale): (Tensor[float], Tensor[float], Tensor[float]) =
  ## calls the `histogram` proc taking into account the `geom` fields for
  ## - numBins
  ## - binWidth
  ## - binEdges
  ## and chooses the correct field for the calculation
  doAssert geom.statKind == stBin, "Can only bin `stBin` geoms!"
  template readTmpl(sc: untyped): untyped =
    ## TODO: investigate! why is `data` here only a view of the full tensor?
    ## Shouldn't `filter` + `groups` iterator return a new tensor?
    sc.col.evaluate(df).toTensor(float).clone.toSeq1D
  let data = readTmpl(scale)
  var
    hist: seq[float]
    binEdges: seq[float]
    binWidths: seq[float]
  template call(binsArg: untyped): untyped =
    let range = if geom.binBy == bbFull: (range.low, range.high) else: (0.0, 0.0)
    let weightData = if weightScale.isSome: readTmpl(weightScale.unsafeGet)
                     else: newSeq[float]()
    template inner(dens: untyped): untyped =
      # need static arg to `density`, thus this inner template
      (hist, binEdges) = histogram(data,
                                   weights = weightData,
                                   bins = binsArg, range = range,
                                   density = dens)
    if geom.density:
      inner(true)
    else:
      inner(false)
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
  result = (hist.toTensor, binEdges.toTensor, binWidths.toTensor)

proc callKde(geom: Geom,
             df: DataFrame,
             scale: Scale,
             weightScale: Option[Scale],
             range: ginger.Scale): (Tensor[float], Tensor[float]) =
  ## Calls the `kde` with the appropriate arguments based on the given settings in the `geom`
  ## from the `geom_density` call.
  ##
  ## Returns the KDE'd data and the corresponding sample points.
  doAssert geom.statKind == stDensity, "Needs to be stat 'density' to perform a KDE."
  let data = df[getColName(scale), float]
  var weight = newTensor[float](0)
  if weightScale.isSome:
    weight = df[getColName(weightScale.get), float]
  var sampleSeq = if geom.sampleSeq.len > 0: geom.sampleSeq
                  elif geom.range != (NegInf, Inf):
                    linspace(geom.range.l, geom.range.h, geom.samples)
                  else:
                    linspace(data.min, data.max, geom.samples)
  let kdeData = kde(data,
                    kernel = geom.kernel,
                    samples = sampleSeq.toTensor,
                    bw = geom.bandwidth,
                    adjust = geom.adjust,
                    normalize = geom.normalize,
                    weights = weight)
  result = (kdeData, sampleSeq.toTensor)

proc filledBinDensityGeom(df: var DataFrame, g: Geom, filledScales: FilledScales): FilledGeom =
  ## Handles binning via `stBin` and density via `stDensity`. Initially only for histograms.
  let countCol = if g.statKind == stDensity or (g.statKind == stBin and g.density): "density" else: CountCol # do not hardcode!
  const widthCol = "binWidths"
  let (x, _, discretes, cont) = df.separateScalesApplyTrafos(g,
                                                             filledScales,
                                                             yIsNone = true)
  if x.dcKind == dcDiscrete:
    raise newException(ValueError, "For discrete data columns use `geom_bar` instead!")

  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  result = FilledGeom(geom: g,
                      xCol: getColName(x),
                      yCol: countCol,
                      #dcKindX: x.dcKind,
                      dcKindX: (if filledScales.discreteX: dcDiscrete else: dcContinuous),
                      dcKindY: dcContinuous,
                      geomKind: g.kind)
  # for histogram data we don't take into account the raw data, because
  # due to custom bin breaks there may be more data than we want to plot
  result.xScale = encompassingDataScale(cont, akX)
  # y scale is not defined yet, only use continuous scales too
  result.yScale = encompassingDataScale(cont, akY)

  result.fillOptFields(filledScales, df)
  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))

  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    # `col` used to calculate height of stacked histogram. Stored as `float`,
    # since histogram may be float (if weights used)
    var col = newColumn(colFloat)
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      # before we assign calculate histogram
      var
        hist: Tensor[float]
        bins: Tensor[float]
        binWidth: Tensor[float]
      case g.statKind
      of stBin:
        (hist, bins, binWidth) = g.callHistogram(
          subDf,
          x,
          weightScale = filledScales.getWeightScale(g),
          range = x.dataScale
        )
      of stDensity:
        (hist, bins) = g.callKde(subDf,
                                 x,
                                 weightScale = filledScales.getWeightScale(g),
                                 range = x.dataScale)
      else:
        raise newException(ValueError, "Unexpected stat kind for histograms and density calculations: " & $g.statKind)

      ## TODO: Find a nicer solution than this. In this way the `countCol` will always
      ## be a `colObject` column on the arraymancer backend!
      var yieldDf = toDf({ getColName(x) : bins,
                           countCol: hist })
      if g.position == pkStack: # only needed for gkHistogram (hdBars) + gkBar
        yieldDf[PrevValsCol] = if col.len == 0: constantColumn(0.0, yieldDf.len)
                              else: col.clone
      # possibly modify column by stacking, else use `hist`
      col.addCountsByPosition(toColumn hist, g.position)
      if g.position == pkStack:
        if not ((g.kind == gkHistogram and g.hdKind == hdBars) or (g.kind == gkBar)):
          # use sum to reassign y column
          yieldDf[result.yCol] = col
      yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
      result.yieldData[toObject(keys)] = applyContScaleIfAny(yieldDf, cont, style,
                                                             g.kind,
                                                             toClone = true)
      result.numX = max(result.numX, yieldDf.len)
      # needs to be done for each
      if g.kind == gkFreqPoly:
        let binWidth = if bins.len > 1: (bins[1] - bins[0]).float else: 0.0
        result.xScale = mergeScales(result.xScale,
                                    (low: bins.min.float - binWidth / 2.0,
                                     high: bins.max.float + binWidth / 2.0))
      else: # gkHistogram & gkLine (geom_density)
        result.xScale = mergeScales(result.xScale,
                                    (low: bins.min.float, high: bins.max.float))

      result.yScale = mergeScales(result.yScale,
                                  (low: 0.0, high: col.toTensor(float).max))
  else:
    # before we assign calculate histogram
    var
      hist: Tensor[float]
      bins: Tensor[float]
      binWidths: Tensor[float]
    case g.statKind
    of stBin:
      (hist, bins, binWidths) = g.callHistogram(
        df,
        x,
        weightScale = filledScales.getWeightScale(g),
        range = x.dataScale
      )
    of stDensity:
      (hist, bins) = g.callKde(
        df,
        x,
        weightScale = filledScales.getWeightScale(g),
        range = x.dataScale
      )
      binWidths = zeros[float](bins.len)
    else:
      raise newException(ValueError, "Unexpected stat kind for histograms and density calculations: " & $g.statKind)
    var yieldDf = toDf({ getColName(x) : bins,
                         countCol: hist,
                         widthCol: binWidths})
    yieldDf[PrevValsCol] = constantColumn(0.0, yieldDf.len)
    yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
    let key = ("", Value(kind: VNull))
    # TODO: does it make sense to apply continuous scales to a histogram result?
    # because: The input DF does not match (element wise) to the resulting DF of
    # the histogram call. That's a problem, because if one wishes to apply continuous
    # styling based on some column not part of the histogram call, that column won't
    # exist for the histogram DF anymore. There isn't really a way to preserve that
    # information, because it's not well defined how to merge individual rows with
    # continuous styling to a single bin.
    # Chances are we end up here, because the "continuous" scale was misclassified
    doAssert cont.len == 0, "Was " & $cont.mapIt($it.col) & " supposed to be discrete?"
    result.yieldData[toObject(key)] = applyContScaleIfAny(yieldDf, cont, style, g.kind)
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

proc maybeRaise(xCol: string, cont: seq[Scale]) =
  ## We raise from `filledCountGeom` in case we have some continuous classification in
  ## addition to our count column `xCol`. This is because `count` reduces the information
  ## in the data frame and the columns for `cont` won't exist afterwards.
  ##
  ## Ref: issue #100
  if cont.len > 0:
    var errMsg = "Cannot perform continuous action using the following formulae: ["
    for i, c in cont:
      errMsg.add "(Column: " & $c.col & ", kind: " & $c.scKind & ")"
      if i < cont.high:
        errMsg.add ", "
    errMsg.add "] in a `count` statistics on column `" & xCol & "`."
    raise newException(ValueError, errMsg)

proc count(df: DataFrame, xCol, name: string, weights: Option[Scale]): DataFrame =
  ## XXX: This should be moved to datamancer next release!
  if weights.isNone:
    result = df.count(xCol, name = CountCol)
  else:
    let w = weights.get.getColName()
    result = df.group_by(xCol)
      .summarize(f{float: name << sum(col(w))})

proc filledCountGeom(df: var DataFrame, g: Geom, filledScales: FilledScales): FilledGeom =
  let (x, _, discretes, cont) = df.separateScalesApplyTrafos(g,
                                                             filledScales,
                                                             yIsNone = true)
  if x.dcKind == dcContinuous:
    raise newException(ValueError, "For continuous data columns use `geom_histogram` instead!")

  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  let xCol = getColName(x)
  result = FilledGeom(geom: g,
                      xCol: xCol,
                      yCol: CountCol,
                      #dcKindX: x.dcKind,
                      dcKindX: (if filledScales.discreteX: dcDiscrete else: dcContinuous),#x.dcKind,
                      dcKindY: dcContinuous,
                      geomKind: g.kind)
  result.xScale = determineDataScale(x, cont, df)
  # y scale is not yet defined, only use encompassing cont. scales
  result.yScale = encompassingDataScale(cont, akY)
  result.fillOptFields(filledScales, df)

  let allClasses = df[xCol].unique
  # w/ all groupings
  var style: GgStyle
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    # `col` used to calculate height of stacked histogram
    # TODO: can be simplified by implementing `count` of `grouped` DFs!
    var col = newColumn(colInt)
    maybeRaise(xCol, cont)
    for keys, subDf in groups(df, order = SortOrder.Descending):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      var yieldDf = subDf.count(xCol, name = CountCol,
                                weights = filledScales.getWeightScale(g))
      # all values, which are zero still have to be accounted for! Add those keys with
      # zero values
      yieldDf.addZeroKeys(allClasses, xCol, CountCol)
      # now arrange by `x.col` to force correct order
      yieldDf = yieldDf.arrange(xCol)
      if g.position == pkStack: # only needed for gkHistogram (hdBars) + gkBar
        yieldDf[PrevValsCol] = if col.len == 0: constantColumn(0.0, yieldDf.len)
                              else: col.clone
      # possibly modify `col` by stacking `yCol`, else use `yCol`
      col.addCountsByPosition(yieldDf[result.yCol], g.position)
      if g.position == pkStack and
         not ((g.kind == gkHistogram and g.hdKind == hdBars) or (g.kind == gkBar)):
        # use new col to modify `y` column
        yieldDf[result.yCol] = col
      yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
      result.yieldData[toObject(keys)] = applyContScaleIfAny(yieldDf, cont, style,
                                                             g.kind,
                                                             toClone = true)
      result.setXAttributes(yieldDf, x)
      result.yScale = mergeScales(result.yScale,
                                  (low: 0.0, high: col.toTensor(int).max.float))
  else:
    maybeRaise(xCol, cont)
    var yieldDf = df.count(xCol, name = CountCol,
                           weights = filledScales.getWeightScale(g))
    yieldDf[PrevValsCol] = constantColumn(0.0, yieldDf.len)
    let key = ("", Value(kind: VNull))
    yieldDf.maybeFilterUnique(result, result.xCol, result.yCol)
    result.yieldData[toObject(key)] = applyContScaleIfAny(yieldDf, cont, style, g.kind)
    result.setXAttributes(yieldDf, x)
    result.yScale = mergeScales(result.yScale,
                                (low: 0.0,
                                 high: max(yieldDf[CountCol].toTensor(int)).float))


  # `numY` for `count` stat is just max of the y scale. Since this uses `count` the
  # maximum value is always an `int`!
  result.numY = result.yScale.high.round.int
  doAssert result.numX == allClasses.len

proc determineExistingCombinations(fs: FilledScales): OrderedSet[Value] =
  # all possible combinations of the keys we group by
  let facet = fs.facets
  let facetScales = facet.columns # get scales of the Facet
  doAssert facetScales.len > 0
  var combinations: seq[seq[Value]]
  if facet.columns.len > 1:
    combinations = product(facetScales.mapIt(it.labelSeq))
  else:
    combinations = facetScales[0].labelSeq.mapIt(@[it])
  # create key / value pairs
  var combLabels: seq[Value]
  for c in combinations:
    var comb: seq[(string, Value)]
    for i, fc in facetScales:
      comb.add ($fc.col, c[i])
    combLabels.add toObject(comb)

  # combinations possibly contains non existing combinations too!
  var exists = newSeq[Value]()
  #var exists = initTable[Value, DataFrame]()
  # now check each geom for each `yieldData` element and see which
  # combination exists in them
  ##
  ## XXX: compute scales and (if needed) date ticks here?
  for fg in fs.geoms:
    for cb in combLabels:
      for xk in keys(fg.yieldData):
        if cb in xk:
          exists.add cb
          #var df = fg.yiedData[xk]
          #if cb notin exists:
          #  exists[cb] = df
          #else:
          #  let eDf = exists[cb]
          #  df = df.add eDf
          #  exists[cb] = df
          #exists.add (cb, scale, df)
          ## Extend xScale, yScale! Fully encompassing data scale of all geom data (can have different columns!) on the facet

  # determine x and y scale for each axis, and if applicable
  #let existSorted = exists.sortedByIt(it[0])
  #let combExists = existSorted.mapIt(it[0])
  #result = combExists.toOrderedSet # order based on label
  result = exists.sorted.toOrderedSet
  doAssert result.card <= combinations.len
  # check user input
  if facet.order.len > 0: # if user given order, overwrite the result!
    if facet.order.len != result.card: # but only if they match in terms of content
      raise newException(ValueError, "Input labels for facet to order by has " & $facet.order.len &
        " elements, but there are " & $result.card & " facet elements in total.")
    for x in facet.order:
      if x notin result:
        raise newException(ValueError, "Label " & $x & " not found in deduced " &
          " labels, but is present in custom ordered labels!")
    result = facet.order.toOrderedSet

proc calcScalesForLabel(facet: Facet,
                        fg: FilledGeom, label: Value): (ginger.Scale, ginger.Scale) =
  ## Given the `ScaleFreeKind` of the `facet` possibly calculate the
  ## real data range of the current label
  proc calcScale(df: DataFrame, col: string): ginger.Scale =
    let data = df[col].toTensor(Value)
    result = (low: min(data).toFloat(allowNull = true),
              high: max(data).toFloat(allowNull = true))
  if facet.sfKind in {sfFreeX, sfFreeY, sfFree}:
    # find the correct DF in the `yieldData` table for this label
    let labDf = fg.findData(label)
    if facet.sfKind in {sfFreeX, sfFree} and fg.dcKindX == dcContinuous:
      let xScale = calcScale(labDf, fg.xCol)
      # only change the scale, if it's not high == low
      result[0] = if xScale.low != xScale.high: xScale
                  else: fg.xScale
    if facet.sfKind in {sfFreeY, sfFree} and fg.dcKindY == dcContinuous:
      let yScale = calcScale(labDf, fg.yCol)
      # only change the scale, if it's not high == low
      result[1] = if yScale.low != yScale.high: yScale
                  else: fg.yScale

proc calcScalesForLabel(facet: Facet, geoms: seq[FilledGeom], label: Value): (ginger.Scale, ginger.Scale) =
  for fg in geoms:
    let (xScale, yScale) = calcScalesForLabel(facet, fg, label)
    template set(idx, scale): untyped = # set scale or merge
      if result[idx].isEmpty:
        result[idx] = scale
      else:
        result[idx] = mergeScales(result[idx], scale)
    set(0, xScale)
    set(1, yScale)

proc calcSingleFacets(facet: Facet, geoms: seq[FilledGeom]): OrderedTable[Value, SingleFacet] =
  var idx = 0
  for label in facet.combinations:
    # 1. calculate scales of each single facet using `yieldData`
    let (xScale, yScale) = calcScalesForLabel(facet, geoms, label)
    # 2. fill `Scale` fields that are needed, i.e.
    # `dateScale` field updated to fill `breaks`
    let singleFacet = SingleFacet(xScale: xScale,
                                  yScale: yScale,
                                  idx: idx)
    result[label] = singleFacet
    inc idx

proc postProcessFacet*(filledScales: var FilledScales, p: GgPlot) =
  ## Performs the post processing of the possible `Facets`. That means computing all possible
  ## single facets (i.e. the distinct labels) and computing the data scales of each facet.
  # 1. Compute all distinct labels for the facet scales
  filledScales.facets.combinations = determineExistingCombinations(filledScales)
  # 2. compute the individual facets and their scales
  filledScales.facets.facets = calcSingleFacets(filledScales.facets, filledScales.geoms)

proc postProcessScales*(filledScales: var FilledScales, p: GgPlot) =
  ## walk all geoms and create the dataframes required to draw the
  ## geoms
  var xScale: ginger.Scale
  var yScale: ginger.Scale
  for g in p.geoms:
    var df = if g.data.isSome: g.data.get
             else: p.data.shallowCopy # copy to avoid modifying for next geom!
    var filledGeom: FilledGeom
    case g.kind
    of gkPoint, gkLine, gkErrorBar, gkTile, gkText, gkRaster:
      # can be handled the same
      # need x and y data for sure
      case g.statKind
      of stIdentity:
        filledGeom = filledIdentityGeom(df, g, filledScales)
      of stCount:
        filledGeom = filledCountGeom(df, g, filledScales)
      of stSmooth:
        filledGeom = filledSmoothGeom(df, g, filledScales)
      else:
        filledGeom = filledBinDensityGeom(df, g, filledScales)
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
        filledGeom = filledBinDensityGeom(df, g, filledScales)
      of stDensity:
        # calculate KDE
        filledGeom = filledBinDensityGeom(df, g, filledScales)
      of stCount:
        raise newException(Exception, "For discrete counts of your data use " &
          "`geom_bar` instead!")
      of stSmooth:
        raise newException(Exception, "Smoothing statistics not implemented for histogram & frequency polygons. " &
          "Do you want a `density` plot using `geom_density` instead?")
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
      of stSmooth:
        raise newException(Exception, "Smoothing statistics not supported for bar plots. Do you want a " &
          "`density` plot using `geom_density` instead?")
      of stDensity:
        raise newException(Exception, "Density statistics not supported for bar plots. Do you want a " &
          "`density` plot using `geom_density` instead?")
    if not xScale.isEmpty:
      xScale = mergeScales(xScale, filledGeom.xScale)
      yScale = mergeScales(yScale, filledGeom.yScale)
    else:
      xScale = filledGeom.xScale
      yScale = filledGeom.yScale
    filledScales.geoms.add filledGeom

  let (finalXScale, _, _) = calcTickLocations(xScale, filledScales.getXTicks())
  let (finalYScale, _, _) = calcTickLocations(yScale, filledScales.getYTicks())

  filledScales.xScale = finalXScale
  filledScales.yScale = finalYScale

  # finally fill data relevant for facet plots
  if filledScales.facets.columns.len > 0:
    postProcessFacet(filledScales, p)
