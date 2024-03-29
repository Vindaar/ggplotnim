import tables, sets, macros, strutils
import datamancer

import ggplot_types, ggplot_utils
import ginger except Scale

#[
Contains procs dealing with `ggplot.Scale`.
]#

proc scaleFromData*(c: Column, s: Scale, ignoreInf: static bool = true): ginger.Scale =
  ## Combination of `colMin`, `colMax` to avoid running over the data
  ## twice. For large DFs to plot this makes a big difference.
  ##
  ## The input `Scale` is just for error messages.
  if c.len == 0: return (low: 0.0, high: 0.0)
  case c.kind
  of colFloat, colInt, colObject:
    # if we have a `colObject` here it (barring no bugs) means it was determined
    # to be continuous. That means it can be converted to float, because it is numeric
    # as long as we drop null values, which we do.
    let t = c.toTensor(float, dropNulls = true)
    var
      minVal = t[0]
      maxVal = t[0]
    for x in t:
      when ignoreInf:
        mixin fcNegInf, fcInf, classify # `import std/math` would work too
        if (classify(x) == fcNegInf or
            classify(x) == fcInf):
          continue
      minVal = min(x, minVal)
      maxVal = max(x, maxVal)
    result = (low: minVal, high: maxVal)
  of colConstant:
    # for a constant it can be valid, as long as the value is int / float
    let cVal = c.cCol
    if cVal.kind in {VInt, VFloat}:
      result = (low: cVal.toFloat, high: cVal.toFloat)
    else:
      raise newException(ValueError, "The input column `" & $s.col & "` is constant " &
        " with value: " & $cVal & ". Cannot compute a numeric scale from it.")
  of colBool, colString, colNone:
    raise newException(ValueError, "The input column `" & $s.col & "` is of kind " & $c.kind &
      " and thus discrete. `scaleFromData` should never be called.")
  of colGeneric:
    raise newException(ValueError, "The input column `" & $s.col & "` is of kind " & $c.kind &
      ". Generic columns are not supported yet.")

proc getColName*(s: Scale): string =
  ## returns the name of the referred column of the given Scale `s`.
  ## Usually this is just the stringification of `s.col`, but for
  ## `scTransformedData`, we have to assign to a column, which includes
  ## the name of the transformation, so that we don't override the
  ## existing column. This is because otherwise we modify the DF (we call
  ## `mutateInplace`) and will apply the transformations multiple times
  ## if several geoms are plotted!
  if s.isNil: return "" # can happen even in sane code paths!
  case s.scKind
  of scTransformedData:
    ## TODO: determine name of `trans` based on transformation proc!
    ## We create a new column for transformed data, because this allows us
    ## to avoid deep copying the input data frame.
    result = "log10(" & evaluate(s.col).toStr & ")"
  else:
    result = evaluate(s.col).toStr

proc getValue*(s: Scale, label: Value): ScaleValue =
  ## returns the `ScaleValue` of the given Scale `s` for `label`
  result = s.valueMap[label]

proc getLabelKey*(s: Scale, at: int): Value =
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

iterator enumerateScalesByIds*(filledScales: FilledScales): Scale =
  ## yields all scales from the FilledScales that are required for the
  ## default backends, which use ginger, i.e. not vega. All missing fields
  ## are accessed in a different manner.
  template genYield(field: untyped): untyped =
    if filledScales.field.main.isSome:
      yield filledScales.field.main.get
    for m in filledScales.field.more:
      yield m
  # color Scale
  genYield(x)
  genYield(y)
  genYield(color)
  genYield(fill)
  genYield(size)
  genYield(shape)
  genYield(yRidges)

iterator enumerateScalesByIdsVega*(filledScales: FilledScales): Scale =
  ## yields ``all`` scales from the FilledScales for Vega.
  template genYield(field: untyped): untyped =
    if filledScales.field.main.isSome:
      yield filledScales.field.main.get
    for m in filledScales.field.more:
      yield m
  # color Scale
  genYield(x)
  genYield(y)
  genYield(color)
  genYield(fill)
  genYield(size)
  genYield(shape)
  genYield(xMin)
  genYield(xMax)
  genYield(yMin)
  genYield(yMax)
  genYield(width)
  genYield(height)
  genYield(text)
  genYield(weight)
  genYield(yRidges)


iterator enumerateScales*(filledScales: FilledScales, geom: Geom): Scale =
  ## Yields all scales, which are allowed for the given geom
  var yieldedSet = initHashSet[Scale]()
  for s in enumerateScalesByIds(filledScales):
    if geom.gid in s.ids and s notin yieldedSet:
      yieldedSet.incl s
      yield s

iterator enumerateScalesVega*(filledScales: FilledScales, geom: Geom): Scale =
  ## Yields all scales, which are allowed for the given geom
  var yieldedSet = initHashSet[Scale]()
  for s in enumerateScalesByIdsVega(filledScales):
    if geom.gid in s.ids and s notin yieldedSet:
      yieldedSet.incl s
      yield s

macro genGetScale(field: untyped): untyped =
  let name = ident("get" & $field.strVal.capitalizeAscii & "Scale")
  result = quote do:
    proc `name`*(filledScales: FilledScales, geom = Geom(gid: 0)): Scale =
      result = new Scale
      if filledScales.`field`.main.isSome:
        # use main
        result = filledScales.`field`.main.get
      else:
        # find scale matching `gid`
        for s in filledScales.`field`.more:
          if geom.gid == 0 or geom.gid in s.ids:
            return s

macro genGetOptScale(field: untyped): untyped =
  let name = ident("get" & $field.strVal.capitalizeAscii & "Scale")
  result = quote do:
    proc `name`*(filledScales: FilledScales, geom = Geom(gid: 0)): Option[Scale] =
      #result = new Scale
      if filledScales.`field`.main.isSome:
        # use main
        result = some(filledScales.`field`.main.get)
      else:
        # find scale matching `gid`
        for s in filledScales.`field`.more:
          if geom.gid == 0 or geom.gid in s.ids:
            return some(s)


genGetScale(x)
genGetScale(y)
genGetOptScale(fill)
genGetOptScale(xMin)
genGetOptScale(yMin)
genGetOptScale(xMax)
genGetOptScale(yMax)
genGetOptScale(width)
genGetOptScale(height)
genGetScale(text)
genGetScale(yRidges)
genGetOptScale(weight)
# not used at the moment
#genGetScale(color)
#genGetScale(size)
#genGetScale(shape)

func updateAesRidges*(p: GgPlot): GgPlot =
  ## Adds the `ridges` information to the `GgPlot` object by assigning
  ## the `yRidges` aesthetic to the global aesthetic and forcing its
  ## scale to be discrete.
  doAssert p.ridges.isSome
  let ridge = p.ridges.unsafeGet
  let scale = some(Scale(scKind: scLinearData, col: ridge.col, axKind: akY,
                         hasDiscreteness: true, # force scale to be discrete!
                         dcKind: dcDiscrete,
                         ids: {0'u16 .. high(uint16)}))
  result = p
  result.aes.yRidges = scale

func getSecondaryAxis*(filledScales: FilledScales, axKind: AxisKind): SecondaryAxis =
  ## Assumes a secondary axis must exist!
  case axKind
  of akX:
    let xScale = filledScales.getXScale()
    result = xScale.secondaryAxis.unwrap()
  of akY:
    let yScale = filledScales.getYScale()
    result = yScale.secondaryAxis.unwrap()

func hasSecondary*(filledScales: FilledScales, axKind: AxisKind): bool =
  case axKind
  of akX:
    let xScale = filledScales.getXScale()
    if xScale.secondaryAxis.isSome:
      result = true
  of akY:
    let yScale = filledScales.getYScale()
    if yScale.secondaryAxis.isSome:
      result = true

func isEmpty*(s: ginger.Scale): bool =
  ## checks if the given scale is empty
  result = s.low == s.high

func mergeScales*(s1, s2: ginger.Scale): ginger.Scale =
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

func encompassingDataScale*(scales: seq[Scale],
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

proc findData*(fg: FilledGeom, label: Value): DataFrame =
  ## returns the `DataFrame` of the given `fg` of that label, which
  ## contains `label` in `yieldData`
  result = newDataFrame()
  for key, val in fg.yieldData:
    if key.kind == VObject and label.kind == VObject and label in key:
      # multiple keys may match, add DFs!
      result.add val[2]

proc findData*(fs: FilledScales, label: Value): DataFrame =
  ## returns the `DataFrame` of matching `label` of all `FilledGeoms`.
  ## contains `label` in `yieldData`
  var data = newSeq[DataFrame]()
  for fg in fs.geoms:
    data.add findData(fg, label)
  result = assignStack(data)
  if result.len == 0: # use the input data frame instead. No facet plot used
    result = fs.inputData
