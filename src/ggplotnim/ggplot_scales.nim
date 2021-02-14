import tables, sets, algorithm, macros, strutils

import ggplot_types, ggplot_utils
when defined(defaultBackend):
  import dataframe/fallback/formula
else:
  import dataframe/arraymancer_backend
import ginger except Scale

#[
Contains procs dealing with `ggplot.Scale`.
]#

proc getColName*(s: Scale): string =
  ## returns the name of the referred column of the given Scale `s`.
  ## Usually this is just the stringification of `s.col`, but for
  ## `scTransformedData`, we have to assign to a column, which includes
  ## the name of the transformation, so that we don't override the
  ## existing column. This is because otherwise we modify the DF (we call
  ## `mutateInplace`) and will apply the transformations multiple times
  ## if several geoms are plotted!
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
