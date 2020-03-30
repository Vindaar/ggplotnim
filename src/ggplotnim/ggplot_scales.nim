import tables, sets, algorithm, macros

import ggplot_types
when defined(defaultBackend):
  import formula
else:
  import ../../playground/arraymancer_backend
import ginger except Scale

#[
Contains procs dealing with `ggplot.Scale`.
]#

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
  ## yields all scales from the FilledScales
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
  # theses don't have to be yielded. They are only accessed
  # for the specific geoms that use them
  #genYield(xMin)
  #genYield(xMax)
  #genYield(yMin)
  #genYield(yMax)
  #genYield(width)
  #genYield(height)
  #genYield(text)
  genYield(yRidges)

iterator enumerateScales*(filledScales: FilledScales, geom: Geom): Scale =
  ## Yields all scales, which are allowed for the given geom
  var yieldedSet = initHashSet[Scale]()
  for s in enumerateScalesByIds(filledScales):
    if geom.gid in s.ids and s notin yieldedSet:
      yieldedSet.incl s
      yield s

macro genGetScale(field: untyped): untyped =
  let name = ident("get" & $field.strVal & "Scale")
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
  let name = ident("get" & $field.strVal & "Scale")
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
genGetOptScale(xMin)
genGetOptScale(yMin)
genGetOptScale(xMax)
genGetOptScale(yMax)
genGetOptScale(width)
genGetOptScale(height)
genGetScale(text)
genGetScale(yRidges)
# not used at the moment
#genGetScale(color)
#genGetScale(size)
#genGetScale(shape)
