## .. include:: ./docs/ggplotnim.rst

import sequtils, tables, sets, algorithm, strutils
import ginger except Scale
import parsecsv, streams, strutils, hashes, sugar

import random

import math
from seqmath import histogram, shape, linspace

import macros

import persvector
export persvector

import ggplotnim / formula
export formula

import ggplotnim / [ggplot_utils, ggplot_types, vega_utils]
export ggplot_types

export sets

import chroma
export chroma

import ggplotnim / colormaps / viridisRaw

# Ids start at 1, because `0` is our magic value for cases where the
# id does not matter!
var IdCounter = 1'u16
template incId(): uint16 =
  let old = IdCounter
  inc IdCounter
  old

template unwrap[T](opt: Option[T], raiseIfNil = true): untyped =
  var tmp: T
  if isSome opt:
    tmp = get opt
  elif raiseIfNil:
    raise newException(Exception, "Option " & $opt & " must exist")
  tmp

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
  template genYield(field: untyped): untyped =
    if gaes.field.isSome:
      yield gaes.field.unsafeGet
    elif paes.field.isSome:
      yield paes.field.unsafeGet
  # color Scale
  genYield(color)
  # fill Scale
  genYield(fill)
  # shape Scale
  genYield(shape)
  # size Scale
  # calling the template does not work ?! Fails with `undelcared identifier: scale`?!
  # genYield(size)
  if gaes.size.isSome:
    yield gaes.size.unsafeGet
  elif paes.size.isSome:
    yield paes.size.unsafeGet

iterator enumerateScales(p: GgPlot, geom: seq[Geom]): Scale =
  ## Overload for above iterator, which allows handing `seq[Geom]`
  var yieldedSet = initHashSet[Scale]()
  for g in geom:
    for scale in enumerateScales(p, g):
      if scale notin yieldedSet:
        yieldedSet.incl scale
        yield scale

iterator enumerateScalesByIds(plotScales: FilledScales): Scale =
  ## yields all scales from the FilledScales
  template genYield(field: untyped): untyped =
    if plotScales.field.main.isSome:
      yield plotScales.field.main.get
    for m in plotScales.field.more:
      yield m
  # color Scale
  genYield(x)
  genYield(y)
  genYield(color)
  genYield(size)
  genYield(shape)

iterator enumerateScales(plotScales: FilledScales, geom: Geom): Scale =
  ## Yields all scales, which are allowed for the given geom
  var yieldedSet = initHashSet[Scale]()
  for s in enumerateScalesByIds(plotScales):
    if geom.gid in s.ids and s notin yieldedSet:
      yieldedSet.incl s
      yield s

proc drawSampleIdx(sHigh: int, num = 100, seed = 42): seq[int] =
  ## draws `num` random sample indices with the seed `42` from the given `s`
  var r = initRand(seed) # for now just set a local state
  let idxNum = min(num - 1, sHigh)
  result = toSeq(0 .. idxNum).mapIt(r.rand(sHigh))

proc guessType(s: seq[Value], drawSamples: static bool = true): ValueKind =
  ## returns a ``guess`` (!) of the data type stored in `s`.
  ## We check a subset of 100 elements of the seq (or the whole if
  ## < 100 elements) and see if they match a single ValueKind.
  ## If they do match, return it, else return `VNull`
  when drawSamples:
    let indices = drawSampleIdx(s.high)
  else:
    # else we take all values as our indices
    let indices = toSeq(0 .. s.high)
  result = VNull
  var resultSet = false
  for i in indices:
    if not resultSet:
      result = s[i].kind
      resultSet = true
    else:
      if result != s[i].kind:
        return VNull

proc isDiscreteData(s: seq[Value], drawSamples: static bool = true): bool =
  ## returns an ``estimate`` (!) of whether the given sequence of
  ## data is most likely discrete or continuous. First determine
  ## most probable type, then check for discreteness
  ## - if Values are strings: discrete
  ## - if float / int: generate set of first 100 elements, check
  ##   if cardinality of set > 50: continuous, else discrete
  ## - if bool: discrete
  let guessedT = s.guessType(drawSamples = drawSamples)
  # TODO: Improve error messages in the case of guessedT == VNull
  # or change handling of that case
  case guessedT
  of VFloat, VInt:
    # same approach as in `guessType`
    when drawSamples:
      let indices = drawSampleIdx(s.high)
    else:
      let indices = toSeq(0 .. s.high)
    let elements = indices.mapIt(s[it]).toHashSet
    if elements.card > (indices.len.float / 8.0).round.int:
      result = false
    else:
      result = true
  of VString:
    # while the "discreteness" condition above might not always be satisfied for
    # strings, how would we represent string data on continuous scales?
    result = true
  of VBool:
    result = true
  of VNull:
    raise newException(ValueError, "Either `guessType` failed to determine the type " &
      "due to multiple base types in the column or the data is really `VNull`")
    #result = false
  of VObject:
     raise newException(Exception, "A VObject can neither be discrete nor continuous!")

proc discreteAndType(df: DataFrame, col: string):
    tuple[isDiscrete: bool, vKind: ValueKind] =
  ## deteremines both the `ValueKind` of the given column as well whether that
  ## data is discrete.
  let indices = drawSampleIdx(df.high)
  let data = indices.mapIt(df[col][it])
  result = (isDiscrete: isDiscreteData(data, drawSamples = false),
            vKind: guessType(data, drawSamples = false))

proc discreteAndType(data: seq[Value]):
    tuple[isDiscrete: bool, vKind: ValueKind] =
  ## deteremines both the `ValueKind` of the given column as well whether that
  ## data is discrete.
  let indices = drawSampleIdx(data.high)
  let data = indices.mapIt(data[it])
  result = (isDiscrete: isDiscreteData(data, drawSamples = false),
            vKind: guessType(data, drawSamples = false))


proc mapDataToScale(refVals: seq[Value], val: Value, scale: Scale): ScaleValue =
  let isDiscrete = refVals.isDiscreteData
  if isDiscrete:
    case scale.scKind
    of scColor, scFillColor, scSize, scShape:
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
  outType: typedesc[U],
  trans: ScaleTransform = nil): seq[U]

proc getXYcols(p: GgPlot, geom: Geom): tuple[x, y: string] =
  ## given both a `Geom` and a `GgPlot` object we need to choose the correct
  ## x, y aesthetics from the two.
  ## This proc returns the correct column keys respecting the precedence
  var
    x: Scale
    y: Scale
  # prefer geom x, y over plot x, y
  if geom.aes.x.isSome: x = geom.aes.x.get
  else: x = p.aes.x.get
  if geom.aes.y.isSome: y = geom.aes.y.get
  else: y = p.aes.y.get
  result = (x: x.col, y: y.col)

proc getAes(p:GgPlot, geom: Geom, axKind: AxisKind): Aesthetics =
  case axKind
  of akX:
    if geom.aes.x.isSome: result = geom.aes
    else: result = p.aes
  of akY:
    if geom.aes.y.isSome: result = geom.aes
    else: result = p.aes

proc getXYAes(p: GgPlot, geom: Geom): tuple[x, y: Aesthetics] =
  ## given both a `Geom` and a `GgPlot` object we need to choose the correct
  ## x, y aesthetics from the two.
  result = (x: getAes(p, geom, akX), y: getAes(p, geom, akY))

proc readXYcols(p: GgPlot, geom: Geom, outType: typedesc): tuple[x, y: seq[outType]] =
  ## given both a `Geom` and a `GgPlot` object we need to choose the correct
  ## x, y aesthetics from the two.
  ## The aesthetic of a geom overwrites the aesthetic of the plot!
  ## Afte this is determined, reads the data and convert to `outType`
  let (x, y) = getXYCols(p, geom)
  result = (x: p.data.dataTo(x, outType), y: p.data.dataTo(y, outType))

proc fillDiscreteColorScale(scKind: static ScaleKind, vKind: ValueKind, col: string,
                            labelSeq: seq[Value]): Scale =
  result = Scale(scKind: scColor, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq
  result.valueMap = initOrderedTable[Value, ScaleValue]()
  let colorCs = ggColorHue(labelSeq.len)
  for i, k in labelSeq:
    # NOTE: workaround, since we cannot do `kind: sckind` atm
    result.valueMap[k] = if scKind == scColor:
                           ScaleValue(kind: scColor, color: colorCs[i])
                         else:
                           ScaleValue(kind: scFillColor, color: colorCs[i])

proc fillDiscreteSizeScale(vKind: ValueKind, col: string,
                           labelSeq: seq[Value]): Scale =
  result = Scale(scKind: scSize, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq
  result.valueMap = initOrderedTable[Value, ScaleValue]()
  let numSizes = min(labelSeq.len, 5)
  const minSize = 2.0
  const maxSize = 7.0
  let stepSize = (maxSize - minSize) / numSizes.float
  for i, k in labelSeq:
    result.valueMap[k] = ScaleValue(kind: scSize, size: minSize + i.float * stepSize)

proc fillDiscreteLinearTransScale(
  scKind: static ScaleKind,
  col: string,
  axKind: AxisKind,
  vKind: ValueKind, labelSeq: seq[Value],
  df: DataFrame,#data: seq[Value],
  trans: Option[ScaleTransform] = none[ScaleTransform]()
     ): Scale =
  result = Scale(scKind: scKind, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq
  result.valueMap = initOrderedTable[Value, ScaleValue]()
  result.axKind = axKind
  if scKind == scTransformedData:
    ## we make  sure `trans` is some in the calling scope!
    result.trans = trans.get
  # TODO: make this the result of some proc we call. Maybe add a Formula
  # field, which is evaluated here, so that we apply arbitrary functions,
  # instead of just counting, which is conveniently done via grouping
  #let df = seqsToDf({col : data})
  let dfgrouped = df.group_by(by = col)
  for keys, subDf in groups(dfgrouped):
    doAssert keys.len == 1
    doAssert keys[0][0] == col
    result.valueMap[keys[0][1]] = ScaleValue(kind: scLinearData, val: %~ subDf.len)
  #for i, k in res.labelSeq:
  #  # TODO: don't filter here?! Inefficient, since we
  #  valueMap[k] = ScaleValue(kind: scLinearData, val: %~ df.filter(f{scale.col == k}).len)
  #raise newException(Exception, "`fillScale` not implemented for " & $scKind)

proc fillContinuousLinearScale(col: string, axKind: AxisKind, vKind: ValueKind,
                               dataScale: ginger.Scale,
                               df: DataFrame): Scale =
                               #data: seq[Value]): Scale =
  result = Scale(scKind: scLinearData, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  result.axKind = axKind
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)#data.high)
      else: idxs = idxsIn
      result = idxs.mapIt(ScaleValue(kind: scLinearData, val: df[col][it]))
  )

proc fillContinuousTransformedScale(col: string,
                                    axKind: AxisKind,
                                    vKind: ValueKind,
                                    trans: ScaleTransform,
                                    dataScale: ginger.Scale,
                                    df: DataFrame): Scale =
                                    #data: seq[Value]): Scale =
  result = Scale(scKind: scTransformedData, vKind: vKind, col: col,
                 dcKind: dcContinuous,
                 dataScale: dataScale)
  result.axKind = axKind
  result.trans = trans
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)#data.high)
      else: idxs = idxsIn
      result = idxs.mapIt(ScaleValue(kind: scTransformedData,
                                     val: trans(df[col][it])))
  )

proc fillContinuousColorScale(scKind: static ScaleKind,
                              col: string,
                              vKind: ValueKind,
                              dataScale: ginger.Scale,
                              df: DataFrame): Scale =
                              #data: seq[Value]): Scale =
  ## devise colormap mapping
  result = Scale(scKind: scKind, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  # for now just take viridis as default
  # map all values to values between 0-255 and get the correct idx of viridis map
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)#data.high)
      else: idxs = idxsIn
      result = newSeq[ScaleValue](idxs.len)
      for i, idx in idxs:
        var colorIdx = (255.0 * ((df[col][idx].toFloat - dataScale.low) /
                                 (dataScale.high - dataScale.low))).round.int
        colorIdx = min(255, colorIdx)
        let cVal = ViridisRaw[colorIdx]
        var scVal = if scKind == scColor:
                      ScaleValue(kind: scColor)
                    else:
                      ScaleValue(kind: scFillColor)
        scVal.color = color(cVal[0], cVal[1], cVal[2])
        result[i] = scVal
  )

proc fillContinuousSizeScale(col: string, vKind: ValueKind,
                             dataScale: ginger.Scale,
                             df: DataFrame): Scale =
                             #data: seq[Value]): Scale =
  const minSize = 2.0
  const maxSize = 7.0
  result = Scale(scKind: scSize, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)#data.high)
      else: idxs = idxsIn
      result = newSeq[ScaleValue](idxs.len)
      for i, idx in idxs:
        let size = (df[col][idx].toFloat - minSize) /
                   (maxSize - minSize)
        result[i] = ScaleValue(kind: scSize,
                               size: size)
  )

proc fillScaleImpl(
  vKind: ValueKind,
  isDiscrete: bool,
  col: string,
  df: DataFrame,#data: seq[Value],
  scKind: static ScaleKind,
  labelSeqOpt: Option[seq[Value]] = none[seq[Value]](), # for discrete data
  dataScaleOpt: Option[ginger.Scale] = none[ginger.Scale](), # for cont data
  axKindOpt: Option[AxisKind] = none[AxisKind](),
  trans: Option[ScaleTransform] = none[ScaleTransform]()): Scale =
  ## fills the `Scale` of `scKind` kind of the `aes`
  ## TODO: make aware of Geom.data optional field!
  ## NOTE: The given `col` arg is not necessarily exactly a DF key anymore, since
  ## it might contain two or more columns as its basis
  # get the data column we scale by
  if isDiscrete:
    # convert to set to filter duplicates, back to seq and sort
    # TODO: we could also use `sequtils.deduplicate` here
    let labelSeq = labelSeqOpt.unwrap()
    case scKind
    of scColor:
      result = fillDiscreteColorScale(scColor, vKind, col, labelSeq)
    of scFillColor:
      result = fillDiscreteColorScale(scFillColor, vKind, col, labelSeq)
    of scSize:
      result = fillDiscreteSizeScale(vKind, col, labelSeq)
    of scLinearData:
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillDiscreteLinearTransScale(scLinearData, col,
                                            axKind, vKind, labelSeq,
                                            df)
    of scTransformedData:
      doAssert trans.isSome, "Transform data needs a ScaleTransform procedure!"
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillDiscreteLinearTransScale(scTransformedData, col,
                                            axKind, vKind, labelSeq,
                                            df,#data,
                                            trans)
    of scShape:
      raise newException(ValueError, "Shape support not yet implemented for " &
        "discrete scales!")
  else:
    let dataScale = dataScaleOpt.unwrap()
    case scKind
    of scLinearData:
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillContinuousLinearScale(col, axKind, vKind, dataScale, df)
    of scTransformedData:
      doAssert trans.isSome, "Transform data needs a ScaleTransform procedure!"
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillContinuousTransformedScale(col, axKind, vKind, trans.get, dataScale, df)
    of scColor:
      result = fillContinuousColorScale(scColor, col, vKind, dataScale, df)
    of scFillColor:
      result = fillContinuousColorScale(scFillColor, col, vKind, dataScale, df)
    of scSize:
      result = fillContinuousSizeScale(col, vKind, dataScale, df)
    of scShape:
      raise newException(ValueError, "Shape not supported for continuous " &
        "variables!")

type
  ScaleData = tuple
    data: Option[DataFrame]
    scale: Scale

proc fillScale(df: DataFrame, scales: seq[Scale], scKind: static ScaleKind): seq[Scale] =
  # get the data column we scale by
  var data = newSeqOfCap[Value](df.len * scales.len)
  var fullCol = "" # full column. May be two or more columns together
  var transOpt: Option[ScaleTransform]
  var axKindOpt: Option[AxisKind]
  # in a first loop over the scales read the data required to make decisions about
  # the appearence of the resulting scale
  for s in scales:
    #if fullCol.len == 0:
    #  fullCol = s.col
    #else:
    #  fullCol = fullCol & ", " & s.col
    if s.col in df:
      data.add df.dataTo(s.col, Value)
    else:
      data.add @[Value(kind: VString, str: s.col)]

  # in the second loop for each of the scales add one filled scale to the result
  # using the combined dataset of all. This way we automatically get the correct
  # data range / correct number of labels while retaining a single scale per
  # geom.
  var dataScaleOpt: Option[ginger.Scale]
  var labelSeqOpt: Option[seq[Value]]
  for s in scales:
    case scKind
    of scLinearData:
      axKindOpt = some(s.axKind)
    of scTransformedData:
      axKindOpt = some(s.axKind)
      # ## we use the last transformation we find!
      transOpt = some(s.trans)
    else: discard

    let (isDiscrete, vKind) = discreteAndType(data)
    if vKind == VNull:
      echo "WARNING: Unexpected data type VNull of column: ", fullCol, "!"
      continue
      #return none[Scale]()

    if isDiscrete:
      labelSeqOpt = some(data.toHashSet.toSeq.sorted)
    else:
      dataScaleOpt = some((low: min(data).toFloat, high: max(data).toFloat))

    # now have to call `fillScaleImpl` with this information
    var filled = fillScaleImpl(vKind, isDiscrete, s.col, df, scKind,
                               labelSeqOpt, dataScaleOpt,
                               axKindOpt, transOpt)
    if scKind in {scLinearData, scTransformedData}:
      filled.secondaryAxis = s.secondaryAxis
    filled.ids = s.ids
    result.add filled

#proc fillAes(df: DataFrame, aes: Aesthetics): Aesthetics =
#  # TODO: we must estimate whether the given column is "continuous like" or
#  # "discrete like"
#  # If continuous like in case of
#  # - color: create colorbar
#  # - shape: raise exception not possible
#  # - size: bin the data and use bins as fixed sizes
#
#
#
#  result = aes
#  result.x = aes.x.fillScale(df, scLinearData) # TODO: add more data
#  result.y = aes.y.fillScale(df, scLinearData) # TODO: add more data
#  result.color = aes.color.fillScale(df, scColor)
#  result.fill = aes.fill.fillScale(df, scFillColor)
#  # not implemented yet:
#  #result.shape = aes.shape.fillScale(df, scShape)
#  result.size = aes.size.fillScale(df, scSize)
#
#proc addAes(p: var GgPlot, aes: Aesthetics) =
#  ## adds the aesthetics to the plot. This is non trivial, because
#  ## an aestetics encodes information that may have to be calculated
#  p.aes = fillAes(p.data, aes)

proc orNone(s: string): Option[string] =
  ## returns either a `some(s)` if s.len > 0 or none[string]()
  if s.len == 0: none[string]()
  else: some(s)

proc orNoneScale(s: string, scKind: static ScaleKind, axKind = akX): Option[Scale] =
  ## returns either a `some(Scale)` of kind `ScaleKind` or `none[Scale]` if
  ## `s` is empty
  if s.len > 0:
    case scKind
    of scLinearData:
      result = some(Scale(scKind: scLinearData, col: s, axKind: axKind))
    of scTransformedData:
      result = some(Scale(scKind: scTransformedData, col: s, axKind: axKind))
    else:
      result = some(Scale(scKind: scKind, col: s))
  else:
    result = none[Scale]()

proc aes*(x = "", y = "", color = "", fill = "", shape = "", size = ""): Aesthetics =
  result = Aesthetics(x: x.orNoneScale(scLinearData, akX),
                      y: y.orNoneScale(scLinearData, akY),
                      color: color.orNoneScale(scColor),
                      fill: fill.orNoneScale(scFillColor),
                      shape: shape.orNoneScale(scShape),
                      size: size.orNoneScale(scSize))

proc aes*(x: FormulaNode, color = "", fill = "", shape = "", size = ""): Aesthetics =
  # extract x and y from FormulaNode
  doAssert x.kind == fkTerm, "Formula must be a term!"
  doAssert x.lhs.kind == fkVariable, "LHS must be a variable!"
  doAssert x.rhs.kind == fkVariable, "RHS must be a variable!"
  result = Aesthetics(x: some(Scale(col: x.lhs.val.toStr,
                                    scKind: scLinearData,
                                    axKind: akX)),
                      y: some(Scale(col: x.rhs.val.toStr,
                                    scKind: scLinearData,
                                    axKind: akY)),
                      color: color.orNoneScale(scColor),
                      fill: fill.orNoneScale(scFillColor),
                      shape: shape.orNoneScale(scShape),
                      size: size.orNoneScale(scSize))

func fillIds*(aes: Aesthetics, gids: set[uint16]): Aesthetics =
  result = aes
  template fillIt(arg: untyped): untyped =
    if arg.isSome:
      var val = arg.get
      val.ids = gids
      arg = some(val)
  fillIt(result.x)
  fillIt(result.y)
  fillIt(result.color)
  fillIt(result.size)
  fillIt(result.shape)

proc ggplot*[T](data: T, aes: Aesthetics = aes()): GgPlot[T] =
  result = GgPlot[T](data: data,
                     numXticks: 10,
                     numYticks: 10)
  #result.addAes aes
  result.aes = aes.fillIds({0'u16 .. high(uint16)})
  # TODO: fill others with defaults
  # add default theme
  result.theme = Theme(discreteScaleMargin: some(quant(0.2,
                                                       ukCentimeter)))

proc geom_point*(aes: Aesthetics = aes(),
                 data = DataFrame(),
                 color: Color = black,
                 size: float = 3.0): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkPoint,
                style: some(Style(color: color,
                                  size: size)),
                aes: aes.fillIds({gid}))

proc geom_bar*(aes: Aesthetics = aes(),
               color: Color = grey20, # color of the bars
               position = "stack",
              ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let style = Style(lineType: ltSolid,
                    lineWidth: 1.0, # draw 1 pt wide black line to avoid white pixels
                                    # between bins at size of exactly 1.0 bin width
                    color: color, # default color
                    fillColor: color)
  let gid = incId()
  result = Geom(gid: gid,
                kind: gkBar,
                aes: aes.fillIds({gid}),
                style: some(style),
                position: pkKind)

proc geom_line*(aes: Aesthetics = aes(),
                data = DataFrame(),
                color: Color = grey20,
                size: float = 1.0,
                lineType: LineType = ltSolid): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkLine,
                style: some(Style(color: color,
                                  lineWidth: size,
                                  lineType: lineType,
                                  fillColor: transparent)),
                aes: aes.fillIds({gid}))

proc geom_histogram*(aes: Aesthetics = aes(),
                     binWidth = 0.0, bins = 30,
                     color: Color = grey20, # color of the bars
                     position = "stack",
                    ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let style = Style(lineType: ltSolid,
                    lineWidth: 1.0, # draw 1 pt wide black line to avoid white pixels
                                    # between bins at size of exactly 1.0 bin width
                    color: color, # default color
                    fillColor: color)
  let gid = incId()
  result = Geom(gid: gid,
                kind: gkHistogram,
                aes: aes.fillIds({gid}),
                numBins: bins,
                style: some(style),
                position: pkKind)

proc geom_freqpoly*(aes: Aesthetics = aes(),
                    color: Color = grey20, # color of the line
                    size: float = 1.0, # line width of the line
                    lineType: LineType = ltSolid,
                    bins = 30,
                    position = "identity",
                   ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let style = Style(lineType: lineType,
                    lineWidth: size,
                    color: color,
                    fillColor: transparent,)
  let gid = incId()
  result = Geom(gid: gid,
                kind: gkFreqPoly,
                aes: aes.fillIds({gid}),
                style: some(style),
                numBins: bins,
                position: pkKind)

proc geom_tile*(aes: Aesthetics = aes()): Geom =
  let gid = incId()
  result = Geom(gid: gid, kind: gkTile, aes: aes.fillIds({gid}))

proc facet_wrap*(fns: varargs[ FormulaNode]): Facet =
  result = Facet()
  for f in fns:
    doAssert f.kind == fkTerm
    doAssert f.rhs.val.kind == VString
    result.columns.add f.rhs.val.str

proc scale_x_log10*(): Scale =
  ## sets the X scale of the plot to a log10 scale
  result = Scale(col: "", # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akX,
                 dcKind: dcContinuous,
                 trans: proc(v: Value): Value =
                          result = %~ log10(v.toFloat))

proc scale_y_log10*(): Scale =
  ## sets the Y scale of the plot to a log10 scale
  result = Scale(col: "", # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akY,
                 dcKind: dcContinuous,
                 trans: proc(v: Value): Value =
                          result = %~ log10(v.toFloat))

func sec_axis*(trans: FormulaNode = nil, name: string = ""): SecondaryAxis =
  ## convenience proc to create a `SecondaryAxis`
  var fn: Option[FormulaNode]
  if not trans.isNil:
    fn = some(trans)
  result = SecondaryAxis(trans: fn,
                         name: name)

proc scale_x_continuous*(name: string = "",
                         secAxis: SecondaryAxis = sec_axis()): Scale =
  ## creates a continuous x axis with a possible secondary axis.
  # NOTE: See note for y axis below
  var msecAxis: SecondaryAxis
  var secAxisOpt: Option[SecondaryAxis]
  if secAxis.name.len > 0:
    msecAxis = secAxis
    msecAxis.axKind = akX
    secAxisOpt = some(msecAxis)
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akX,
                 dcKind: dcContinuous,
                 secondaryAxis: secAxisOpt)

proc scale_y_continuous*(name: string = "",
                         secAxis: SecondaryAxis = sec_axis()): Scale =
  ## creates a continuous y axis with a possible secondary axis.
  # NOTE: so far this only allows to set the name (read label) of the
  # axis. Also the possible transformation for the secondary axis
  # is ignored!
  var msecAxis: SecondaryAxis
  var secAxisOpt: Option[SecondaryAxis]
  if secAxis.name.len > 0:
    msecAxis = secAxis
    msecAxis.axKind = akY
    secAxisOpt = some(msecAxis)
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akY,
                 dcKind: dcContinuous,
                 secondaryAxis: secAxisOpt)

proc ggtitle*(title: string, subtitle = ""): (string, string) = (title, subtitle)

proc genDiscreteLegend(view: var Viewport,
                       cat: Scale,
                       markers: seq[GraphObject]) =
  let startIdx = view.len
  view.layout(1, rows = cat.valueMap.len + 1)
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
    let rect = ch.initRect(c(0.0, 0.0),
                           quant(ch.height.val * viewRatio, ukRelative),
                           quant(1.0, ukRelative),
                           style = some(style),
                           name = "markerRectangle")
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
                             color = markers[j].ptColor,
                             name = "markerPoint")
    var labelText = ""
    case cat.scKind
    of scColor, scFillColor, scShape, scSize:
      labelText = $cat.getLabelKey(j)
    else:
      raise newException(Exception, "`createLegend` unsupported for " & $cat.scKind)

    let label = ch.initText(
      Coord(
        x: c1(ch.height.val * viewRatio) +
           c1(quant(0.3, ukCentimeter).toRelative(some(ch.wImg)).val),
        y: c1(0.5)),
      labelText,
      textKind = goText,
      alignKind = taLeft,
      name = "markerText"
    )
    ch.addObj [rect, point, label]
    view[i] = ch
    inc j

proc genContinuousLegend(view: var Viewport,
                         cat: Scale,
                         markers: seq[GraphObject]) =
  case cat.scKind
  of scSize:
    view.layout(1, rows = 5 + 1)
  else:
    discard

proc createLegend(view: var Viewport,
                  cat: Scale,
                  markers: seq[GraphObject]) =
  ## creates a full legend within the given viewport based on the categories
  ## in `cat` with a headline `title` showing data points of `markers`
  let startIdx = view.len
  case cat.dcKind
  of dcDiscrete:
    view.genDiscreteLegend(cat, markers)
  of dcContinuous:
    # for now 5 sizes...
    view.genContinuousLegend(cat, markers)

  # get the first viewport for the header
  if startIdx < view.len:
    var header = view[startIdx]
    var label = header.initText(
      Coord(x: header.origin.x,
            y: c1(0.5)),
      cat.col,
      textKind = goText,
      alignKind = taLeft,
      name = "legendHeader")
    # set to bold
    label.txtFont.bold = true
    header.addObj label
    view[startIdx] = header

proc legendPosition*(x = 0.0, y = 0.0): Theme =
  ## puts the legend at position `(x, y)` in relative coordinates of
  ## the plot viewport in range (0.0 .. 1.0)
  result = Theme(legendPosition: some(Coord(x: c1(x),
                                            y: c1(y))))

proc xlab*(label = "", margin = NaN): Theme =
  if label.len > 0:
    result.xlabel = some(label)
  if classify(margin) != fcNaN:
    result.xlabelMargin = some(margin)

proc ylab*(label = "", margin = NaN): Theme =
  if label.len > 0:
    result.ylabel = some(label)
  if classify(margin) != fcNaN:
    result.ylabelMargin = some(margin)

proc applyTheme(pltTheme: var Theme, theme: Theme) =
  ## applies all elements of `theme`, which are `Some` to
  ## the same fields of `pltTheme`
  if theme.xlabelMargin.isSome:
    pltTheme.xlabelMargin = theme.xlabelMargin
  if theme.ylabelMargin.isSome:
    pltTheme.ylabelMargin = theme.ylabelMargin
  if theme.xlabel.isSome:
    pltTheme.xlabel = theme.xlabel
  if theme.ylabel.isSome:
    pltTheme.ylabel = theme.ylabel
  if theme.legendPosition.isSome:
    pltTheme.legendPosition = theme.legendPosition

proc `+`*(p: GgPlot, geom: Geom): GgPlot =
  ## adds the given geometry to the GgPlot object
  result = p
  # fill the aesthetics of the geom
  #var mgeom = geom
  #if geom.data.isSome:
  #  mgeom.aes = fillAes(geom.data.unsafeGet, geom.aes)
  #else:
  #  mgeom.aes = fillAes(p.data, geom.aes)
  #result.geoms.add mgeom
  result.geoms.add geom

proc `+`*(p: GgPlot, facet: Facet): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.facet = some(facet)

proc `+`*(p: GgPlot, aes: Aesthetics): GgPlot =
  ## adds the given aesthetics to the GgPlot object
  result = p
  #result.addAes aes
  result.aes = p

proc `+`*(p: GgPlot, titleTup: (string, string)): GgPlot =
  ## adds the given title / subtitle to the GgPlot object
  result = p
  result.title = titleTup[0]
  result.subtitle = titleTup[1]

proc `+`*(p: GgPlot, theme: Theme): GgPlot =
  ## adds the given theme (or theme element) to the GgPlot object
  result = p
  applyTheme(result.theme, theme)

proc applyScale(aes: Aesthetics, scale: Scale): Aesthetics =
  ## applies the given `scale` to the `aes` by returning a modified
  ## `aes`
  var mscale = scale
  result = aes
  case mscale.scKind
  of scLinearData, scTransformedData:
    # potentially `scale` has no `column` asigned yet, read from
    # `axKind` from the given `aes`. If `aes` has no `x`/`y` scale,
    # `mscale` will remain unchanged
    case scale.axKind
    of akX:
      if aes.x.isSome:
        mscale.dataScale = aes.x.get.dataScale
        mscale.col = aes.x.get.col
        result.x = some(mscale)
    of akY:
      if aes.y.isSome:
        mscale.dataScale = aes.y.get.dataScale
        mscale.col = aes.y.get.col
        result.y = some(mscale)
  of scFillColor, scColor: result.color = some(mscale)
  of scSize: result.size = some(mscale)
  of scShape: result.shape = some(mscale)

proc `+`*(p: GgPlot, scale: Scale): GgPlot =
  ## adds the given Scale to the GgPlot object.
  ## Overwrites
  result = p
  # Adding a scale requires to update the Scale of all existing
  # Aesthetics. Both of the plot and of its geoms. ggplot2 does the
  # inverse too. Adding a scale before another geom, still applies this
  # scale transformation to that geom...
  # scale_x_log10*() + geom_point(aes(x = "cty")) is considered the same as
  # geom_point(aes(x = "cty")) + scale_x_log10()
  # first apply to GgPlot aes:
  result.aes = applyScale(result.aes, scale)
  for p in mitems(result.geoms):
    p.aes = applyScale(p.aes, scale)

template anyScale(arg: untyped): untyped =
  if arg.main.isSome or arg.more.len > 0:
    true
  else:
    false

proc requiresLegend(plotScales: FilledScales): bool =
  ## returns true if the plot requires a legend to be drawn
  #if plotScales.color.main.isSome or
  #   plotScales.size.main.isSome or
  #   plotScales.shape.main.isSome:
  #  result = true
  if anyScale(plotScales.color) or
     anyScale(plotScales.size) or
     anyScale(plotScales.shape):
    result = true
  else:
    result = false

proc plotLayoutWithLegend(view: var Viewport) =
  ## creates a layout for a plot in the current viewport that leaves space
  ## for a legend. Important indices of the created viewports:
  ## - main plot: idx = 4
  ## - legend: idx = 5
  view.layout(3, 3, colwidths = @[quant(2.5, ukCentimeter),
                                  quant(0.0, ukRelative),
                                  quant(5.0, ukCentimeter)],
              rowheights = @[quant(1.25, ukCentimeter),
                             quant(0.0, ukRelative),
                             quant(2.0, ukCentimeter)])
  view[0].name = "topLeft"
  view[1].name = "title"
  view[2].name = "topRight"
  view[3].name = "yLabel"
  view[4].name = "plot"
  view[5].name = "legend"
  view[6].name = "bottomLeft"
  view[7].name = "xLabel"
  view[8].name = "bottomRight"

proc plotLayoutWithoutLegend(view: var Viewport) =
  ## creates a layout for a plot in the current viewport without a legend
  ## Main plot viewport will be:
  ## idx = 4
  view.layout(3, 3, colwidths = @[quant(2.5, ukCentimeter),
                                  quant(0.0, ukRelative),
                                  quant(1.0, ukCentimeter)],
              rowheights = @[quant(1.0, ukCentimeter),
                             quant(0.0, ukRelative),
                             quant(2.0, ukCentimeter)])
  view[0].name = "topLeft"
  view[1].name = "title"
  view[2].name = "topRight"
  view[3].name = "yLabel"
  view[4].name = "plot"
  view[5].name = "noLegend"
  view[6].name = "bottomLeft"
  view[7].name = "xLabel"
  view[8].name = "bottomRight"


proc dataTo[T: Table | OrderedTable | DataFrame; U](
  df: T,
  col: string,
  outType: typedesc[U],
  trans: ScaleTransform = nil): seq[U] =
  ## reads the column `col` from the Table / DataFrame and converts
  ## it to `outType`, returns it as a `seq[outType]`
  ## NOTE: This proc may also be used as a means to extract a column from
  ## a `DataFrame` as a `seq[Value]`, although that is identical to just
  ## calling `toSeq(df[column])`.
  ## NOTE: For now we just assume that a Table will be of kind
  ## `Table[string, seq[string]]`!
  if df.len == 0:
    return @[]
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
        if not trans.isNil:
          result = df[col].toSeq.mapIt(it.trans.toInt.outType)
        else:
          result = df[col].toSeq.mapIt(it.num.outType)
      of VFloat:
        if not trans.isNil:
          result = df[col].toSeq.mapIt(it.trans.fnum.outType)
        else:
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
  of scFillColor:
    # for FillColor we set both the stroke and fill color to the
    # same value
    result.color = scVal.color
    result.fillColor = scVal.color
  of scSize:
    result.size = scVal.size
  else:
    raise newException(Exception, "Setting style of " & $scVal.kind & " not " &
      "supported at the moment!")

iterator markerStylePairs(df: DataFrame, plotScales: FilledScales, geom: Geom): (int, (MarkerKind, Style)) {.exportc: "testtest".} =
  ## iterates all scales relevant for `p` and `geom` and yields the
  ## `MarkerKind` and the `Style` required for ``each datapoint`` as well
  ## as the current index.
  var style = geom.style.unsafeGet
  var marker = mkCircle

  # first collect all scales we have to consider for this geom
  var scales: seq[Scale]
  for scale in enumerateScales(plotScales, geom):
    case scale.scKind
    of scLinearData, scTransformedData:
      continue
    else: scales.add scale

  # then walk data frame and extracting correct style for each
  var lStyle: Style
  var val: ScaleValue
  #let df = if geom.data.isSome: geom.data.get else: p.data
  for i in 0 ..< df.len:
    lStyle = style
    for s in scales:
      # walk all scales and build the correct style
      case s.dcKind
      of dcDiscrete:
        if s.col notin df:
          # constant value
          val = s.getValue(%~ s.col)
        else:
          val = s.getValue(df[s.col][i])
        #let val = s.getValue(df[s.col][i])# %~ s.col)
        case val.kind
        of scShape:
          # Marker is not encoded in `ginger.Style`, hence retrieve manually
          marker = val.marker
        of scLinearData, scTransformedData:
          continue
        else:
          lStyle = changeStyle(lStyle, val)
      else:
        # get the `i`-th element from the data
        val = s.mapData(@[i])[0]
        case val.kind
        of scShape:
          # Marker is not encoded in `ginger.Style`, hence get retrieve manually
          marker = val.marker
        of scLinearData, scTransformedData:
          continue
        else:
          lStyle = changeStyle(lStyle, val)
    yield (i, (marker, lStyle))

iterator markerStyles(p: GgPlot, geom: Geom): (MarkerKind, Style) =
  ## iterates all scales relevant for `p` and `geom` and yields the
  ## `MarkerKind` and the `Style` required for ``each datapoint``.
  for _, markerStyle in markerStyles(p, geom):
    yield markerStyle

macro genGetScale(field: untyped): untyped =
  let name = ident("get" & $field.strVal & "Scale")
  result = quote do:
    proc `name`(plotScales: FilledScales, geom = Geom(gid: 0)): Scale =
      if plotScales.`field`.main.isSome:
        # use main
        result = plotScales.`field`.main.get
      else:
        # find scale matching `gid`
        for s in plotScales.`field`.more:
          if geom.gid == 0 or geom.gid in s.ids:
            return s

genGetScale(x)
genGetScale(y)
genGetScale(color)
genGetScale(size)
genGetScale(shape)

#func getXScale(plotScales: FilledScales, geom: Geom): Scale =
#  if plotScales.x.main.isSome:
#    # use main
#    result = plotScales.x.main.get
#  else:
#    # find scale matching `gid`
#    for s in plotScales.x.more:
#      if geom.gid in s.ids:
#        return s.scale

proc readScaleAwareData[T: tuple](plotScales: FilledScales, geom: Geom): T =
  ## TODO: not quite done yet. Only supports float etc.
  case geom.kind
  of gkLine, gkPoint:
    # gkLine, gkPoint need `x` and `y` (with stat = "identity"; default)
    let
      xScale = plotScales.getXScale(geom)
      yScale = plotScales.getYScale(geom)
    result = (xScale.mapData(), yScale.mapData())
  else:
    discard
#    template getData(scale: untyped): untyped =
#      let df = if geom.data.isSome: geom.data.get else: p.data
#      var data: seq[float]
#      case scale.scKind
#      of scLinearData:
#        data = df.dataTo(scale.col, float)
#      of scTransformedData:
#        data = df.dataTo(scale.col, float, scale.trans)
#      else: discard
#      data
#    let xdata = getData(xAes.x.get)
#    let ydata = getData(yAes.y.get)
#    result = (xData, yData)

proc createPointGobj(view: var Viewport,
                     df: DataFrame, geom: Geom,
                     plotScales: FilledScales): seq[GraphObject] =
  ## creates the GraphObjects for a `gkPoint` geom
  ## TODO: we could unify the code in the `create*Gobj` procs, by
  ## - making all procs in ginger take a `Style`
  ## - just build the style in the same way we do here (in a separate proc)
  ##   and create the `GraphObject`
  doAssert geom.kind == gkPoint
  doAssert geom.style.isSome
  # then walk data frame and extracting correct style for each
  let (xdata, ydata) = readScaleAwareData[(seq[ScaleValue],
                                           seq[ScaleValue])](plotScales,
                                                             geom)
  for i, (marker, style) in markerStylePairs(df, plotScales, geom):
    result.add initPoint(view, (x: xdata[i].val.toFloat, y: ydata[i].val.toFloat),
                         marker = marker,
                         color = style.color,
                         size = style.size)

proc createLineGobj(view: var Viewport,
                    df: DataFrame,
                    geom: Geom,
                    plotScales: FilledScales): seq[GraphObject] =
  ## creates the `goPolyLine` objects for the given geom
  doAssert geom.kind == gkLine
  doAssert geom.style.isSome
  # for line gobj we have to be a little more careful, because we draw the whole line
  # in one go. Thus collect marker styles and corresponding indices first
  var msMap = initTable[(MarkerKind, Style), seq[int]]()
  for i, (marker, style) in markerStylePairs(df, plotScales, geom):
    if (marker, style) notin msMap:
      msMap[(marker, style)] = newSeqOfCap[int](df.len)
    msMap[(marker, style)].add i

  # and then read all idx for each marker kind, sort them and draw the line
  let (xData, yData) = readScaleAwareData[(seq[ScaleValue], seq[ScaleValue])](plotScales, geom)
  # TODO: in case of float, handle non connected points!
  for markerStyle, pointIdxs in pairs(msMap):
    var points = newSeq[Point](pointIdxs.len)
    for i, idx in pointIdxs:
      points[i] = (x: xData[idx].val.toFloat, y: yData[idx].val.toFloat)
    points.sort((x, y: Point) => cmp(x.x, y.x))
    result.add view.initPolyLine(points, some(markerStyle[1]))

proc addHistoRect[T](view: var Viewport, val: T, style: Style,
                     yPos: Coord1D = c1(1.0),
                     width = 1.0 ) =
  ## creates a rectangle for a histogram and adds it to the viewports object
  # TODO: replace width argument by float range, so we
  # only allow values [0.0..1.0]
  if val.float > 0.0:
    # calc left side of bar based on width, since we wa t the bar to be centered
    let left = (1.0 - width) / 2.0
    let r = view.initRect(Coord(x: c1(left),
                                y: yPos), # bottom left
                          quant(width, ukRelative),
                          quant(-val.float, ukData),
                          style = some(style))
    view.addObj r

proc addHistoRects(view: var Viewport,
                   data: OrderedTable[string, (seq[int], Style)],
                   yScale: ginger.Scale,
                   position: PositionKind,
                   width = 1.0,
                   ignorePortIdxs: HashSet[int] = initHashSet[int]()) =
  ## Adds all rectangles for a histogram
  ## The `data` table contains both the `seq[float]` data and the `Style`
  ## that corresponds to it
  ## If `ignorePortIdxs` contains values, we will skip the children viewports
  ## corresponding to these indices
  ## generate the histogram
  var i = 0
  var idx = 0
  for p in mitems(view):
    #doAssert p.yScale.high >= hist.max.float
    if i in ignorePortIdxs:
      inc i
      continue
    case position
    of pkIdentity:
      for label, (val, style) in data:
        p.addHistoRect(val[idx], style, width = width)
    of pkStack:
      # create one rectangle for each label, each successive starting at the
      # top of the previous
      var prevTop = c1(1.0)
      for label, (val, style) in data:
        p.addHistoRect(val[idx], style, prevTop, width = width)
        prevTop = prevTop - Coord1D(pos: yScale.high - val[idx].float, kind: ukData,
                                    scale: yScale, axis: akY)
    of pkDodge:
      discard
    of pkFill:
      discard
    inc i
    inc idx

proc addHistoRects(view: var Viewport,
                   hist: seq[int],
                   yScale: ginger.Scale,
                   style: Style,
                   position: PositionKind,
                   width = 1.0,
                   ignorePortIdxs: HashSet[int] = initHashSet[int]()) =
  ## overload of the above working on a whole data frame. This just extracts the
  ## (label / data) pairs and hands it to `addHistoRects`
  var data = initOrderedTable[string, (seq[int], Style)]()
  data["x"] = (hist, style)
  view.addHistoRects(data, yScale, position, width = width, ignorePortIdxs = ignorePortIdxs)

proc addFreqPoly(view: var Viewport,
                 data: OrderedTable[string, (seq[int], Style)],
                 binWidth: float,
                 nbins: int,
                 position: PositionKind) =
  # only single viewport will be used
  # calculate bin centers
  let binCenters = linspace(view.xScale.low + binWidth / 2.0, view.xScale.high - binWidth / 2.0, nbins)
  # build data points for polyLine
  case position
  of pkIdentity:
    for label, (val, style) in data:
      var points = newSeq[Point](val.len)
      for i in 0 ..< nbins:
        points[i] = (x: binCenters[i], y: val[i].toFloat)
      view.addObj view.initPolyLine(points, some(style))
  of pkStack:
    var polyTab = initOrderedTable[string, seq[seq[Point]]]()
    for label in keys(data):
      polyTab[label] = newSeqWith(1, newSeq[Point]())

    # It tries to take care of drawing separate poly lines for each "unconnected" line, i.e.
    # each line disconnected by more than 1 empty bin
    # This is somewhat complicated.
    for i in 0 ..< nbins:
      var binVal = 0
      for label, (val, style) in data:
        # add the current value to the current bin value
        binVal = binVal + val[i]
        if val[i] > 0 or # has data int it, add
           binVal == 0 or # nothing in the bin yet, add
           polyTab[label][^1].len == 0 or # current polyLine is empty, add
          (polyTab[label][^1].len > 0 and i > 0 and # sanity checks
            (val[i - 1] > 0 and val[i] == 0) # this element is empty, but last
                                             # was not, so add to draw back to 0
          ):
          polyTab[label][^1].add (x: binCenters[i], y: binVal.toFloat)
        elif polyTab[label][^1].len > 0 and i != nbins - 1 and val[i + 1] == 0:
          # only create new seq, if has content and next element is 0
          polyTab[label].add newSeq[Point]()
    # now create the poly lines from the data
    for label, (val, style) in data:
      for line in polyTab[label]:
        if line.len > 0:
          view.addObj view.initPolyLine(line, some(style))
  else:
    doAssert false

proc addFreqPoly(view: var Viewport,
                 hist: seq[int],
                 binWidth: float,
                 nbins: int,
                 style: Style,
                 position: PositionKind) =
  var data = initOrderedTable[string, (seq[int], Style)]()
  data["x"] = (hist, style)
  view.addFreqPoly(data, binWidth, nbins, position)

proc createHistFreqPolyGobj(view: var Viewport,
                            df: DataFrame,
                            geom: Geom,
                            plotScales: FilledScales): seq[GraphObject] =
  # before performing a calculation for the histogram viewports, get the
  # new xScale, by calling calcTickLocations with it
  # Note: we don't have to assign it to the `view` viewport, since that will
  # happen when calculation of the actual ticks will be done later on
  #let xAes = getAes(p, geom, akX)
  #let xScale = xAes.x.get
  #let (isDiscrete, vKind) = discreteAndType(p.data, xScale.col)
  #if isDiscrete:
  #  raise newException(ValueError, "The selected column " & $xScale.col &
  #    " contains discrete data. Did you want to call geom_bar?")
  #
  #let (newXScale, _, _) = calcTickLocations(view.xScale, p.numXTicks)
  ## TODO: here?
  ## assign the new XScale to the view
  #view.xScale = newXScale
  #
  ## generate the histogram itself
  #let nbins = geom.numBins
  #let binWidth = (newXScale.high - newXScale.low).float / nbins.float
  #
  #var style: Style
  #if geom.style.isSome:
  #  style = geom.style.unsafeGet
  #else:
  #  # TODO: inherit from parent somehow?
  #  discard
  #
  ## create the layout needed for the different geoms
  #case geom.kind
  #of gkHistogram:
  #  view.layout(nbins, 1)
  #else:
  #  doAssert geom.kind == gkFreqPoly
  #  # we juse use the given `Viewport`
  #
  #var any = false
  #var yScaleBase = (low: 0.0, high: 0.0)
  #for scale in enumerateScales(p, geom):
  #  any = true
  #  var data: seq[Value]
  #  # TODO: we do not actually make use of `data`!
  #  if scale.col in p.data:
  #    data = p.data.dataTo(scale.col, Value)
  #  else:
  #    data = toSeq(0 ..< p.data.len).mapIt(Value(kind: VString, str: scale.col))
  #
  #  # accumulate data before we do anything with our viewports, since depending on
  #  # the `position` of the `geom`, we might need the data for each label at the
  #  # same time
  #  var labData = initOrderedTable[string, (seq[int], Style)]()
  #  var numLabel = 0
  #  for label, val in scale:
  #    when type(p.data) is DataFrame:
  #      let df = p.data.filter(f{scale.col == label})
  #    else:
  #      let df = toDf(p.data).filter(f{scale.col == label})
  #    # just the regular rectangles, one behind another
  #    style = changeStyle(style, val)
  #    const epsilon = 1e-2
  #    # for every label, we increase the `lineWidth` by `epsilon` so that the last
  #    # layer completely covers the ones before it
  #    # TODO: this is still not a nice solution, especially regarding top and bottom
  #    # of the rectangles!
  #    if geom.kind == gkHistogram:
  #      style.lineWidth = style.lineWidth + numLabel.float * epsilon
  #    inc numLabel
  #    let rawData = df.dataTo(p.aes.x.get.col, float)
  #    # generate the histogram
  #    var (hist, bins) = histogram(rawData, bins = nbins, range = (newXScale.low, newXScale.high))
  #    # increase the scale if necessary
  #    yScaleBase = (low: 0.0, high: max(yScaleBase.high, hist.max.float))
  #    labData[$label] = (hist, style)
  #
  #  # reverse the order of `labData`, so that the element class with highest string
  #  # value is located at the bottom of the histogram (to match `ggplot2`)
  #  labData.sort(
  #    cmp = (
  #      proc(a, b: (string, (seq[int], Style))): int =
  #        result = system.cmp(a[0], b[0])
  #    ),
  #    order = SortOrder.Descending)
  #
  #  # calculate the new Y scale maximum of the plot
  #  var newYMax = yScaleBase.high
  #  case geom.position
  #  of pkStack:
  #    # for `pkStack` we must find the bin with the largest sum of
  #    # label elements
  #    var sums = newSeq[int](nbins)
  #    for i in 0 ..< nbins:
  #      var s = 0
  #      for label in keys(labData):
  #        s += labData[label][0][i]
  #      sums[i] = s
  #    newYMax = max(sums).float
  #  of pkFill:
  #    # TODO: `pkFill` is basically the same as `pkStack`, only that the sum of
  #    # any non empty bin must add up to 1.0
  #    raise newException(Exception, "Not yet implemented!")
  #  else: discard
  #  # with the data available, create the histogram rectangles
  #  # fix the data scales on the children viewports
  #  yScaleBase = (low: 0.0, high: newYMax)
  #  let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)
  #  case geom.kind
  #  of gkHistogram:
  #    view.addHistoRects(labData, newYScale, geom.position)
  #  of gkFreqPoly:
  #    view.yScale = newYScale
  #    view.addFreqPoly(labData, binWidth, nbins, geom.position)
  #  else:
  #    doAssert false
  #if not any:
  #  var hist: seq[int]
  #  var bins: seq[float]
  #  case vKind
  #  of VFloat, VInt:
  #    let xSc = p.aes.x.get
  #    let rawData = p.data.dataTo(xSc.col, float)
  #    doAssert xSc.dcKind == dcContinuous
  #    doAssert xSc.mapData().mapIt(it.val.toFloat) == rawData
  #    # generate the histogram
  #    (hist, bins) = histogram(rawData, bins = nbins, range = (newXScale.low, newXScale.high))
  #  else: doAssert false, "not implemented " & $vKind & " for histogram/freqpoly"
  #  # set the y scale
  #  yScaleBase = (low: 0.0, high: hist.max.float)
  #  # fix the data scales on the children viewports
  #  let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)
  #  case geom.kind
  #  of gkHistogram:
  #    view.addHistoRects(hist, newYScale, style, geom.position)
  #  of gkFreqPoly:
  #    view.yScale = newYScale
  #    view.addFreqPoly(hist, binWidth, nbins, style, geom.position)
  #  else:
  #    doAssert false
  #
  ## fix the data scales on the children viewports
  #let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)
  #view.yScale = newYScale
  #for ch in mitems(view):
  #  ch.yScale = newYScale
  discard

proc createBarGobj(view: var Viewport,
                   df: DataFrame,
                   geom: Geom,
                   plotScales: FilledScales): seq[GraphObject] =
  ## creates the GraphObjects required for a bar plot
  #let xAes = getAes(p, geom, akX)
  #let xScale = xAes.x.get
  #let (isDiscrete, vKind) = discreteAndType(p.data, xScale.col)
  #if not isDiscrete:
  #  raise newException(ValueError, "The selected column " & $xScale.col &
  #    "contains continuous data. Did you want to call geom_histogram?")
  #let numElements = xScale.labelSeq.len
  #var style: Style
  #if geom.style.isSome:
  #  style = geom.style.unsafeGet
  #else:
  #  # TODO: inherit from parent somehow?
  #  #doAssert false
  #  discard
  #
  ##of VFloat, VInt:
  ##  doAssert false, "not implemented"
  #let discrMarginOpt = p.theme.discreteScaleMargin
  #var discrMargin = quant(0.0, ukRelative)
  #if discrMarginOpt.isSome:
  #  discrMargin = discrMarginOpt.unsafeGet
  #let indWidths = toSeq(0 ..< numElements).mapIt(quant(0.0, ukRelative))
  #view.layout(numElements + 2, 1,
  #            colwidths = concat(@[discrMargin],
  #                               indWidths,
  #                               @[discrMargin]))
  #let toIgnore = toSet([0, numElements + 1])
  #var yScaleBase: ginger.Scale
  #case vKind
  #of VFloat, VInt, VString:
  #  # instead get count of each element
  #  var maxVal = 0
  #  var hist: seq[int]
  #  for k, v in pairs(xScale):
  #    let val = v.val.toInt.int
  #    hist.add val
  #    if val > maxVal:
  #      maxVal = val
  #  yScaleBase = (low: 0.0, high: maxVal.float)
  #  view.addHistoRects(hist, yScaleBase, style, geom.position,
  #                     width = 0.8, ignorePortIdxs = toIgnore)
  #else:
  #  doAssert false, "not implemented"
  ## fix child viewport yscales
  #let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)
  #view.yScale = newYScale
  #for ch in mitems(view):
  #  ch.yScale = newYScale
  discard

proc createGobjFromGeom(view: var Viewport,
                        df: DataFrame,
                        geom: Geom,
                        plotScales: FilledScales): seq[GraphObject] =
  ## performs the required conversion of the data from the data
  ## frame according to the given `geom`
  case geom.kind
  of gkPoint:
    result = view.createPointGobj(df, geom, plotScales)
  of gkHistogram, gkFreqPoly:
    result = view.createHistFreqPolyGobj(df, geom, plotScales)
  of gkLine:
    result = view.createLineGobj(df, geom, plotScales)
  of gkBar:
    result = view.createBarGobj(df, geom, plotScales)
  else:
    discard

proc generateLegendMarkers(plt: Viewport, scale: Scale): seq[GraphObject] =
  ## generate the required Legend Markers for the given `aes`
  ## TODO: add different objects to be shown depending on the scale and geom.
  ## E.g. in case of `fill` fill the whole rectangle with the color. In case
  ## of geom_line only draw a line etc.
  ## Thus also put the rectangle drawing here.
  # TODO: rewrite this either via a template, proc or macro!
  case scale.sckind
  of scColor:
    case scale.dcKind
    of dcDiscrete:
      for i in 0 ..< scale.valueMap.len:
        let color = scale.getValue(scale.getLabelKey(i)).color
        result.add initPoint(plt,
                             (0.0, 0.0), # dummy coordinates
                             marker = mkCircle,
                             color = color) # assign same marker as above
    of dcContinuous:
      # TODO: replace by a creation of a colormap display
      discard
  of scFillColor:
    for i in 0 ..< scale.valueMap.len:
      let color = scale.getValue(scale.getLabelKey(i)).color
      result.add initPoint(plt,
                           (0.0, 0.0), # dummy coordinates
                           marker = mkCircle,
                           color = color) # assign same marker as above
  of scShape:
    for i in 0 ..< scale.valueMap.len:
      result.add initPoint(plt,
                           (0.0, 0.0), # dummy coordinates
                           marker = scale.getValue(scale.getLabelKey(i)).marker)
  of scSize:
   for i in 0 ..< scale.valueMap.len:
     let size = scale.getValue(scale.getLabelKey(i)).size
     result.add initPoint(plt,
                          (0.0, 0.0), # dummy coordinates
                          marker = mkCircle,
                          size = size)
  else:
    raise newException(Exception, "`createLegend` unsupported for " & $scale.scKind)

# TODO: move this, remove one of the two (instead calc from the other)
# TODO2: use almostEqual from `formula` instead of this one here!!!
proc smallestPow(x: float): float =
  doAssert x > 0.0
  result = 1.0
  if x < 1.0:
    while result > x and not result.almostEqual(x):
      result /= 10.0
  else:
    while result < x and not result.almostEqual(x):
      result *= 10.0
    result /= 10.0

proc largestPow(x: float): float =
  doAssert x > 0.0
  result = 1.0
  if x < 1.0:
    while result > x and not result.almostEqual(x):
      result /= 10.0
    result *= 10.0
  else:
    while result < x and not result.almostEqual(x):
      result *= 10.0

proc tickposlog(minv, maxv: float): (seq[string], seq[float]) =
  let numTicks = 10 * (log10(maxv) - log10(minv)).round.int
  var
    labs = newSeq[string]()
    labPos = newSeq[float]()
  for i in 0 ..< numTicks div 10:
    let base = (minv * pow(10, i.float))
    let test = linspace(base, 9 * base, 9)
    labs.add formatTickValue(base)
    labs.add toSeq(0 ..< 8).mapIt("")
    labPos.add test.mapIt(it.log10)
  labs.add $maxv
  labPos.add log10(maxv)
  result = (labs, labPos)

func getSecondaryAxis(plotScales: FilledScales, axKind: AxisKind): SecondaryAxis =
  ## Assumes a secondary axis must exist!
  case axKind
  of akX:
    let xScale = plotScales.getXScale()
    result = xScale.secondaryAxis.unwrap()#p.aes.x.get.secondaryAxis.get
  of akY:
    let yScale = plotScales.getYScale()
    result = yScale.secondaryAxis.unwrap()#p.aes.x.get.secondaryAxis.get

func hasSecondary(plotScales: FilledScales, axKind: AxisKind): bool =
  case axKind
  of akX:
    let xScale = plotScales.getXScale()
    if xScale.secondaryAxis.isSome:
      result = true
  of akY:
    let yScale = plotScales.getYScale()
    if yScale.secondaryAxis.isSome:
      result = true

func hasSecondary(theme: Theme, axKind: AxisKind): bool =
  case axKind
  of akX:
    if theme.xLabelSecondary.isSome:
      result = true
  of akY:
    if theme.yLabelSecondary.isSome:
      result = true

proc handleContinuousTicks(view: var Viewport, p: GgPlot, axKind: AxisKind,
                           scale: Scale, numTicks: int,
                           isSecondary = false): seq[GraphObject] =
  case scale.scKind
  of scLinearData:
    let ticks = view.initTicks(axKind, numTicks, isSecondary = isSecondary)
    let tickLabs = view.tickLabels(ticks, isSecondary = isSecondary)
    view.addObj concat(ticks, tickLabs)
    result = ticks
  of scTransformedData:
    # for now assume log10 scale
    let minVal = p.data[scale.col].toSeq.filterIt(it.toFloat > 0.0).min.toFloat.smallestPow
    let maxVal = p.data[scale.col].toSeq.filterIt(it.toFloat > 0.0).max.toFloat.largestPow
    let (labs, labelpos) = tickposlog(minVal, maxVal)
    var tickLocs: seq[Coord1D]
    case axKind
    of akX:
      tickLocs = labelpos.mapIt(Coord1D(pos: it,
                                        kind: ukData,
                                        scale: view.xScale,
                                        axis: akX))
      view.xScale = (low: log10(minVal), high: log10(maxVal))
    of akY:
      tickLocs = labelpos.mapIt(Coord1D(pos: it,
                                        kind: ukData,
                                        scale: view.yScale,
                                        axis: akY))
      view.yScale = (low: log10(minVal), high: log10(maxVal))

    let (tickObjs, labObjs) = view.tickLabels(tickLocs, labs, axKind, isSecondary = isSecondary)
    view.addObj concat(tickObjs, labObjs)
    result = tickObjs
  else: discard

proc handleDiscreteTicks(view: var Viewport, p: GgPlot, axKind: AxisKind,
                         scale: Scale,
                         isSecondary = false): seq[GraphObject] =
  # create custom tick labels based on the possible labels
  # and assign tick locations based on ginger.Scale for
  # linear/trafo kinds and evenly spaced based on string?
  # start with even for all
  if isSecondary:
    raise newException(Exception, "Secondary axis for discrete axis not yet implemented!")
  let numTicks = scale.labelSeq.len
  var tickLabels: seq[string]
  var tickLocs: seq[Coord1D]
  let gScale = if scale.axKind == akX: view.xScale else: view.yScale

  # TODO: check if we should use w/hImg here, distinguish the axes
  let discrMarginOpt = p.theme.discreteScaleMargin
  var discrMargin = 0.0
  if discrMarginOpt.isSome:
    discrMargin = discrMarginOpt.unsafeGet.toRelative(length = some(view.wView)).val
  # NOTE: the following only holds if def. of `wview` changed in ginger
  # doAssert view.wview != view.wimg
  let barViewWidth = (1.0 - 2 * discrMargin) / numTicks.float
  let centerPos = barViewWidth / 2.0
  for i in 0 ..< numTicks:
    tickLabels.add $scale.labelSeq[i]
    # in case of a discrete scale we have categories, which are evenly spaced.
    # taking into account the margin of the plot, calculate center of all categories
    let pos = discrMargin + i.float * barViewWidth + centerPos
    tickLocs.add Coord1D(pos: pos,
                         kind: ukData,
                         scale: gScale,
                         axis: axKind)
  let (tickObjs, labObjs) = view.tickLabels(tickLocs, tickLabels, axKind)
  view.addObj concat(tickObjs, labObjs)
  result = tickObjs

proc handleTicks(view: var Viewport, plotScales: FilledScales, p: GgPlot,
                 axKind: AxisKind): seq[GraphObject] =
  var scale: Scale#Option[Scale]
  var numTicks: int
  case axKind
  of akX:
    scale = plotScales.getXScale() #.aes.x
    numTicks = p.numXTicks
  of akY:
    scale = plotScales.getYScale() #aes.y
    numTicks = p.numYTicks
  if scale.col.len > 0: #isSome:
    #let scale = scale.get
    case scale.dcKind
    of dcDiscrete:
      result = view.handleDiscreteTicks(p, axKind, scale)
      if hasSecondary(plotScales, axKind):
        let secAxis = plotScales.getSecondaryAxis(axKind)
        result.add view.handleDiscreteTicks(p, axKind, scale, isSecondary = true)
    of dcContinuous:
      result = view.handleContinuousTicks(p, axKind, scale, numTicks)
      if hasSecondary(plotScales, axKind):
        let secAxis = plotScales.getSecondaryAxis(axKind)
        result.add view.handleContinuousTicks(p, axKind, scale, numTicks, isSecondary = true)
  else:
    # this should mean the main geom is histogram like?
    doAssert axKind == akY, "we can have akX without scale now?"
    # in this case don't read into anything and just call ticks / labels
    let ticks = view.initTicks(axKind, numTicks)
    let tickLabs = view.tickLabels(ticks)
    view.addObj concat(ticks, tickLabs)
    result = ticks

template argMaxIt(s, arg: untyped): untyped =
  ## `s` has to have a `pairs` iterator
  # TODO: move elsehere
  block:
    var
      maxVal = 0
      maxId = 0
    for i, it {.inject.} in s:
      if maxVal < arg:
        maxId = i
        maxVal = arg
    maxId

proc handleLabels(view: var Viewport, theme: Theme) =#p: GgPlot) =
  ## potentially moves the label positions and enlarges the areas (not yet)
  ## potentially moves the label positions and enlarges the areas (not yet)
  ## for the y label / tick label column or x row.
  # TODO: clean this up!
  var
    xLabObj: GraphObject
    yLabObj: GraphObject
    xMargin: Coord1D
    yMargin: Coord1D
  let
    xlabTxt = theme.xLabel.unwrap()#labelName(p, akX)
    ylabTxt = theme.yLabel.unwrap()#labelName(p, akY)
  #if p.theme.xlabelMargin.isSome:
  #  marginVal = p.theme.xlabelMargin.get
  template getMargin(marginVar, themeField, nameVal, axKind: untyped): untyped =
    if not themeField.isSome:
      let labs = view.objects.filterIt(it.name == nameVal)#"ytickLabel")
      let labNames = labs.mapIt(it.txtText)
      let labLens = labNames.argMaxIt(len(it)) #labNames.sortedByIt(len(it))
      let font = labs[0].txtFont
      case axKind
      of akX:
        marginVar = Coord1D(pos: 1.1, kind: ukStrHeight,
                            text: labNames[labLens], font: font)
      of akY:
        marginVar = Coord1D(pos: 1.0, kind: ukStrWidth,
                            text: labNames[labLens], font: font) +
                    Coord1D(pos: 0.3, kind: ukCentimeter)

  template createLabel(label, labproc, labTxt, themeField, marginVal: untyped,
                       isSecond = false): untyped =
    if themeField.isSome:
      label = labproc(view,
                      labTxt,
                      margin = get(themeField),
                      isCustomMargin = true,
                      isSecondary = isSecond)
    else:
      label = labproc(view,
                      labTxt,
                      margin = marginVal,
                      isSecondary = isSecond)#quant(margin.toPoints.pos, ukPoint).toCentimeter.val + 0.5)

  getMargin(xMargin, theme.xlabelMargin, "xtickLabel", akX)
  getMargin(yMargin, theme.ylabelMargin, "ytickLabel", akY)
  createLabel(yLabObj, ylabel, yLabTxt, theme.yLabelMargin, yMargin)
  createLabel(xLabObj, xlabel, xLabTxt, theme.xLabelMargin, xMargin)

  view.addObj @[xLabObj, yLabObj]

  if theme.hasSecondary(akX):#p, akX):
    let secAxisLabel = theme.xLabelSecondary.unwrap()
    var labSec: GraphObject
    createLabel(labSec, xlabel, secAxisLabel, theme.yLabelMargin, 0.0,#xMargin,
                true)
    view.addObj @[labSec]
  if theme.hasSecondary(akY):#p, akY):
    let secAxisLabel = theme.yLabelSecondary.unwrap()
    var labSec: GraphObject
    createLabel(labSec, ylabel, secAxisLabel, theme.yLabelMargin, 0.0,#yMargin,
                true)
    view.addObj @[labSec]

#proc mergeScales(view: var Viewport, geom: Geom) =
proc mergeScales(p: var GgPlot, geom: Geom) =
  ## This proc performs a merging of the
  ## NOTE: Not to be confused with `updateDataScale`!
  ## TODO: We still have to somehow successfully combine not only the
  ## ginger.Scales of the viewport, but also all other scales,
  # performs a merge of the current `geom` aes  into the corresponding
  # `p` aes

  # NOTE NOTE NOTE:
  # This merges *ONLY* the data scales! `mapData` etc are not touched
  # since those are handled with fillScales and the access of those procs
  # when createing the graph objects!
  var pAes = p.aes
  let gAes = geom.aes
  var scale: Scale
  macro mergeInto(dcKind: DiscreteKind, scKind: ScaleKind, fident: static string): untyped =
    # this template assumes that `gAes.field` is some!
    let field = ident(fident)
    result = quote do:
      var toMerge = false
      if pAes.`field`.isSome:
        scale = pAes.`field`.get
        toMerge = true
      else:
        pAes.`field` = gAes.`field`
      if toMerge:
        case `dcKind`
        of dcContinuous:
          case `scKind`
          of scLinearData:
            let newscale = (min(scale.dataScale.low, gAes.`field`.get.dataScale.low),
                            max(scale.dataScale.high, gAes.`field`.get.dataScale.high))
            scale.dataScale = newScale
          else:
            raise newException(Exception, "no bad : " & $`scKind` & " and " & $`dcKind`)
        of dcDiscrete:
          case `scKind`
          of scLinearData:
            discard
          else:
            raise newException(Exception, "no bad : " & $`scKind` & " and " & $`dcKind`)
        pAes.`field` = some(scale)

  macro checkIfSome(fident: static string): untyped =
    let field = ident(fident)
    result = quote do:
      if gAes.`field`.isSome and
        (pAes.`field`.isNone xor
             (pAes.`field`.isSome and
              pAes.`field`.get.dcKind == gAes.`field`.get.dcKind)) and
        (pAes.`field`.isNone xor
             (pAes.`field`.isSome and
              pAes.`field`.get.scKind == gAes.`field`.get.scKind)):
        let gAesScale = gAes.`field`.get
        mergeInto(gAesScale.dcKind, gAesScale.scKind, `fident`)

  checkIfSome("x")
  checkIfSome("y")
  checkIfSome("color")
  checkIfSome("size")
  checkIfSome("shape")
  p.aes = pAes

proc generatePlot(view: Viewport, p: GgPlot, plotScales: FilledScales,
                  theme: Theme,
                  addLabels = true): Viewport =
  # first write all plots into dummy viewport
  result = view
  result.background()

  # set the data scale for the result
  let xScale = plotScales.getXScale()
  let yScale = plotScales.getYScale()
  result.xScale = xScale.dataScale
  result.yScale = yScale.dataScale

  for geom in p.geoms:
    # for each geom, we create a child viewport of `result` covering
    # the whole resultport, which will house the data we just created.
    # Due to being a child, if will be drawn *after* its parent. This way things like
    # ticks will be below the data.
    # On the other hand this allows us to draw several geoms in on a plot and have the
    # order of the function calls `geom_*` be preserved
    var pChild = result.addViewport(name = "data")

    #p.mergeScales(geom)
    let df = if geom.data.isSome: geom.data.get else: p.data
    let gobjs = pChild.createGobjFromGeom(df, geom, plotScales)

    # add the data to the child
    pChild.addObj gobjs
    # add the data viewport to the view
    result.children.add pChild

    # for these types there is no y data scale attached to the image so far,
    # thus we assign `pChild`'s data scale to it
    # TODO: solve this more elegantly!

    # potentially the creation of the graph objects have altered the scales
    # of the viewport. We have to make sure that the parents receive an updated
    # scale too
    # result.yScale = pChild.yScale

  #echo "result children ", result.children

  var xticks = result.handleTicks(plotScales, p, akX)
  var yticks = result.handleTicks(plotScales, p, akY)
  # TODO: make it such that we don't have to do that here!
  result.updateDataScale()

  result.updateDataScale(xticks)
  result.updateDataScale(yticks)
  let grdLines = result.initGridLines(some(xticks), some(yticks))

  # given the just created plot and tick labels, have to check
  # whether we should enlarge the column / row for the y / x label and
  # move the label
  if addLabels:
    # TODO: why do we add labels to child 4 and not directly into the viewport we
    # use to provide space for it, i.e. 3?
    result.handleLabels(theme)
  result.addObj @[grdLines]

proc generateFacetPlots(view: Viewport, p: GgPlot): Viewport =
  # first perform faceting by creating subgroups
  # doAssert p.facet.isSome
  # var mplt = p
  # mplt.data = p.data.group_by(p.facet.unsafeGet.columns)
  # result = view
  # var pltSeq: seq[Viewport]
  # for (pair, df) in groups(mplt.data):
  #   mplt = p
  #   mplt.data = df
  #   var viewFacet = result
  #   # add layout within `viewFacet` to accomodate the plot as well as the header
  #   viewFacet.layout(1, 2, rowHeights = @[quant(0.1, ukRelative), quant(0.9, ukRelative)],
  #                    margin = quant(0.01, ukRelative))
  #   var headerView = viewFacet[0]
  #   # set the background of the header
  #   headerView.background()
  #   # put in the text
  #   let text = pair.mapIt($it[0] & ": " & $it[1]).join(", ")
  #   let headerText = headerView.initText(c(0.5, 0.5),
  #                                        text,
  #                                        textKind = goText,
  #                                        alignKind = taCenter,
  #                                        name = "facetHeaderText")
  #   headerView.addObj headerText
  #   headerView.name = "facetHeader"
  #   var plotView = viewFacet[1]
  #   # now add dummy plt to pltSeq
  #   plotView = plotView.generatePlot(mplt, addLabels = false)
  #   plotView.name = "facetPlot"
  #   viewFacet[0] = headerView
  #   viewFacet[1] = plotView
  #   viewFacet.name = "facet_" & text
  #   pltSeq.add viewFacet
  #
  # # now create layout in `view`, the actual canvas for all plots
  # let (rows, cols) = calcRowsColumns(0, 0, pltSeq.len)
  # result.layout(cols, rows, margin = quant(0.02, ukRelative))
  # for i, plt in pltSeq:
  #   result.children[i].objects = plt.objects
  #   result.children[i].children = plt.children
  discard

proc setInitialScale(p: GgPlot, scaleOpt: Option[Scale]): ginger.Scale =
  # TODO:  alternative could be using `isDiscrete`?
  if scaleOpt.isSome:
    # TODO: this is expensive for large columns!
    let scale = scaleOpt.unsafeGet
    let (isDiscrete, vKind) = discreteAndType(p.data, scale.col)
    if not isDiscrete:
      result = scale.dataScale
    else:
      result = (low: 0.0, high: 1.0)
    #  case vKind
    #  of VFloat, VInt:
    #    let data = p.data.dataTo(scale.col, float)
    #    let minx = data.min
    #    result = (low: data.min, high: data.max)
    #  of VString:
    #    # simply use equivalent of relative coordinates to space the categories
    #    result = (low: 0.0, high: 1.0)
    #  else: doAssert false, "unsupported!"
    #else:
    #  discard

proc customPosition(t: Theme): bool =
  ## returns true if `legendPosition` is set and thus legend sits at custom pos
  result = t.legendPosition.isSome

proc callFillScale(pData: DataFrame, scales: seq[ScaleData],
                   scKind: static ScaleKind): seq[Scale] =
  ## `pData` corresponds to the DataFrame of the `GgPlot` object. This is ``only`` (!!)
  ## used, if:
  ## - current scale is ``not`` in `GgPlot.aes`
  ## - `geom` with this scale has ``no`` `data` field
  # handle those geoms separately, which have their own data
  let separateIdxs = toSeq(0 .. scales.high).filterIt(scales[it].data.isSome)
  var scalesToUse = newSeq[Scale]()
  for i, s in scales:
    if i notin separateIdxs:
      scalesToUse.add s.scale
  if scalesToUse.len > 0:
    let filled = fillScale(pData, scalesToUse, scKind)
    for fs in filled:
      result.add fs

  # now separates
  for i in separateIdxs:
    let additional = fillScale(scales[i].data.get, @[scales[i].scale], scKind)
    doAssert additional.len <= 1
    for fs in additional:
      result.add fs #(scale: fs, ids: scales[i].ids)

proc collectScales(p: GgPlot): FilledScales =#seq[Scale] =
  ## Collects all scales required to draw the plot. This means comparing each
  ## possible aesthetic scale of the `GgPlot p` itself with its geoms and
  ## building the final `Scale` for each.
  macro collect(f: static string): untyped =
    let field = ident(f)
    result = quote do:
      block:
        var sds = newSeq[ScaleData]()
        if p.aes.`field`.isSome:
          # NOTE: the dataframe of `GgPlot` is always given individually to
          # the `fill*` procs, hence we give a `none` here
          sds.add (data: none(DataFrame), scale: p.aes.`field`.get)
        for g in p.geoms:
          if g.aes.`field`.isSome:
            sds.add (data: g.data, scale: g.aes.`field`.get)
        sds

  macro fillField(f: static string, arg: typed): untyped =
    let field = ident(f)
    let argId = ident(arg.strVal)
    result = quote do:
      if `argId`.len > 0 and `argId`[0].ids.card == 0:
        result.`field` = (main: some(`argId`[0]), more: `argId`[1 .. ^1])
      else:
        result.`field` = (main: none[Scale](), more: `argId`)

  let xs = collect("x")
  let xFilled = callFillScale(p.data, xs, scLinearData) # NOTE must not definitely be linear!
  fillField("x", xFilled)
  let ys = collect("y")
  let yFilled = callFillScale(p.data, ys, scLinearData) # NOTE must not definitely be linear!
  fillField("y", yFilled)
  let colors = collect("color")
  let colorFilled = callFillScale(p.data, colors, scColor)
  fillField("color", colorFilled)
  let sizes = collect("size")
  let sizeFilled = callFillScale(p.data, sizes, scSize)
  fillField("size", sizeFilled)
  let shapes = collect("shape")
  let shapeFilled = callFillScale(p.data, shapes, scShape)
  fillField("shape", shapeFilled)

func labelName(plotScales: FilledScales, p: GgPlot, axKind: AxisKind): string =
  ## extracts the correct label for the given axis.
  ## First checks whether the theme sets a name, then checks the name of the
  ## x / y `Scale` and finally defaults to the column name.
  # doAssert p.aes.x.isSome, "x scale should exist?"
  case axKind
  of akX:
    let xScale = getXScale(plotScales)
    if xScale.name.len > 0:
      result = xScale.name
    else:
      result = xScale.col
  of akY:
    let yScale = getYScale(plotScales)
    if yScale.name.len > 0:
      result = yScale.name
    elif yScale.col.len > 0:
      result = yScale.col
    else:
      result = "count"

#    func

proc buildTheme*(plotScales: FilledScales, p: GgPlot): Theme =
  ## builds the final theme used for the plot. It takes the theme of the
  ## `GgPlot` object and fills in all missing fields as required from
  ## `plotScales` and `p`.
  result = p.theme
  if result.xLabel.isNone:
    result.xLabel = some(labelName(plotScales, p, akX))
  if result.yLabel.isNone:
    result.yLabel = some(labelName(plotScales, p, akY))
  if result.xLabelSecondary.isNone and plotScales.hasSecondary(akX):
    result.xLabelSecondary = some(plotScales.getSecondaryAxis(akX).name)
  if result.yLabelSecondary.isNone and plotScales.hasSecondary(akY):
    result.yLabelSecondary = some(plotScales.getSecondaryAxis(akY).name)

proc ggcreate*(p: GgPlot, width = 640.0, height = 480.0): PlotView =
  ## applies all calculations to the `GgPlot` object required to draw
  ## the plot with cairo and returns a `PlotView`. The `PlotView` contains
  ## the final `Scales` built from the `GgPlot` object and all its geoms
  ## plus the ginal ginger.Viewport which only has to be drawn to produce the
  ## plot.
  ## This proc is useful to investigate the final Scales or the Viewport
  ## that will actually be drawn.
  let plotScales = collectScales(p)

  let theme = buildTheme(plotScales, p)

  # TODO: this probably doesn't have to happen here!
  #let (xAes, yAes) = getXYAes(p, p.geoms[0])

  # create the plot
  var img = initViewport(#xScale = some(xScale),
                         #yScale = some(yScale),
                         name = "root",
                         wImg = width,
                         hImg = height)


  # NOTE: the question if ``not`` whether a `PLOT` requires a legend
  # but rather whether an `AESTHETIC` with an attached `SCALE` requires
  # legend! So check for each `aes` whether we have to draw a legend!
  # Assembly of the plot layout thus has to happen at the very end
  let drawLegend = plotScales.requiresLegend
  if drawLegend:
    img.plotLayoutWithLegend()
  else:
    img.plotLayoutWithoutLegend()
  # get viewport of plot
  var pltBase = img[4]

  if p.facet.isSome:
    pltBase = pltBase.generateFacetPlots(p)
    # TODO :clean labels up, combine with handleLabels!
    # Have to consider what should happen for that though.
    # Need flag to disable auto subtraction, because we don't have space or
    # rather if done needs to be done on all subplots?
    echo "XLABEL ", theme.xLabel.unwrap()
    echo "YLABEL ", theme.yLabel.unwrap()
    let xlabel = pltBase.xlabel(theme.xLabel.unwrap())#p.aes.x.get.col)
    let ylabel = pltBase.ylabel(theme.yLabel.unwrap())#p.aes.x.get.col)
    pltBase.addObj @[xlabel, ylabel]
  else:
    pltBase = pltBase.generatePlot(p, plotScales, theme)

  let xScale = pltBase.xScale#plotScales.x.main.get.dataScale #setInitialScale(p, xAes.x)
  let yScale = pltBase.yScale#plotScales.y.main.get.dataScale#setInitialScale(p, yAes.y)
  img[4] = pltBase
  img.xScale = xScale
  img.yScale = yScale
  #img.updateDataScale()

  # possibly correct the yScale assigned to the root Viewport
  img.yScale = pltBase.yScale

  # draw legends
  # store each type of drawn legend. only one type for each kind
  var drawnLegends = initHashSet[(DiscreteKind, ScaleKind)]()
  for scale in enumerateScalesByIds(plotScales):
    if scale.scKind notin {scLinearData, scTransformedData} and
       (scale.dcKind, scale.scKind) notin drawnLegends:
      # handle color legend
      var lg = img[5]
      let markers = lg.generateLegendMarkers(scale)
      # TODO: The following currently creates stacked legends for each Scale that
      # requires one. Need to create a `seq[Viewport]` or something to first build
      # all legends and then calculate the sizes required.
      # set height to number of markers + 1 centimeter
      lg.height = quant((markers.len + 1).float, ukCentimeter)
      if customPosition(p.theme):
        let pos = p.theme.legendPosition.get
        lg.origin.x = pos.x
        lg.origin.y = pos.y
      else:
        lg.origin.y = lg.origin.y + c1(img[4].height.val / 8.0)
        lg.origin.x = lg.origin.x + img.c1(0.5, akX, ukCentimeter)
      lg.createLegend(scale, markers)
      img[5] = lg
      drawnLegends.incl (scale.dcKind, scale.scKind)

  if p.title.len > 0:
    var titleView = img[1]
    let font = Font(family: "sans-serif",
                    size: 16.0,
                    color: black)
    let title = titleView.initText(c(0.0, 0.5),
                                   p.title,
                                   textKind = goText,
                                   alignKind = taLeft,
                                   font = some(font))
    titleView.addObj title
    img[1] = titleView

  result.plotScales = plotScales
  result.view = img

proc ggdraw*(view: Viewport, fname: string) =
  ## draws the given viewport and stores it in `fname`.
  ## It assumes that the `view` was created from a `GgPlot` object with
  ## `ggcreate`
  view.draw(fname)

proc ggsave*(p: GgPlot, fname: string, width = 640.0, height = 480.0) =
  let plt = p.ggcreate(width = width, height = height)
  plt.view.ggdraw(fname)

proc ggsave*(fname: string, width = 640.0, height = 480.0): Draw =
  Draw(fname: fname,
       width: some(width),
       height: some(height))

proc `+`*(p: GgPlot, d: Draw) =
  if d.width.isSome and d.height.isSome:
    p.ggsave(d.fname,
             width = d.width.get,
             height = d.height.get)
  else:
    p.ggsave(d.fname)

proc ggvega*(): VegaDraw = VegaDraw()

from json import nil
proc `+`*(p: GgPlot, d: VegaDraw): json.JsonNode =
  p.toVegaLite()

proc countLines(s: var FileStream): int =
  ## quickly counts the number of lines and then resets stream to beginning
  ## of file
  var buf = newString(500)
  while s.readLine(buf):
    inc result
  s.setPosition(0)

proc readCsv*(fname: string,
              sep = ',',
              header = "#",
              skipLines = 0): OrderedTable[string, seq[string]] =
  ## returns a CSV file as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  ## The `header` field can be used to designate the symbol used to
  ## differentiate the `header`. By default `#`.
  var s = newFileStream(fname, fmRead)
  if s == nil:
    quit("cannot open the file" & fname)

  let lineCount = countLines(s)

  var parser: CsvParser
  open(parser, s, fname, separator = sep, skipInitialSpace = true)
  var isHeader = true
  parser.readHeaderRow()
  result = initOrderedTable[string, seq[string]]()
  # filter out the header, delimiter, if any
  parser.headers.keepItIf(it != header)
  var colHeaders: seq[string]
  for colUnstripped in items(parser.headers):
    let col = colUnstripped.strip
    colHeaders.add col
    result[col] = newSeqOfCap[string](lineCount)
  var lnCount = 0
  while readRow(parser):
    if lnCount < skipLines:
      inc lnCount
      continue
    for i, col in parser.headers:
      parser.rowEntry(col).removePrefix({' '})
      parser.rowEntry(col).removeSuffix({' '})
      result[colHeaders[i]].add parser.rowEntry(col)

#when isMainModule:
#  let mpg = toDf(readCsv("data/mpg.csv"))
#  let plt = ggplot(mpg, aes(x = "displ", y = "hwy")) +
#    geom_point()
#  plt.ggsave("scatter.pdf")
#
#  mpg.filter(f{"class" == "suv"}) # comparison via `f{}` macro
#    .mutate(ratioHwyToCity ~ hwy / cty # raw untyped template function definition
#    ) # <- note that we have to use normal UFCS to hand to `ggplot`!
#    .ggplot(aes(x = "ratioHwyToCity", y = "displ", color = "class")) +
#    geom_point() +
#    ggsave("scatterFromDf.pdf")
#
#  #let re = df.mutate(f{"cty_norm" ~ "cty" / abs("hwy")})
#            # f{"displ_ccm" ~ "displ" * "1000.0"}, # displacement in ccm
#            # unfortunately this is not yet possible. The `FormulaNode.fkVariable`
#            # needs to be converted to type Value before we can do that
#
#  let f = f{"cty_norm" ~ "cty" / mean("cty") * 2.0}
#  echo "Func ", f
#
#  let val = 1000
#  let key = "cty"
#  mpg.mutate(f{"cty_norm" ~ "cty" / mean(key) * val})
#            # f{"displ_ccm" ~ "displ" * "1000.0"}, # displacement in ccm
#            # unfortunately this is not yet possible. The `FormulaNode.fkVariable`
#            # needs to be converted to type Value before we can do that
#    .ggplot(aes(x = "displ", y = "cty_norm", color = "class")) +
#    geom_point() +
#    ggsave("classVsNormCty.pdf")
#
#  ggplot(mpg, aes(x = "displ", y = "cty", color = "class")) +
#    geom_point() +
#    ggtitle("ggplotnim - or I Suck At Naming Things™") +
#    ggsave("scatterColor.pdf")
#
#  ggplot(mpg, aes("hwy")) +
#    geom_histogram() +
#    ggsave("simpleHisto.pdf")
#
#  ggplot(mpg, aes("hwy")) +
#    geom_freqpoly() +
#    ggsave("freqpoly.pdf")
#
#  ggplot(mpg, aes("hwy")) +
#    geom_histogram() + # the order of the geom calls decides the order in which
#                       # they are drawn! FreqPoly will be drawn on top of histo
#    geom_freqpoly(color = parseHex("FD971F"),
#                  size = 3.0) +
#    ggsave("histoPlusFreqpoly.pdf")
#
#  ggplot(mpg, aes(x = "class")) +
#    geom_bar() +
#    ggsave("bar_plot.pdf")


  # we don't parse `FormulaNode` in `aes` arguments yet
  #ggplot(mpg, aes(year ~ (displ * hwy + cty), color = "class")) +
  #  geom_point() +
  #  ggtitle("ggplotnim - or I Suck At Naming Things™") +
  #  ggsave("scatterColor.pdf")
