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

iterator enumerateScalesByIds(filledScales: FilledScales): Scale =
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

iterator enumerateScales(filledScales: FilledScales, geom: Geom): Scale =
  ## Yields all scales, which are allowed for the given geom
  var yieldedSet = initHashSet[Scale]()
  for s in enumerateScalesByIds(filledScales):
    if geom.gid in s.ids and s notin yieldedSet:
      yieldedSet.incl s
      yield s

iterator enumerateData(geom: FilledGeom): (seq[Style], DataFrame) =
  ## yields the pairs of continuous styles for the current discrete style and
  ## its data from `yieldData`
  for (style, df) in values(geom.yieldData):
    yield (style, df)

iterator reversedEnumerateData(geom: FilledGeom): (seq[Style], DataFrame) =
  ## yields the pairs of continuous styles for the current discrete style and
  ## its data from `yieldData` in reversed form
  let revKeys = toSeq(keys(geom.yieldData)).reversed
  for k in revKeys:
    yield geom.yieldData[k]

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
  for i, k in result.labelSeq:
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
  df: DataFrame,
  trans: Option[ScaleTransform] = none[ScaleTransform]()
     ): Scale =
  result = Scale(scKind: scKind, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq.sortedByIt(it)
  result.valueMap = initOrderedTable[Value, ScaleValue]()
  result.axKind = axKind
  if scKind == scTransformedData:
    ## we make  sure `trans` is some in the calling scope!
    result.trans = trans.get

proc fillContinuousLinearScale(col: string, axKind: AxisKind, vKind: ValueKind,
                               dataScale: ginger.Scale,
                               df: DataFrame): Scale =
  result = Scale(scKind: scLinearData, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  result.axKind = axKind
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)
      else: idxs = idxsIn
      result = idxs.mapIt(ScaleValue(kind: scLinearData, val: df[col][it]))
  )

proc fillContinuousTransformedScale(col: string,
                                    axKind: AxisKind,
                                    vKind: ValueKind,
                                    trans: ScaleTransform,
                                    dataScale: ginger.Scale,
                                    df: DataFrame): Scale =
  result = Scale(scKind: scTransformedData, vKind: vKind, col: col,
                 dcKind: dcContinuous,
                 dataScale: dataScale)
  result.axKind = axKind
  result.trans = trans
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)
      else: idxs = idxsIn
      result = idxs.mapIt(ScaleValue(kind: scTransformedData,
                                     val: trans(df[col][it])))
  )

proc fillContinuousColorScale(scKind: static ScaleKind,
                              col: string,
                              vKind: ValueKind,
                              dataScale: ginger.Scale,
                              df: DataFrame): Scale =
  ## devise colormap mapping
  result = Scale(scKind: scKind, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  # for now just take viridis as default
  # map all values to values between 0-255 and get the correct idx of viridis map
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)
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
  const minSize = 2.0
  const maxSize = 7.0
  result = Scale(scKind: scSize, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  result.mapData = (
    proc(idxsIn: seq[int] = @[]): seq[ScaleValue] =
      var idxs: seq[int]
      if idxsIn.len == 0: idxs = toSeq(0 .. df.high)
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
  df: DataFrame,
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

proc getIdentityData(df: DataFrame, col, name: string): DataFrame =
  if col in df:
    result = df.select(f{name ~ col})
  else:
    let d = @[Value(kind: VString, str: col)]
    result = seqsToDf({name : d})

proc fillScale(df: DataFrame, scales: seq[Scale],
               scKind: static ScaleKind): seq[Scale] =
  # NOTE: `rawCol` is used to build a DF of all data of the given scales. Be aware
  # that all scales given here belong to the same `aes` field, i.e. the same
  # "axis" (x, y, color,...) and thus can be considered compatible and part of the
  # same scale / classes! The actual data given to each filled scale however is not
  # this DF, but rather the input `df.select(s.col)`, see below.
  const rawCol = "data"
  # get the data column we scale by
  var data: DataFrame #newSeqOfCap[Value](df.len * scales.len)
  var transOpt: Option[ScaleTransform]
  var axKindOpt: Option[AxisKind]
  # in a first loop over the scales read the data required to make decisions about
  # the appearence of the resulting scale
  for s in scales:
    # add this scales dasta to `data` DF for deduction of labels / data scales
    data.add getIdentityData(df, s.col, rawCol)
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

    # now determine labels, data scale from `data`
    let (isDiscrete, vKind) = discreteAndType(data, rawCol)
    if vKind == VNull:
      echo "WARNING: Unexpected data type VNull of column: ", s.col, "!"
      continue

    if isDiscrete:
      labelSeqOpt = some(data[rawCol].unique.sorted)
    else:
      dataScaleOpt = some((low: min(data[rawCol]).toFloat,
                           high: max(data[rawCol]).toFloat))

    # now have to call `fillScaleImpl` with this information
    # note that data given to proc is a DF of only this scales column
    var filled = fillScaleImpl(vKind, isDiscrete, s.col, df.select(s.col), scKind,
                               labelSeqOpt, dataScaleOpt,
                               axKindOpt, transOpt)
    if scKind in {scLinearData, scTransformedData}:
      filled.secondaryAxis = s.secondaryAxis
    filled.ids = s.ids
    result.add filled

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
  fillIt(result.fill)
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

template assignBinFields(res: var Geom, stKind, bins,
                         binWidth, breaks: untyped): untyped =
  case stKind
  of stBin:
    if breaks.len > 0:
      result.binEdges = some(breaks)
    if binWidth > 0.0:
      result.binWidth = some(binWidth)
    if bins > 0:
      result.numBins = bins
  else: discard

proc geom_point*(aes: Aesthetics = aes(),
                 data = DataFrame(),
                 color: Color = black,
                 size: float = 3.0,
                 stat = "identity",
                 bins = -1,
                 binWidth = 0.0,
                 breaks: seq[float] = @[],
                 binPosition = "none"
                ): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkPoint,
                style: some(Style(color: color,
                                  size: size)),
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks)

proc geom_bar*(aes: Aesthetics = aes(),
               color: Color = grey20, # color of the bars
               position = "stack",
               stat = "count",
              ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
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
                position: pkKind,
                binPosition: bpNone,
                statKind: stKind)

proc geom_line*(aes: Aesthetics = aes(),
                data = DataFrame(),
                color: Color = grey20,
                size: float = 1.0,
                lineType: LineType = ltSolid,
                stat = "identity",
                bins = -1,
                binWidth = 0.0,
                breaks: seq[float] = @[],
                binPosition = "none",
               ): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkLine,
                style: some(Style(color: color,
                                  lineWidth: size,
                                  lineType: lineType,
                                  fillColor: transparent)),
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks)

proc geom_histogram*(aes: Aesthetics = aes(),
                     binWidth = 0.0, bins = 30,
                     breaks: seq[float] = @[],
                     color: Color = grey20, # color of the bars
                     position = "stack",
                     stat = "bin",
                     binPosition = "left",
                    ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let style = Style(lineType: ltSolid,
                    lineWidth: 0.2, # draw 1 pt wide black line to avoid white pixels
                                    # between bins at size of exactly 1.0 bin width
                    color: color, # default color
                    fillColor: color)
  let gid = incId()
  result = Geom(gid: gid,
                kind: gkHistogram,
                aes: aes.fillIds({gid}),
                style: some(style),
                position: pkKind,
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks)

proc geom_freqpoly*(aes: Aesthetics = aes(),
                    color: Color = grey20, # color of the line
                    size: float = 1.0, # line width of the line
                    lineType: LineType = ltSolid,
                    bins = 30,
                    binWidth = 0.0,
                    breaks: seq[float] = @[],
                    position = "identity",
                    stat = "bin",
                    binPosition = "center"
                   ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let style = Style(lineType: lineType,
                    lineWidth: size,
                    color: color,
                    fillColor: transparent,)
  let gid = incId()
  result = Geom(gid: gid,
                kind: gkFreqPoly,
                aes: aes.fillIds({gid}),
                style: some(style),
                position: pkKind,
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks)


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
  result.geoms.add geom

proc `+`*(p: GgPlot, facet: Facet): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.facet = some(facet)

proc `+`*(p: GgPlot, aes: Aesthetics): GgPlot =
  ## adds the given aesthetics to the GgPlot object
  result = p
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
        mscale.col = aes.x.get.col
        mscale.ids = aes.x.get.ids
        result.x = some(mscale)
    of akY:
      if aes.y.isSome:
        mscale.col = aes.y.get.col
        mscale.ids = aes.y.get.ids
        result.y = some(mscale)
  of scColor:
    mscale.ids = aes.color.get.ids
    result.color = some(mscale)
  of scFillColor:
    mscale.ids = aes.fill.get.ids
    result.fill = some(mscale)
  of scSize:
    mscale.ids = aes.size.get.ids
    result.size = some(mscale)
  of scShape:
    mscale.ids = aes.shape.get.ids
    result.shape = some(mscale)

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

proc requiresLegend(filledScales: FilledScales): bool =
  ## returns true if the plot requires a legend to be drawn
  if anyScale(filledScales.color) or
     anyScale(filledScales.fill) or
     anyScale(filledScales.size) or
     anyScale(filledScales.shape):
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
  of scShape:
    result.marker = scVal.marker
  else:
    raise newException(Exception, "Setting style of " & $scVal.kind & " not " &
      "supported at the moment!")

proc applyStyle(style: var Style, df: DataFrame, scales: seq[Scale], keys: seq[(string, Value)]) =
  var styleVal: ScaleValue
  for (col, val) in keys:
    for s in scales:
      # walk all scales and build the correct style
      case s.dcKind
      of dcDiscrete:
        if col notin df:
          # constant value
          styleVal = s.getValue(%~ s.col)
        else:
          styleVal = s.getValue(val)
        style = changeStyle(style, styleVal)
      else:

        discard

iterator markerStylePairs(df: DataFrame, filledScales: FilledScales, geom: Geom): (int, Style) =
  ## iterates all scales relevant for `p` and `geom` and yields the
  ## the `Style` required for ``each datapoint`` as well as the current index.
  var style = geom.style.unsafeGet
  # first collect all scales we have to consider for this geom
  var scales: seq[Scale]
  for scale in enumerateScales(filledScales, geom):
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
        lStyle = changeStyle(lStyle, val)
      else:
        # get the `i`-th element from the data
        val = s.mapData(@[i])[0]
        case val.kind
        of scLinearData, scTransformedData:
          continue
        else:
          lStyle = changeStyle(lStyle, val)
    yield (i, lStyle)

iterator enumerateStyles(df: DataFrame, scales: seq[Scale], geom: Geom): (int, Style) =
  #df: DataFrame, filledScales: FilledScales, geom: Geom): (int, Style) =
  ## iterates all scales relevant for `p` and `geom` and yields the
  ## the `Style` required for ``each datapoint`` as well as the current index.
  var style = geom.style.unsafeGet
  # first collect all scales we have to consider for this geom
  # then walk data frame and extracting correct style for each
  var lStyle: Style
  var val: ScaleValue
  for i in 0 ..< df.len:
    for s in scales:
      # walk all scales and build the correct style
      case s.dcKind
      of dcDiscrete:
        if s.col notin df:
          # constant value
          val = s.getValue(%~ s.col)
        else:
          val = s.getValue(df[s.col][i])
        lStyle = changeStyle(lStyle, val)
      else:
        # get the `i`-th element from the data
        val = s.mapData(@[i])[0]
        case val.kind
        of scLinearData, scTransformedData:
          continue
        else:
          lStyle = changeStyle(lStyle, val)
    yield (i, lStyle)

iterator markerStyles(p: GgPlot, geom: Geom): (MarkerKind, Style) =
  ## iterates all scales relevant for `p` and `geom` and yields the
  ## `MarkerKind` and the `Style` required for ``each datapoint``.
  for _, markerStyle in markerStyles(p, geom):
    yield markerStyle

macro genGetScale(field: untyped): untyped =
  let name = ident("get" & $field.strVal & "Scale")
  result = quote do:
    proc `name`(filledScales: FilledScales, geom = Geom(gid: 0)): Scale =
      if filledScales.`field`.main.isSome:
        # use main
        result = filledScales.`field`.main.get
      else:
        # find scale matching `gid`
        for s in filledScales.`field`.more:
          if geom.gid == 0 or geom.gid in s.ids:
            return s

genGetScale(x)
genGetScale(y)
genGetScale(color)
genGetScale(size)
genGetScale(shape)

proc addHistoRect[T](view: var Viewport, val: T, style: Style,
                     yPos: Coord1D = c1(1.0),
                     width = 1.0) =
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

proc addHistoCentered[T](view: var Viewport, val: T, style: Style,
                         yPos: Coord1D = c1(1.0),
                               width = 1.0): GraphObject =
  ## creates a rectangle for a histogram and adds it to the viewports object
  # TODO: replace width argument by float range, so we
  # only allow values [0.0..1.0]
  if val.float > 0.0:
    # calc left side of bar based on width, since we wa t the bar to be centered
    let left = (1.0 - width) / 2.0
    view.addObj view.initRect(Coord(x: c1(left),
                                y: yPos), # bottom left
                          quant(width, ukRelative),
                          quant(-val.float, ukData),
                          style = some(style))
    #view.addObj result

proc addPointCentered[T](view: var Viewport, val: T, style: Style): GraphObject =
  ## creates a rectangle for a histogram and adds it to the viewports object
  if val.float > 0.0:
    # TODO: dispatch on discrete axis!
    view.addObj initPoint(view,
                          pos = Coord(
                            x: c1(0.5, ukRelative),
                            y: Coord1D(pos: val, kind: ukData,
                                       axis: akY,
                                       scale: view.yScale)),
                          marker = style.marker,
                          color = style.color,
                          size = style.size)

proc getDiscreteData(view: var Viewport,
                     fg: FilledGeom,
                     theme: Theme):
                       tuple[data: OrderedTable[int, (seq[float], Style)],
                             toIgnore: HashSet[int]] =
  ## creates the GraphObjects required for a bar plot
  let numElements = fg.numX
  let toIgnore = toSet([0, numElements + 1])
  var labData = initOrderedTable[int, (seq[float], Style)]()
  var numLabel = 0
  for (styles, subDf) in enumerateData(fg):
    let counts = dataTo(subDf, fg.ycol, float)
    if styles.len == 1:
      let style = styles[0]
      labData[numLabel] = (counts, style)
    else:
      # what's this supposed to be? continuously colored bins?
      raise newException(Exception, "Does this make sense?")
    inc numLabel
  # reverse the order of `labData`, so that the element class with highest string
  # value is located at the bottom of the histogram (to match `ggplot2`)
  labData.sort(
    cmp = (
      proc(a, b: (int, (seq[float], Style))): int =
        result = system.cmp(a[0], b[0])
    ),
    order = SortOrder.Descending)
  result = (data: labData, toIgnore: toIgnore)

proc addGeomCentered(view: var Viewport,
                     fg: FilledGeom): seq[GraphObject] =
  ## given N(xM soon) viewports, will add the `data` at index `i` for viewport
  ## `i` in the center using the given GeomKind
  doAssert fg.dcKindX == dcDiscrete or fg.dcKindY == dcDiscrete, "at least one axis must be discrete!"
  # TODO: can both be discrete? Yes.
  # TODO: can identity and stack be unified?
  let (data, toIgnore) = getDiscreteData(view, fg, Theme())
  var i = 0
  var idx = 0
  for p in mitems(view):
    if i in toIgnore:
      inc i
      continue
    case fg.geom.position
    of pkIdentity:
      for label, (val, style) in data:
        case fg.geom.kind
        of gkBar:
          result.add p.addHistoCentered(val[idx], style, width = 0.8) # geom.barWidth
        of gkPoint:
          result.add p.addPointCentered(val[idx], style)
        of gkLine:
          raise newException(Exception, "Need two points for line!")
        else:
          raise newException(Exception, "Implement me: " & $fg.geom.kind)
    of pkStack:
      # create one rectangle for each label, each successive starting at the
      # top of the previous
      var prevTop = c1(1.0)
      var prevVal = 0.0
      for label, (val, style) in data:
        case fg.geom.kind
        of gkBar:
          result.add p.addHistoCentered(val[idx], style, prevTop, width = 0.8) # geom.barWidth
        of gkPoint:
          result.add p.addPointCentered(val[idx] + prevVal, style)
        else:
          raise newException(Exception, "Implement me: " & $fg.geom.kind)
        prevVal = val[idx]
        prevTop = prevTop - Coord1D(pos: fg.yScale.high - val[idx].float, kind: ukData,
                                    scale: fg.yScale, axis: akY)
    of pkDodge:
      raise newException(Exception, "Not implemented yet :)")
    of pkFill:
      raise newException(Exception, "Not implemented yet :)")
    inc i
    inc idx

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

func readOrCalcBinWidth(df: DataFrame, idx: int,
                        dataCol: string,
                        col = "binWidths"): float =
  ## either reads the bin width from the DF for element at `idx`
  ## from the bin widths `col` or calculates it from the distance
  ## to the next bin in the `dataCol`.
  ## NOTE: Cannot be calculated for the last element of the DataFrame
  ## DataFrame. So make sure the DF contains the right bin edge
  ## (we assume bins are actually left edge) is included in the DF.
  if col in df:
    result = df[col][idx].toFloat
  elif idx < df.high:
    result = (df[dataCol][idx + 1].toFloat - df[dataCol][idx].toFloat)

proc moveBinPosition(x: var float, bpKind: BinPositionKind, binWidth: float) =
  ## moves `x` by half the bin width, if required by `bpKind`
  case bpKind
  of bpLeft, bpNone:
    # bpLeft requires no change, since bins are assumed to be left edge based
    # or bpNone if data is not bin like
    discard
  of bpCenter:
    # since our data is given as `bpLeft`, move half to right
    x = x + binWidth / 2.0
  of bpRight:
    x = x + binWidth

proc identityDraw[T: Style | seq[Style]](view: var Viewport,
                                         fg: FilledGeom,
                                         styleIn: T,
                                         df: DataFrame): seq[GraphObject] =
  # TODO: add support for decision what bin columns means (for results of
  # statBin that is! Left edge, center or right edge!
  # needed for gkLine, gkPolyLine
  var linePoints = newSeqOfCap[(float, float)](df.len)
  # needed for histogram
  var binWidth: float
  for i in 0 ..< df.len:
    when T is Style:
      let style = styleIn
    else:
      let style = styleIn[i]
    var x = df[fg.xcol][i].toFloat
    let y = df[fg.ycol][i].toFloat
    binWidth = readOrCalcBinWidth(df, i, fg.xcol)
    # potentially move `x` by half of the `binWidth`
    x.moveBinPosition(fg.geom.binPosition, binWidth)
    case fg.geom.kind
    of gkPoint:
      result.add initPoint(view, (x: x, y: y),
                           marker = style.marker,
                           color = style.color,
                           size = style.size)
    of gkHistogram:
      let xPos = x # assumes bins are left edges
      let rect = view.initRect(Coord(x: Coord1D(pos: xPos, kind: ukData,
                                                axis: akX, scale: fg.xScale),
                                     y: c1(1.0)),
                                quant(binWidth, ukData),
                                quant(-y, ukData),
                                style = some(style))
      result.add rect
    of gkLine, gkFreqPoly:
      # have to accumulate the data first before we draw it
      linePoints.add (x: x, y: y)
    else:
      raise newException(Exception, "I'm not implemented yet in identityDraw: " & $fg.geom.kind)
  # for `gkLine`, `gkFreqPoly` now draw the lines
  if fg.geom.kind in {gkLine, gkFreqPoly}:
    when T is Style:
      result.add view.initPolyLine(linePoints, some(styleIn))
    else:
      # since `ginger` doesn't support gradients on lines atm, we just draw from
      # `(x1/y1)` to `(x2/y2)` with the style of `(x1/x2)`. We could build the average
      # of styles between the two, but we don't atm!
      echo "WARNING: using non-gradient drawing of line with multiple colors!"
      for i in 0 ..< styleIn.high: # last element covered by i + 1
        result.add view.initPolyLine(@[linePoints[i], linePoints[i+1]], some(styleIn[i]))


proc stackDraw[T: Style | seq[Style]](view: var Viewport,
                                      prevVals: var seq[float],
                                      fg: FilledGeom,
                                      styleIn: T,
                                      df: DataFrame): seq[GraphObject] =
  # TODO: add support for decision what bin columns means (for results of
  # TODO: add support for stacking in X rather than Y
  # TODO: unify with identityDraw? Lots of similar code!
  # statBin that is! Left edge, center or right edge!
  # needed for gkLine, gkPolyLine
  var linePoints = newSeqOfCap[Point](df.len)
  var binWidth: float
  for i in 0 ..< df.len:
    when T is Style:
      let style = styleIn
    else:
      let style = styleIn[i]
    var x = df[fg.xcol][i].toFloat
    let y = df[fg.ycol][i].toFloat
    binWidth = readOrCalcBinWidth(df, i, fg.xcol)
    # potentially move `x` by half of the `binWidth`
    x.moveBinPosition(fg.geom.binPosition, binWidth)
    case fg.geom.kind
    of gkPoint:
      result.add initPoint(view, (x: x, y: y + prevVals[i]), # TODO: is + prevals correct
                           marker = style.marker,
                           color = style.color,
                           size = style.size)
    of gkHistogram:
      let newypos = c1(1.0) - Coord1D(pos: fg.yScale.high - prevVals[i], kind: ukData,
                                      axis: akY, scale: fg.yScale)
      let rect = view.initRect(Coord(x: Coord1D(pos: x, kind: ukData,
                                                axis: akX, scale: fg.xScale),
                                     y: newypos),
                                quant(binWidth, ukData),
                                quant(-y.float, ukData),
                                style = some(style))
      result.add rect
    of gkLine, gkFreqPoly:
      # have to accumulate the data first before we draw it
      linePoints.add (x: x, y: y)
    else:
      raise newException(Exception, "I'm not implemented yet in stackDraw: " & $fg.geom.kind)
    # now update the previous values
    prevVals[i] += y
  # for `gkLine`, `gkFreqPoly` now draw the lines
  if fg.geom.kind in {gkLine, gkFreqPoly}:
    when T is Style:
      #result.add view.initPolyLine(linePoints, some(styles[0]))
      result.add view.drawStackedPolyLine(prevVals, linePoints, styleIn)
    else:
      # since `ginger` doesn't support gradients on lines atm, we just draw from
      # `(x1/y1)` to `(x2/y2)` with the style of `(x1/x2)`. We could build the average
      # of styles between the two, but we don't atm!
      echo "WARNING: using non-gradient drawing of line with multiple colors!"
      if fg.geom.kind == gkFreqPoly:
        echo "WARNING: probably doing something weird right now drawing gkFreqPoly!"
      for i in 0 ..< styleIn.high: # last element covered by i + 1
        let start = (x: linePoints[i].x, y: linePoints[i].y + prevVals[i])
        let stop = (x: linePoints[i + 1].x, y: linePoints[i + 1].y + prevVals[i + 1])
        result.add view.initPolyLine(@[start, stop],
                                     some(styleIn[i]))

template colsRows(fg: FilledGeom): (int, int) =
  var
    cols = 1
    rows = 1
  if fg.dcKindX == dcDiscrete:
    cols = fg.numX
  if fg.dcKindY == dcDiscrete:
    rows = fg.numY
  (cols, rows)

proc prepareViews(view: var Viewport, fg: FilledGeom, theme: Theme) =
  ## prepares the required viewports in `view` for `fg` to be drawn in
  ## In each axis x,y will create N children viewports for the number
  ## of discrete labels along that axis. For continuous data no further
  ## children are created.
  let (cols, rows) = colsRows(fg)
  # view.layout(cols, rows) # TODO: extend for discrete rows
  let discrMarginOpt = theme.discreteScaleMargin
  var discrMargin = quant(0.0, ukRelative)
  if discrMarginOpt.isSome:
    discrMargin = discrMarginOpt.unsafeGet
  let indWidths = toSeq(0 ..< cols * rows).mapIt(quant(0.0, ukRelative))
  view.layout(cols * rows + 2, 1,
              colwidths = concat(@[discrMargin],
                                 indWidths,
                                 @[discrMargin]))

proc createGobjFromGeom(view: var Viewport,
                        fg: FilledGeom,
                        theme: Theme): seq[GraphObject] =
  ## performs the required conversion of the data from the data
  ## frame according to the given `geom`
  view.prepareViews(fg, theme)
  # TODO: some geoms are by (our) definition discrete, e.g. geom_bar!
  # should we case on those first?
  case fg.dcKindX
  of dcDiscrete:
    # draw discrete along x
    # doAssert fg.dcKindY == dcContinuous
    # TODO: make sure all procs return correct stuff...!
    result = view.addGeomCentered(fg)
  of dcContinuous:
    # draw continuous both axes
    case fg.geom.position
    of pkIdentity:
      for (styles, subDf) in enumerateData(fg):
        if styles.len == 1:
          result.add view.identityDraw(fg, styles[0], subDf)
        else:
          result.add view.identityDraw(fg, styles, subDf)
    of pkStack:
      # yield data in reversed order, so that "higehst value" appears at bottom
      var prevVals = newSeq[float](fg.numX)
      for (styles, subDf) in reversedEnumerateData(fg):
        if styles.len == 1:
          result.add view.stackDraw(prevVals, fg, styles[0], subDf)
        else:
          result.add view.stackDraw(prevVals, fg, styles, subDf)
    else:
      raise newException(Exception, $fg.geom.position & " not implemented yet. :)")

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

func getSecondaryAxis(filledScales: FilledScales, axKind: AxisKind): SecondaryAxis =
  ## Assumes a secondary axis must exist!
  case axKind
  of akX:
    let xScale = filledScales.getXScale()
    result = xScale.secondaryAxis.unwrap()
  of akY:
    let yScale = filledScales.getYScale()
    result = yScale.secondaryAxis.unwrap()

func hasSecondary(filledScales: FilledScales, axKind: AxisKind): bool =
  case axKind
  of akX:
    let xScale = filledScales.getXScale()
    if xScale.secondaryAxis.isSome:
      result = true
  of akY:
    let yScale = filledScales.getYScale()
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
    let scale = (low: 0.0, high: 1.0)
    tickLocs.add Coord1D(pos: pos,
                         kind: ukRelative)
  let (tickObjs, labObjs) = view.tickLabels(tickLocs, tickLabels, axKind)
  view.addObj concat(tickObjs, labObjs)
  result = tickObjs

proc handleTicks(view: var Viewport, filledScales: FilledScales, p: GgPlot,
                 axKind: AxisKind): seq[GraphObject] =
  ## This handles the creation of the tick positions and tick labels.
  ## It automatically updates the x and y scales of both the viewport and the `filledScales`!
  var scale: Scale
  var numTicks: int
  case axKind
  of akX:
    scale = filledScales.getXScale()
    numTicks = p.numXTicks
  of akY:
    scale = filledScales.getYScale()
    numTicks = p.numYTicks
  if scale.col.len > 0:
    case scale.dcKind
    of dcDiscrete:
      result = view.handleDiscreteTicks(p, axKind, scale)
      if hasSecondary(filledScales, axKind):
        let secAxis = filledScales.getSecondaryAxis(axKind)
        result.add view.handleDiscreteTicks(p, axKind, scale, isSecondary = true)
    of dcContinuous:
      result = view.handleContinuousTicks(p, axKind, scale, numTicks)
      if hasSecondary(filledScales, axKind):
        let secAxis = filledScales.getSecondaryAxis(axKind)
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

proc handleLabels(view: var Viewport, theme: Theme) =
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
    xlabTxt = theme.xLabel.unwrap()
    ylabTxt = theme.yLabel.unwrap()
  template getMargin(marginVar, themeField, nameVal, axKind: untyped): untyped =
    if not themeField.isSome:
      let labs = view.objects.filterIt(it.name == nameVal)
      let labNames = labs.mapIt(it.txtText)
      let labLens = labNames.argMaxIt(len(it))
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
                      isSecondary = isSecond)

  getMargin(xMargin, theme.xlabelMargin, "xtickLabel", akX)
  getMargin(yMargin, theme.ylabelMargin, "ytickLabel", akY)
  createLabel(yLabObj, ylabel, yLabTxt, theme.yLabelMargin, yMargin)
  createLabel(xLabObj, xlabel, xLabTxt, theme.xLabelMargin, xMargin)
  view.addObj @[xLabObj, yLabObj]

  if theme.hasSecondary(akX):
    let secAxisLabel = theme.xLabelSecondary.unwrap()
    var labSec: GraphObject
    createLabel(labSec, xlabel, secAxisLabel, theme.yLabelMargin, 0.0,
                true)
    view.addObj @[labSec]
  if theme.hasSecondary(akY):#p, akY):
    let secAxisLabel = theme.yLabelSecondary.unwrap()
    var labSec: GraphObject
    createLabel(labSec, ylabel, secAxisLabel, theme.yLabelMargin, 0.0,
                true)
    view.addObj @[labSec]

proc generatePlot(view: Viewport, p: GgPlot, filledScales: FilledScales,
                  theme: Theme,
                  addLabels = true): Viewport =
  # first write all plots into dummy viewport
  result = view
  result.background()

  # set the data scale for the result
  result.xScale = filledScales.xScale
  result.yScale = filledScales.yScale
  for fg in filledScales.geoms:
    # for each geom, we create a child viewport of `result` covering
    # the whole resultport, which will house the data we just created.
    # Due to being a child, if will be drawn *after* its parent. This way things like
    # ticks will be below the data.
    # On the other hand this allows us to draw several geoms in on a plot and have the
    # order of the function calls `geom_*` be preserved
    var pChild = result.addViewport(name = "data")
    # DF here not needed anymore!
    let gobjs = pChild.createGobjFromGeom(fg, theme)
    # add the data to the child
    pChild.addObj gobjs
    # add the data viewport to the view
    result.children.add pChild

  var xticks = result.handleTicks(filledScales, p, akX)
  var yticks = result.handleTicks(filledScales, p, akY)

  # TODO: Make sure we still have to do this. I think not!
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

proc generateFacetPlots(view: Viewport, p: GgPlot, plotScales: FilledScales,
                        theme: Theme): Viewport =
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

proc customPosition(t: Theme): bool =
  ## returns true if `legendPosition` is set and thus legend sits at custom pos
  result = t.legendPosition.isSome

type
  ScaleData = tuple
    data: Option[DataFrame]
    scale: Scale
    statKind: StatKind

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
    var filled: seq[Scale]
    # If the first scale is transformed, the others are too. Transformed handled
    # here, because `collectScales` uses `scLinearData` for `x` and `y`
    case scalesToUse[0].scKind
    of scTransformedData:
      filled = fillScale(pData, scalesToUse, scTransformedData)
    else:
      filled = fillScale(pData, scalesToUse, scKind)
    for fs in filled:
      result.add fs
  # now separates
  for i in separateIdxs:
    var additional: seq[Scale]
    case scales[i].scale.scKind
    of scTransformedData:
      additional = fillScale(scales[i].data.get, @[scales[i].scale], scTransformedData)
    else:
      additional = fillScale(scales[i].data.get, @[scales[i].scale], scKind)
    doAssert additional.len <= 1
    for fs in additional:
      result.add fs

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

func isEmpty(s: ginger.Scale): bool =
  ## checks if the given scale is empty
  result = s.low == s.high

func mergeScales(s1, s2: ginger.Scale): ginger.Scale =
  ## merges the two data scales and returns a version encompassing both
  result = (low: min(s1.low, s2.low),
            high: max(s1.high, s2.high))

proc applyTransformations(df: var DataFrame, scales: seq[Scale]) =
  ## Given a sequence of scales applies all transformations of the `scales`.
  ## That is for each `scTransformedData` scale the accroding transformation
  ## is applied its column
  var fns: seq[FormulaNode]
  for s in scales:
    if s.scKind == scTransformedData:
      let fn = f{ s.col ~ s.trans( s.col ) }
      fns.add fn
    elif s.col in df:
      # `s.col` may be pointing to scale which sets constant value
      fns.add f{ s.col }
  df = df.transmute(fns)

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
                         scales: seq[Scale]): (seq[string], seq[string]) =
  ## splits the given discrete (!) columns by whether they set data (i.e.
  ## arg not a DF column) or map data (arg is column)
  var setDiscCols = newSeq[string]()
  var mapDiscCols = newSeq[string]()
  for d in scales:
    # for discrete scales, build the continuous (if any) scales
    if d.col in df:
      mapDiscCols.add d.col
    else:
      setDiscCols.add d.col
  result = (setDiscCols, mapDiscCols)

proc setCountXScaleByType(xScale: var ginger.Scale, vKind: ValueKind,
                          xCol: string, df: DataFrame) =
  ## sets the X scale according to the value kind for `"count"` stats
  case vKind
  of VString, VBool, VNull, VObject:
    xScale = (low: 0.0, high: 1.0)
  of VInt, VFloat:
    let dfXScale = (low: min(df[xCol]).toFloat,
                    high: max(df[xCol]).toFloat)
    if xScale.isEmpty:
      xScale = dfXScale
    else:
      xScale = mergeScales(xScale, dfXScale)

proc applyContScaleIfAny(yieldDf: DataFrame,
                         fullDf: DataFrame,
                         scales: seq[Scale], baseStyle: Style): (seq[Style], DataFrame) =
  ## given continuous `scales` (if any) return the correct scales based
  ## on each of these scales
  ## NOTE: This modifies `yieldDf` adding all continuous scale columns to it
  result[1] = yieldDf
  for c in scales:
    result[1][c.col] = fullDf[c.col]
    for el in c.mapData():
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

proc filledIdentityGeom(df: var DataFrame, g: Geom,
                        filledScales: FilledScales): FilledGeom =
  let (x, y, discretes, cont) = df.separateScalesApplyTrafos(g.gid,
                                                             filledScales)
  let contCols = cont.mapIt(it.col)
  let (setDiscCols, mapDiscCols) = splitDiscreteSetMap(df, discretes)
  result = FilledGeom(geom: g,
                      xcol: x.col,
                      ycol: y.col,
                      dcKindX: x.dcKind,
                      dcKindY: y.dcKind)
  if x.dataScale.isEmpty:
    result.xScale = (low: min(df[x.col]).toFloat, high: max(df[x.col]).toFloat)
  else:
    result.xScale = x.dataScale
  if y.dataScale.isEmpty:
    result.yScale = (low: min(df[y.col]).toFloat, high: max(df[y.col]).toFloat)
  else:
    result.yScale = y.dataScale
  # w/ all groupings
  doAssert g.style.isSome
  var style = g.style.get
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    for keys, subDf in groups(df):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      var yieldDf = subDf.select(concat(@[x.col, y.col], contCols))
      result.numX = max(result.numX, yieldDf.len)
      result.yieldData[style] = applyContScaleIfAny(yieldDf, df, cont, style)
  else:
    # is select here even useful? Just makes the df given smaller, but...
    var yieldDf = df.select(concat(@[x.col, y.col], contCols))
    result.numX = yieldDf.len
    result.yieldData[style] = applyContScaleIfAny(yieldDf, df, cont, style)

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
                      xcol: x.col,
                      ycol: countCol,
                      dcKindX: x.dcKind,
                      dcKindY: dcContinuous)
  # w/ all groupings
  doAssert g.style.isSome
  var style = g.style.get
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    # sumHist used to calculate height of stacked histogram
    var sumHist: seq[int]
    for keys, subDf in groups(df):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      # before we assign calculate histogram
      let (hist, bins, binWidths) = g.callHistogram(subDf.dataTo(x.col, float),
                                                    range = x.dataScale)
      sumHist.addBinCountsByPosition(hist, g.position)
      var yieldDf = seqsToDf({ x.col : bins,
                               countCol: hist })
      result.yieldData[style] = applyContScaleIfAny(yieldDf, df, cont, style)
      result.numX = max(result.numX, yieldDf.len)
      result.xScale = mergeScales(result.xScale, (low: bins.min.float,
                                                  high: bins.max.float))
      result.yScale = mergeScales(result.yScale, (low: 0.0,
                                                  high: sumHist.max.float))
  else:
    let (hist, bins, binWidths) = g.callHistogram(df.dataTo(x.col, float),
                                                  range = x.dataScale)
    var yieldDf = seqsToDf({ x.col : bins,
                             countCol: hist,
                             widthCol: binWidths})
    result.yieldData[style] = applyContScaleIfAny(yieldDf, df, cont, style)
    result.numX = yieldDf.len
    result.xScale = (low: bins.min.float, high: bins.max.float)
    result.yScale = (low: 0.0, high: hist.max.float)

  # `numY` for `bin` stat is just max of the y scale. Since `histogram` counts the
  # number of values in a binned continuous scale the maximum value is always an `int`!
  result.numY = result.yScale.high.round.int

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
                      xcol: x.col,
                      ycol: countCol,
                      dcKindX: x.dcKind,
                      dcKindY: dcContinuous)
  let allClasses = df[x.col].unique
  # w/ all groupings
  doAssert g.style.isSome
  var style = g.style.get
  for setVal in setDiscCols:
    applyStyle(style, df, discretes, setDiscCols.mapIt((it, Value(kind: VNull))))
  if mapDiscCols.len > 0:
    df = df.group_by(mapDiscCols)
    # sumCounts used to calculate height of stacked histogram
    # TODO: can be simplified by implementing `count` of `grouped` DFs!
    var sumCounts = DataFrame()
    for keys, subDf in groups(df):
      # now consider settings
      applyStyle(style, subDf, discretes, keys)
      var yieldDf = subDf.count(x.col, name = countCol)
      # all values, which are zero still have to be accounted for! Add those keys with
      # zero values
      yieldDf.addZeroKeys(allClasses, x.col, countCol)
      # now arrange by `x.col` to force correct order
      yieldDf = yieldDf.arrange(x.col)
      sumCounts.addCountsByPosition(yieldDf, countCol, g.position)
      result.yieldData[style] = applyContScaleIfAny(yieldDf, df, cont, style)
      result.numX = max(result.numX, yieldDf.len)
      result.xScale.setCountXScaleByType(x.vKind, x.col, yieldDf)
      result.yScale = mergeScales(result.yScale,
                                  (low: 0.0,
                                   high: max(sumCounts[countCol]).toFloat))
  else:
    let yieldDf = df.count(x.col, name = countCol)
    result.numX = yieldDf.len
    result.yieldData[style] = applyContScaleIfAny(yieldDf, df, cont, style)
    result.xScale.setCountXScaleByType(x.vKind, x.col, yieldDf)
    result.yScale = (low: 0.0, high: yieldDf[countCol].max.toFloat)

  # `numY` for `count` stat is just max of the y scale. Since this uses `count` the
  # maximum value is always an `int`!
  result.numY = result.yScale.high.round.int
  doAssert result.numX == allClasses.len

proc postProcessScales(filledScales: var FilledScales, p: GgPlot) =
  ## walk all geoms and create the dataframes required to draw the
  ## geoms
  var xScale: ginger.Scale
  var yScale: ginger.Scale
  for g in p.geoms:
    var df = if g.data.isSome: g.data.get else: p.data
    var filledGeom: FilledGeom
    case g.kind
    of gkPoint, gkLine:
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
    else:
      raise newException(Exception, "Woaah, hey there, I'm just gonna ignore " &
        "you! " & $g.kind)

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

proc collectScales(p: GgPlot): FilledScales =
  ## Collects all scales required to draw the plot. This means comparing each
  ## possible aesthetic scale of the `GgPlot p` itself with its geoms and
  ## building the final `Scale` for each.
  # TODO: clean up
  macro collect(f: static string): untyped =
    let field = ident(f)
    result = quote do:
      block:
        var sds = newSeq[ScaleData]()
        if p.aes.`field`.isSome:
          # NOTE: the dataframe of `GgPlot` is always given individually to
          # the `fill*` procs, hence we give a `none` here
          sds.add (data: none(DataFrame), scale: p.aes.`field`.get,
                   statKind: stIdentity)
        for g in p.geoms:
          if g.aes.`field`.isSome:
            sds.add (data: g.data, scale: g.aes.`field`.get,
                     statKind: g.statKind)
        sds

  macro fillField(f: static string, arg: typed): untyped =
    let field = ident(f)
    let argId = ident(arg.strVal)
    result = quote do:
      if `argId`.len > 0 and `argId`[0].ids == {0'u16 .. high(uint16)}:
        result.`field` = (main: some(`argId`[0]), more: `argId`[1 .. ^1])
      else:
        result.`field` = (main: none[Scale](), more: `argId`)
  let xs = collect("x")
  # NOTE: transformed data handled from this in `callFillScale`!
  let xFilled = callFillScale(p.data, xs, scLinearData)
  fillField("x", xFilled)
  let ys = collect("y")
  # NOTE: transformed data handled from this in `callFillScale`!
  let yFilled = callFillScale(p.data, ys, scLinearData)
  fillField("y", yFilled)
  let colors = collect("color")
  let colorFilled = callFillScale(p.data, colors, scColor)
  fillField("color", colorFilled)
  let fills = collect("fill")
  let fillFilled = callFillScale(p.data, fills, scFillColor)
  fillField("fill", fillFilled)
  let sizes = collect("size")
  let sizeFilled = callFillScale(p.data, sizes, scSize)
  fillField("size", sizeFilled)
  let shapes = collect("shape")
  let shapeFilled = callFillScale(p.data, shapes, scShape)
  fillField("shape", shapeFilled)

  postProcessScales(result, p)

func labelName(filledScales: FilledScales, p: GgPlot, axKind: AxisKind): string =
  ## extracts the correct label for the given axis.
  ## First checks whether the theme sets a name, then checks the name of the
  ## x / y `Scale` and finally defaults to the column name.
  # doAssert p.aes.x.isSome, "x scale should exist?"
  case axKind
  of akX:
    let xScale = getXScale(filledScales)
    if xScale.name.len > 0:
      result = xScale.name
    else:
      result = xScale.col
  of akY:
    let yScale = getYScale(filledScales)
    if yScale.name.len > 0:
      result = yScale.name
    elif yScale.col.len > 0:
      result = yScale.col
    else:
      result = "count"

proc buildTheme*(filledScales: FilledScales, p: GgPlot): Theme =
  ## builds the final theme used for the plot. It takes the theme of the
  ## `GgPlot` object and fills in all missing fields as required from
  ## `filledScales` and `p`.
  result = p.theme
  if result.xLabel.isNone:
    result.xLabel = some(labelName(filledScales, p, akX))
  if result.yLabel.isNone:
    result.yLabel = some(labelName(filledScales, p, akY))
  if result.xLabelSecondary.isNone and filledScales.hasSecondary(akX):
    result.xLabelSecondary = some(filledScales.getSecondaryAxis(akX).name)
  if result.yLabelSecondary.isNone and filledScales.hasSecondary(akY):
    result.yLabelSecondary = some(filledScales.getSecondaryAxis(akY).name)

proc ggcreate*(p: GgPlot, width = 640.0, height = 480.0): PlotView =
  ## applies all calculations to the `GgPlot` object required to draw
  ## the plot with cairo and returns a `PlotView`. The `PlotView` contains
  ## the final `Scales` built from the `GgPlot` object and all its geoms
  ## plus the ginal ginger.Viewport which only has to be drawn to produce the
  ## plot.
  ## This proc is useful to investigate the final Scales or the Viewport
  ## that will actually be drawn.
  let filledScales = collectScales(p)
  let theme = buildTheme(filledScales, p)
  # create the plot
  var img = initViewport(name = "root",
                         wImg = width,
                         hImg = height)

  let drawLegend = filledScales.requiresLegend
  if drawLegend:
    img.plotLayoutWithLegend()
  else:
    img.plotLayoutWithoutLegend()
  # get viewport of plot
  var pltBase = img[4]

  if p.facet.isSome:
    pltBase = pltBase.generateFacetPlots(p, filledScales, theme)
    # TODO :clean labels up, combine with handleLabels!
    # Have to consider what should happen for that though.
    # Need flag to disable auto subtraction, because we don't have space or
    # rather if done needs to be done on all subplots?
    let xlabel = pltBase.xlabel(theme.xLabel.unwrap())
    let ylabel = pltBase.ylabel(theme.yLabel.unwrap())
    pltBase.addObj @[xlabel, ylabel]
  else:
    pltBase = pltBase.generatePlot(p, filledScales, theme)
  let xScale = pltBase.xScale
  let yScale = pltBase.yScale
  img[4] = pltBase
  img.xScale = xScale
  img.yScale = yScale
  #img.updateDataScale()

  # possibly correct the yScale assigned to the root Viewport
  img.yScale = pltBase.yScale

  # draw legends
  # store each type of drawn legend. only one type for each kind
  var drawnLegends = initHashSet[(DiscreteKind, ScaleKind)]()
  for scale in enumerateScalesByIds(filledScales):
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

  result.filledScales = filledScales
  result.view = img

proc ggdraw*(view: Viewport, fname: string) =
  ## draws the given viewport and stores it in `fname`.
  ## It assumes that the `view` was created as the field of
  ## a `PlotView` object from a `GgPlot` object with `ggcreate`
  view.draw(fname)

proc ggdraw*(plt: PlotView, fname: string) =
  ## draws the viewport of the given `PlotView` and stores it in `fname`.
  ## It assumes that the `plt`` was created from a `GgPlot` object with
  ## `ggcreate`
  plt.view.draw(fname)

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
