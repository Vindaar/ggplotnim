import tables, algorithm, sequtils, random, sets, math, macros, options

import ggplot_types, ggplot_utils
from ggplot_scales import scaleFromData
import ggplotnim / colormaps / viridisRaw
import postprocess_scales
import datamancer

import ginger except Scale

proc addIdentityData(data: var Column, df: DataFrame, s: Scale) =
  case s.col.kind
  of fkScalar:
    # a scalar may happen if the user uses a reducing operation as a formula
    # for an aes, e.g. ``x = f{float -> float: getMean(`bins`, `counts`)``
    data = data.add constantColumn(s.col.reduce(df), 1)
  else:
    data = data.add s.col.evaluate(df)

proc drawSampleIdx(sHigh: int, num = 100, seed = 42): seq[int] =
  ## draws `num` random sample indices with the seed `42` from the given `s`
  var r = initRand(seed) # for now just set a local state
  let idxNum = min(num - 1, sHigh)
  result = toSeq(0 .. idxNum).mapIt(r.rand(sHigh))

proc isDiscreteData(col: Column, s: Scale, drawSamples: static bool = true,
                    discreteThreshold = 0.125): bool =
  ## returns an ``estimate`` (!) of whether the given sequence of
  ## data is most likely discrete or continuous. This depends mainly
  ## on the column kind of the input column. If the column is:
  ## - float: continuous
  ## - string: discrete
  ## - bool: discrete
  ## - int: discrete
  ## - if float / int: generate set of first 100 elements, check
  ##   if cardinality of set > 50: continuous, else discrete
  ## - if bool: discrete
  ##
  ## It is possible to overwrite this by manually setting discreteness
  ## of a scale (use `scale_x/y_discrete/continuous` or `factor(col)`
  ## as an argument to `aes` for continuous data to be discrete).
  ##
  ## The associated `Scale` is only used for better error messages.
  case col.kind
  of colInt:
    when drawSamples:
      let indices = drawSampleIdx(col.high)
    else:
      let indices = toSeq(0 .. col.high)
    let elements = indices.mapIt(col[it, int]).toHashSet
    if elements.card > (indices.len.float * discreteThreshold).round.int:
      result = false
      echo "INFO: The integer column `", $s.col, "` has been automatically ",
       "determined to be continuous. To overwrite this behavior add a ",
       "`+ scale_x/y_discrete()` call to the plotting chain. Choose `x`",
       " or `y` depending on which axis this column refers to. Or apply a",
       " `factor` to the column name in the `aes` call, i.e.",
       " `aes(..., factor(\"" & $s.col & "\"), ...)`."
    else:
      result = true
      echo "INFO: The integer column `", $s.col, "` has been automatically ",
       "determined to be discrete. To overwrite this behavior add a ",
       "`+ scale_x/y_continuous()` call to the plotting chain. Choose `x`",
       " or `y` depending on which axis this column refers to."
  of colFloat:
     result = false
  of colString:
    # while the "discreteness" condition above might not always be satisfied for
    # strings, how would we represent string data on continuous scales?
    result = true
  of colBool:
    result = true
  of colConstant:
    # quite the literal definiton of a constant!
    result = true
  of colObject:
    ## use the same approach as for `colInt` if data is only numeric. If contains
    ## non numeric data (except `VNull`) it's discrete.
    when drawSamples:
      let indices = drawSampleIdx(col.high)
    else:
      let indices = toSeq(0 .. col.high)
    var discreteObjectCol = false
    let elVals = indices.mapIt(col[it, Value])
    let elementKinds = elVals.mapIt(it.kind).toHashSet
    if VObject in elementKinds:
      raise newException(ValueError, "Input column " & $s.col & " contains object like " &
        "values (key / value pairs in a single element). Such a column cannot be plotted.")
    elif VString in elementKinds or
         VBool in elementKinds:
      discreteObjectCol = true
    else:
      let elements = indices.mapIt(col[it, Value]).toHashSet
      if elements.card <= (indices.len.float * discreteThreshold).round.int:
        discreteObjectCol = true
    if not discreteObjectCol:
      result = false
      echo "INFO: The object column `", $s.col, "` has been automatically ",
       "determined to be continuous. To overwrite this behavior use ",
       "`scale_x/y_discrete` or apply `factor` to the column name in the `aes` ",
       "call."
    else:
      result = true
      echo "INFO: The object column `", $s.col, "` has been automatically ",
       "determined to be discrete. To overwrite this behavior use ",
       "`scale_x/y_continuous`."
  of colNone:
    raise newException(ValueError, "Input column " & $s.col & " is empty. Such a column " &
      "cannot be plotted.")

proc discreteAndType(data: Column,
                     s: Scale,
                     dcKind: Option[DiscreteKind] = none[DiscreteKind]()):
    tuple[isDiscrete: bool, vKind: ValueKind] =
  ## Returns the column kind (as a `ValueKind`) of the input column and the
  ## discreteness. For integer data we guess the discreteness based on a
  ## threshold of unique values in the column.
  ##
  ## The associated `Scale` is only used for better error messages.
  let vKind = toValueKind(data.kind)
  # auto determine discreteness iff not set manually by user
  let isDiscrete = block:
    if dcKind.isSome:
      let dc = dcKind.get
      dc == dcDiscrete
    else:
      isDiscreteData(data, s, drawSamples = true)
  result = (isDiscrete: isDiscrete,
            vKind: toValueKind(data.kind))

proc fillDiscreteColorScale(scKind: static ScaleKind, vKind: ValueKind, col: FormulaNode,
                            labelSeq: seq[Value],
                            valueMapOpt: Option[OrderedTable[Value, ScaleValue]]): Scale =
  result = Scale(scKind: scKind, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq

  if valueMapOpt.isSome:
    result.valueMap = valueMapOpt.get
  else:
    result.valueMap = initOrderedTable[Value, ScaleValue]()
    let colorCs = ggColorHue(labelSeq.len)
    for i, k in result.labelSeq:
      # NOTE: workaround, since we cannot do `kind: sckind` atm
      result.valueMap[k] = if scKind == scColor:
                             ScaleValue(kind: scColor, color: colorCs[i])
                           else:
                             ScaleValue(kind: scFillColor, color: colorCs[i])

proc fillDiscreteSizeScale(vKind: ValueKind, col: FormulaNode,
                           labelSeq: seq[Value],
                           valueMapOpt: Option[OrderedTable[Value, ScaleValue]]): Scale =
  result = Scale(scKind: scSize, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq
  result.valueMap = initOrderedTable[Value, ScaleValue]()
  if valueMapOpt.isSome:
    result.valueMap = valueMapOpt.get
  else:
    let numSizes = min(labelSeq.len, 5)
    const minSize = 2.0
    const maxSize = 7.0
    let stepSize = (maxSize - minSize) / numSizes.float
    for i, k in labelSeq:
      result.valueMap[k] = ScaleValue(kind: scSize, size: minSize + i.float * stepSize)

proc fillDiscreteShapeScale(vKind: ValueKind, col: FormulaNode,
                           labelSeq: seq[Value],
                           valueMapOpt: Option[OrderedTable[Value, ScaleValue]]): Scale =
  result = Scale(scKind: scShape, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq
  result.valueMap = initOrderedTable[Value, ScaleValue]()
  if valueMapOpt.isSome:
    result.valueMap = valueMapOpt.get
  else:
    for i, k in labelSeq:
      result.valueMap[k] = ScaleValue(kind: scShape,
                                      marker: MarkerKind(i mod (MarkerKind.high.ord + 1)),
                                      lineType: LineType(i mod (ord(LineType.high) - 1) + 1))

proc fillDiscreteLinearTransScale(
  scKind: static ScaleKind,
  col: FormulaNode,
  axKind: AxisKind,
  vKind: ValueKind, labelSeq: seq[Value],
  trans: Option[ScaleTransform] = none[ScaleTransform](),
  invTrans: Option[ScaleTransform] = none[ScaleTransform]()
     ): Scale =
  result = Scale(scKind: scKind, vKind: vKind, col: col, dcKind: dcDiscrete)
  result.labelSeq = labelSeq
  result.valueMap = initOrderedTable[Value, ScaleValue]()
  result.axKind = axKind
  if scKind == scTransformedData:
    ## we make  sure `trans` is some in the calling scope!
    result.trans = trans.get
    result.invTrans = invTrans.get

proc fillContinuousLinearScale(col: FormulaNode, axKind: AxisKind, vKind: ValueKind,
                               dataScale: ginger.Scale): Scale =
  result = Scale(scKind: scLinearData, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  result.axKind = axKind

proc fillContinuousTransformedScale(col: FormulaNode,
                                    axKind: AxisKind,
                                    vKind: ValueKind,
                                    trans: ScaleTransform,
                                    invTrans: ScaleTransform,
                                    dataScale: ginger.Scale): Scale =
  result = Scale(scKind: scTransformedData, vKind: vKind, col: col,
                 dcKind: dcContinuous,
                 # apply transformation to data scale
                 dataScale: (low: trans(dataScale.low),
                             high: trans(dataScale.high)))
  result.axKind = axKind
  result.trans = trans
  result.invTrans = invTrans

proc fillContinuousColorScale(scKind: static ScaleKind,
                              col: FormulaNode,
                              vKind: ValueKind,
                              dataScale: ginger.Scale,
                              df: DataFrame): Scale =
  ## devise colormap mapping
  result = Scale(scKind: scKind, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  # for now just take viridis as default
  # map all values to values between 0-255 and get the correct idx of viridis map
  result.mapData = (
    proc(df: DataFrame): seq[ScaleValue] =
      result = newSeq[ScaleValue](df.len)
      let t = col.evaluate(df).toTensor(float)
      assert t.size == df.len, "Resulting tensor size does not match df len!"
      for idx in 0 ..< t.size:
        var colorIdx = (255.0 * ((t[idx] - dataScale.low) /
                                 (dataScale.high - dataScale.low))).round.int
        colorIdx = max(0, min(255, colorIdx))
        let cVal = ViridisRaw[colorIdx]
        var scVal = if scKind == scColor:
                      ScaleValue(kind: scColor)
                    else:
                      ScaleValue(kind: scFillColor)
        scVal.color = color(cVal[0], cVal[1], cVal[2])
        result[idx] = scVal
  )


proc fillContinuousSizeScale(col: FormulaNode, vKind: ValueKind,
                             dataScale: ginger.Scale,
                             df: DataFrame): Scale =
  const minSize = 2.0
  const maxSize = 7.0
  result = Scale(scKind: scSize, vKind: vKind, col: col, dcKind: dcContinuous,
                 dataScale: dataScale)
  result.mapData = (
    proc(df: DataFrame): seq[ScaleValue] =
      let t = col.evaluate(df).toTensor(float)
      result = newSeq[ScaleValue](df.len)
      assert t.size == df.len, "Resulting tensor size does not match df len!"
      for idx in 0 ..< t.size:
        let size = (t[idx] - minSize) /
                   (maxSize - minSize)
        result[idx] = ScaleValue(kind: scSize,
                               size: size)
  )

proc fillScaleImpl(
  vKind: ValueKind,
  isDiscrete: bool,
  col: FormulaNode,
  df: DataFrame,
  scKind: static ScaleKind,
  labelSeqOpt = none[seq[Value]](), # for discrete data
  valueMapOpt = none[OrderedTable[Value, ScaleValue]](), # for discrete data
  dataScaleOpt = none[ginger.Scale](), # for cont data
  axKindOpt = none[AxisKind](),
  trans = none[ScaleTransform](),
  invTrans = none[ScaleTransform]()): Scale =
  ## fills the `Scale` of `scKind` kind of the `aes`
  ## TODO: make aware of Geom.data optional field!
  ## NOTE: The given `col` arg is not necessarily exactly a DF key anymore, since
  ## it might contain two or more columns as its basis
  # get the data column we scale by
  result = new Scale
  if isDiscrete:
    # convert to set to filter duplicates, back to seq and sort
    # TODO: we could also use `sequtils.deduplicate` here
    let labelSeq = labelSeqOpt.unwrap()
    case scKind
    of scColor:
      result = fillDiscreteColorScale(scColor, vKind, col, labelSeq, valueMapOpt)
    of scFillColor:
      result = fillDiscreteColorScale(scFillColor, vKind, col, labelSeq, valueMapOpt)
    of scSize:
      result = fillDiscreteSizeScale(vKind, col, labelSeq, valueMapOpt)
    of scLinearData:
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillDiscreteLinearTransScale(scLinearData, col,
                                            axKind, vKind, labelSeq)

    of scTransformedData:
      doAssert trans.isSome, "Transform data needs a ScaleTransform procedure!"
      doAssert invTrans.isSome, "Transform data needs an inverse ScaleTransform procedure!"
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillDiscreteLinearTransScale(scTransformedData, col,
                                            axKind, vKind, labelSeq,
                                            trans, invTrans)
    of scShape:
      result = fillDiscreteShapeScale(vKind, col, labelSeq, valueMapOpt)
    of scText: result = Scale(scKind: scText,
                              col: col) # nothing required but the column
  else:
    let dataScale = dataScaleOpt.unwrap()
    case scKind
    of scLinearData:
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillContinuousLinearScale(col, axKind, vKind, dataScale)
    of scTransformedData:
      doAssert trans.isSome, "Transform data needs a ScaleTransform procedure!"
      doAssert invTrans.isSome, "Transform data needs an inverse ScaleTransform procedure!"
      doAssert axKindOpt.isSome, "Linear data scales need an axis!"
      let axKind = axKindOpt.get
      result = fillContinuousTransformedScale(col, axKind, vKind,
                                              trans.get, invTrans.get,
                                              dataScale)
    of scColor:
      result = fillContinuousColorScale(scColor, col, vKind, dataScale, df)
    of scFillColor:
      result = fillContinuousColorScale(scFillColor, col, vKind, dataScale, df)
    of scSize:
      result = fillContinuousSizeScale(col, vKind, dataScale, df)
    of scShape:
      raise newException(ValueError, "Shape not supported for continuous " &
        "variables!")
    of scText: result = Scale(scKind: scText,
                              col: col) # nothing required but the column

type
  ScaleData = tuple
    dataFrame: Option[DataFrame]
    scale: Scale

proc fillScale(df: DataFrame, scales: seq[Scale],
               scKind: static ScaleKind): seq[Scale] =
  # NOTE: `data` is used to build a seq of data of the given scales. Be aware
  # that all scales given here belong to the same `aes` field, i.e. the same
  # "axis" (x, y, color,...) and thus can be considered compatible and part of the
  # same scale / classes! The actual data given to each filled scale however is not
  # this DF, but rather the input `df.select(s.col)`, see below.
  var data = newColumn()
  var transOpt: Option[ScaleTransform]
  var invTransOpt: Option[ScaleTransform]
  var axKindOpt: Option[AxisKind]
  # in a first loop over the scales read the data required to make decisions about
  # the appearence of the resulting scale
  for s in scales:
    # add this scales data to `data` DF for deduction of labels / data scales
    data.addIdentityData(df, s)
  # in the second loop for each of the scales add one filled scale to the result
  # using the combined dataset of all. This way we automatically get the correct
  # data range / correct number of labels while retaining a single scale per
  # geom.
  var dataScaleOpt: Option[ginger.Scale]
  var labelSeqOpt: Option[seq[Value]]
  var valueMapOpt: Option[OrderedTable[Value, ScaleValue]]
  var dcKindOpt: Option[DiscreteKind]
  for s in scales:
    # check if scale predefined discreteness
    if s.hasDiscreteness:
      dcKindOpt = some(s.dcKind)
    case scKind
    of scLinearData:
      axKindOpt = some(s.axKind)
    of scTransformedData:
      axKindOpt = some(s.axKind)
      # ## we use the last transformation we find!
      transOpt = some(s.trans)
      invTransOpt = some(s.invTrans)
    else: discard

    # now determine labels, data scale from `data`
    let (isDiscrete, vKind) = discreteAndType(data, s, dcKindOpt)
    if vKind == VNull:
      echo "WARNING: Unexpected data type VNull of column: ", s.col, "!"
      continue

    if isDiscrete:
      if s.labelSeq.len == 0 and
        (s.scKind in {scLinearData, scTransformedData} and not s.reversed or
         s.scKind notin {scLinearData, scTransformedData}):
        labelSeqOpt = some(data.unique.toTensor(Value).toRawSeq.sorted)
      elif s.labelSeq.len == 0 and
        (s.scKind in {scLinearData, scTransformedData} and s.reversed):
        labelSeqOpt = some(data.unique.toTensor(Value).toRawSeq.sorted.reversed)
      else:
        labelSeqOpt = some(s.labelSeq)
      if s.valueMap.len > 0:
        valueMapOpt = some(s.valueMap)
    else:
      # check if scale already set to something by user, if so use it
      dataScaleOpt = if s.dcKind == dcContinuous and s.dataScale.low != s.dataScale.high:
                       some(s.dataScale)
                     else:
                       some(scaleFromData(data, s))

    # now have to call `fillScaleImpl` with this information
    var filled = fillScaleImpl(vKind, isDiscrete, s.col, df, scKind,
                               labelSeqOpt, valueMapOpt, dataScaleOpt,
                               axKindOpt, transOpt, invTransOpt)
    if scKind in {scLinearData, scTransformedData}:
      filled.secondaryAxis = s.secondaryAxis
      # assign the `dateScale` if any
      filled.dateScale = s.dateScale
      filled.numTicks = s.numTicks
      filled.breaks = s.breaks
      # `dcKind` is already populated and won't be deduced
      # so that `s.dcKind` and `isDiscrete` are consistent
      if s.dcKind == dcDiscrete and isDiscrete:
        filled.formatDiscreteLabel = s.formatDiscreteLabel
      elif s.dcKind == dcContinuous and not isDiscrete:
        filled.formatContinuousLabel = s.formatContinuousLabel
    filled.ids = s.ids
    result.add filled

proc callFillScale(pData: DataFrame, scales: seq[ScaleData],
                   scKind: static ScaleKind): seq[Scale] =
  ## `pData` corresponds to the DataFrame of the `GgPlot` object. This is ``only`` (!!)
  ## used, if:
  ## - current scale is ``not`` in `GgPlot.aes`
  ## - `geom` with this scale has ``no`` `data` field
  # handle those geoms separately, which have their own data
  let separateIdxs = toSeq(0 .. scales.high).filterIt(scales[it].dataFrame.isSome)
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
      additional = fillScale(scales[i].dataFrame.get, @[scales[i].scale], scTransformedData)
    else:
      additional = fillScale(scales[i].dataFrame.get, @[scales[i].scale], scKind)
    doAssert additional.len <= 1
    for fs in additional:
      result.add fs

proc addFacets(fs: var FilledScales, p: GgPlot) =
  ## fills and adds the scales used as facets to the FilledScales object
  doAssert p.facet.isSome
  let facet = p.facet.unsafeGet
  var facetCols = newSeq[ScaleData]()
  for fc in facet.columns:
    let sc = (
      dataFrame: none[DataFrame](),
      scale: Scale(col: f{fc},
                   name: fc,
                   hasDiscreteness: true,
                   dcKind: dcDiscrete,
                   ids: {0'u16 .. high(uint16)}) # all geoms affected
    )
    # NOTE: we have to add each facet column individually to make sure their
    # discrete data is not mangled together in the `labelSeq` of each scale.
    fs.facets.add callFillScale(p.data, @[sc], scLinearData)

macro collect(p: GgPlot, field: untyped): untyped =
  result = quote do:
    var sds = newSeq[ScaleData]()
    if isSome(`p`.aes.`field`):
      # NOTE: the dataframe of GgPlot is always given individually to
      # the fill* procs, hence we give a none here
      let element = (dataFrame: none(DataFrame),
                     scale: `p`.aes.`field`.get)
      sds.add element
    for g in `p`.geoms:
      if isSome(g.aes.`field`):
        sds.add (dataFrame: g.data, scale: g.aes.`field`.get)
    sds

proc collectScales*(p: GgPlot): FilledScales =
  ## Collects all scales required to draw the plot. This means comparing each
  ## possible aesthetic scale of the `GgPlot p` itself with its geoms and
  ## building the final `Scale` for each.
  template fillField(f: untyped, arg: typed): untyped =
    if arg.len > 0 and arg[0].ids == {0'u16 .. high(uint16)}:
      result.f = (main: some(arg[0]), more: arg[1 .. ^1])
    else:
      result.f = (main: none[Scale](), more: arg)
  let xs = collect(p, x)
  # NOTE: transformed data handled from this in `callFillScale`!
  let xFilled = callFillScale(p.data, xs, scLinearData)
  fillField(x, xFilled)
  # possibly assign `reversedX` for filledScales
  if xs.anyIt(it.scale.reversed):
    result.reversedX = true
  if xFilled.anyIt(it.dcKind == dcDiscrete):
    result.discreteX = true

  let xsMin = collect(p, xMin)
  let xMinFilled = callFillScale(p.data, xsMin, scLinearData)
  fillField(xMin, xMinFilled)

  let xsMax = collect(p, xMax)
  let xMaxFilled = callFillScale(p.data, xsMax, scLinearData)
  fillField(xMax, xMaxFilled)

  var ys = collect(p, y)
  # NOTE: transformed data handled from this in `callFillScale`!
  let yFilled = callFillScale(p.data, ys, scLinearData)
  fillField(y, yFilled)
  # possibly assign `reversedX` for filledScales
  if ys.anyIt(it.scale.reversed):
    result.reversedY = true
  if yFilled.anyIt(it.dcKind == dcDiscrete):
    result.discreteY = true

  let ysMin = collect(p, yMin)
  let yMinFilled = callFillScale(p.data, ysMin, scLinearData)
  fillField(yMin, yMinFilled)

  let ysMax = collect(p, yMax)
  let yMaxFilled = callFillScale(p.data, ysMax, scLinearData)
  fillField(yMax, yMaxFilled)

  let ysRidges = collect(p, yRidges)
  let yRidgesFilled = callFillScale(p.data, ysRidges, scLinearData)
  fillField(yRidges, yRidgesFilled)

  let colors = collect(p, color)
  let colorFilled = callFillScale(p.data, colors, scColor)
  fillField(color, colorFilled)

  let fills = collect(p, fill)
  let fillFilled = callFillScale(p.data, fills, scFillColor)
  fillField(fill, fillFilled)

  let sizes = collect(p, size)
  let sizeFilled = callFillScale(p.data, sizes, scSize)
  fillField(size, sizeFilled)

  let shapes = collect(p, shape)
  let shapeFilled = callFillScale(p.data, shapes, scShape)
  fillField(shape, shapeFilled)

  let widths = collect(p, width)
  let widthFilled = callFillScale(p.data, widths, scLinearData)
  fillField(width, widthFilled)

  let heights = collect(p, height)
  let heightFilled = callFillScale(p.data, heights, scLinearData)
  fillField(height, heightFilled)
  # `text` is essentially a "dummy" scale, not required. Only care about
  # the column
  let texts = collect(p, text)
  let textFilled = callFillScale(p.data, texts, scText)
  fillField(text, textFilled)

  let weights = collect(p, weight)
  let weightFilled = callFillScale(p.data, weights, scLinearData)
  fillField(weight, weightFilled)

  # finally add all available facets if any
  if p.facet.isSome:
    result.addFacets(p)

  postProcessScales(result, p)
