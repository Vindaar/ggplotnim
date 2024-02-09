## .. include:: ./docs/ggplotnim_autogen.rst

import std / [sequtils, tables, sets, algorithm, strutils, macros, hashes, math, times]
from os import createDir, splitFile, expandTilde

when (NimMajor, NimMinor, NimPatch) > (1, 3, 0):
  export strutils.nimIdentNormalize # for parseEnum to work

import ginger except Scale
export ginger.types

from seqmath import linspace

# ggplotnim continues to make the dataframe available of course
import datamancer
export datamancer

import ggplotnim / [
  ggplot_utils, ggplot_types, ggplot_theme, ggplot_ticks, ggplot_styles, theme_toml,
  # utils dealing with scales
  ggplot_scales,
  # first stage of drawing: collect and fill `Scales`:
  collect_and_fill,
  # second stage of drawing: post process the scales (not required to be imported
  # in this module)
  # postprocess_scales,
  ggplot_drawing, # third stage: the actual drawing
  # vega backend, not imported by default, because webview adds compilation
  # flags even if we do not use vega, thus we depend on webkit for no reason
  # ggplot_vega
]
export ggplot_types
export ggplot_utils

import ggplotnim / colormaps / colormaps
export viridis, magma, inferno, plasma, cool

import chroma
export chroma

export sets

import options
export options

# Ids start at 1, because `0` is our magic value for cases where the
# id does not matter!
var IdCounter = 1'u16
template incId(): uint16 =
  let old = IdCounter
  inc IdCounter
  old

proc orNone(f: float): Option[float] =
  ## returns either a `some(f)` if `classify(f) != NaN` or none[float]()
  if classify(f) != fcNan: some(f)
  else: none[float]()

proc orNoneScale*[T: string | SomeNumber | FormulaNode](
  s: T, scKind: static ScaleKind,
  axKind = akX,
  hasDiscreteness = false,
  dcKind = dcDiscrete): Option[Scale] =
  ## returns either a `some(Scale)` of kind `ScaleKind` or `none[Scale]` if
  ## `s` is empty
  if ($s).len > 0:
    when T is string or T is SomeNumber:
      let fs = f{s}
    else:
      let fs = s
    case scKind
    of scLinearData:
      result = some(Scale(scKind: scLinearData, col: fs, axKind: axKind,
                          hasDiscreteness: hasDiscreteness,
                          dcKind: dcKind))
    of scTransformedData:
      result = some(Scale(scKind: scTransformedData, col: fs, axKind: axKind,
                          hasDiscreteness: hasDiscreteness,
                          dcKind: dcKind))
    of scSize:
      result = some(Scale(scKind: scSize, col: fs,
                          sizeRange: DefaultSizeRange,
                          hasDiscreteness: hasDiscreteness,
                          dcKind: dcKind))
    of scAlpha:
      result = some(Scale(scKind: scAlpha, col: fs,
                          alphaRange: DefaultAlphaRange,
                          hasDiscreteness: hasDiscreteness,
                          dcKind: dcKind))
    of scColor:
      result = some(Scale(scKind: scColor, col: fs,
                          colorScale: DefaultColorScale,
                          hasDiscreteness: hasDiscreteness,
                          dcKind: dcKind))
    of scFillColor:
      result = some(Scale(scKind: scFillColor, col: fs,
                          colorScale: DefaultColorScale,
                          hasDiscreteness: hasDiscreteness,
                          dcKind: dcKind))
    else:
      result = some(Scale(scKind: scKind, col: fs,
                          hasDiscreteness: hasDiscreteness,
                          dcKind: dcKind))
  else:
    result = none[Scale]()

template hasFactor(n: NimNode): untyped = n.kind == nnkCall and n[0].strVal == "factor"
template hasGradient(n: NimNode): untyped = n.kind == nnkCall and n[0].strVal == "gradient"

proc initField*(name: string, val: NimNode): NimNode =
  # determine if magic `factor` used to designate a scale as discrete
  let isDiscrete = hasFactor(val)
  let isContinuous = hasGradient(val)
  # if so, use the actual arg as value
  var val = if isDiscrete or isContinuous: val[1] else: val
  let dcKind = if isDiscrete: dcDiscrete elif isContinuous: dcContinuous else: dcDiscrete
  template call(kind: untyped, ax = akX): untyped =
    result = nnkCall.newTree(ident"orNoneScale", val, newLit kind, newLit ax,
                             newLit (isDiscrete or isContinuous),
                             newLit dcKind)

  case name.normalize
  of "x" : call(scLinearData, akX)
  of "y" : call(scLinearData, akY)
  of "color" : call(scColor)
  of "fill" : call(scColor)
  of "shape" : call(scShape)
  of "size" : call(scSize)
  of "xmin" : call(scLinearData, akX)
  of "xmax" : call(scLinearData, akX)
  of "ymin" : call(scLinearData, akY)
  of "ymax" : call(scLinearData, akY)
  of "width" : call(scLinearData, akX)
  of "height" : call(scLinearData, akY)
  of "text" : call(scText)
  of "yridges" : call(scLinearData, akY)
  of "weight" : call(scLinearData, akY)
  else: doAssert false, "not reachable"

proc getArgValue(n: NimNode, arg: string): NimNode =
  case n.kind
  of nnkSym: result = n
  of nnkIdent:
    let valAsStr = n.strVal
    result = quote do:
      when declared(`n`):
        # is a variable in existing scope. Use it
        `n`
      else:
        # doesn't exist, interpret as string for a column
        `valAsStr`
  of nnkIntLit .. nnkFloat64Lit, nnkStrLit, nnkTripleStrLit, nnkRStrLit:
    result = n
  of nnkCurlyExpr:
    # Formula node via `f{}`
    result = n
  of nnkPar:
    # e.g. `("someString" & $var)`
    result = n
  of nnkInfix:
    # some compuatition, e.g. `someVar + 2.0`
    result = n
  of nnkCall:
    # either function call returning some normal value or magic `factor`, `gradient`
    if hasFactor(n):
      result = nnkCall.newTree(n[0], getArgValue(n[1], arg))
    elif hasGradient(n):
      result = nnkCall.newTree(n[0], getArgValue(n[1], arg))
    else:
      result = n
  of nnkCommand:
    # might be an argument like `fn {someFormula}`
    result = n
  of nnkPrefix:
    # might be a string conversion `$foo`
    result = n
  else:
    error("Invalid value for argument `" & $arg & "`: " & $n.repr & " of node " &
      "kind " & $n.kind)

proc getArgStr(n: NimNode): string =
  case n.kind
  of nnkIdent, nnkSym,
     nnkStrLit, nnkTripleStrLit, nnkRStrLit: result = n.strVal
  of nnkOpenSymChoice, nnkClosedSymChoice: result = n[0].strVal
  else:
    error("Invalid node for argument `" & $(n.repr) & " in aes macro!")

macro aes*(args: varargs[untyped]): untyped =
  ## This macro parses the given arguments and returns an `Aesthetics` object
  ## based on the given input.
  ## The argument has to be an argument list, which can have have elements of
  ## different forms.
  ##
  ## - named / unnamed arguments:
  ##   - for named arguments, the name *must* be a valid field of the `Aesthetics`
  ##     object
  ##   - unnamed arguments are supported. The macro picks the field corresponding
  ##     to the index of each field in the order of the fields of `Aesthetics`
  ##     fields
  ##   In principle unnamed arguments *can* follow named ones, but better do not
  ##   abuse that...
  ## - Different types are supported for the values
  ##   - literals: string, int, float. Will be treated as constant `FormulaNode`
  ##     values for the associated scale (useful for e.g. `width = 0.5`)
  ##   - formula nodes: formula nodes are simply assigned as the columns for
  ##     the generated scale. Can refer to a column or a complicated expression.
  ##   - idents: raw idents are supported. If the identifier refers to something
  ##     declared, the value of that is used. Else the identifier is treated as
  ##     a string. Be careful with this feature!
  const allowedArgs = [ "x", "y", "color", "fill", "shape", "size", "xmin",
                        "xmax", "ymin", "ymax", "width", "height", "text",
                        "yridges", "weight" ]
  # first check if all `args` are allowed
  expectKind(args, nnkArgList)
  # given valid arguments, parse as required and create the `Aesthetic` object
  var aesArgs = nnkObjConstr.newTree(ident"Aesthetics")
  # now walk args again and create the output fields; things to note:
  # If no field name given, we go by idx -> index `allowedArgs` with it
  for idx, arg in args:
    case arg.kind
    of nnkExprEqExpr:
      let argStr = getArgStr(arg[0])
      if argStr.normalize notin allowedArgs:
        error("Invalid aesthetic: " & $argStr & "!")
      else:
        aesArgs.add nnkExprColonExpr.newTree(
          ident(argStr),
          initField(argStr,
                    getArgValue(arg[1], argStr))
        )
    of nnkIdent, nnkSym,
       nnkIntLit .. nnkFloat64Lit,
       nnkStrLit, nnkTripleStrLit, nnkRStrLit,
       nnkCurlyExpr:
      aesArgs.add nnkExprColonExpr.newTree(
        ident(allowedArgs[idx]),
        initField(allowedArgs[idx],
                  getArgValue(arg, allowedArgs[idx]))
      )
    else:
      error("Invalid `aes` argument of node kind " & $arg.kind & ": " & $arg.repr)
  result = aesArgs

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
  fillIt(result.xMin)
  fillIt(result.xMax)
  fillIt(result.yMin)
  fillIt(result.yMax)
  fillIt(result.width)
  fillIt(result.height)
  fillIt(result.text)
  fillIt(result.yRidges)
  fillIt(result.weight)

proc ggplot*(data: DataFrame, aes: Aesthetics = aes(),
             backend = bkNone): GgPlot =
  ## Note: The backend argument is required when using `ggcreate` with a
  ## a `ggplot` argument without `ggsave`. All string related placements
  ## require knowledge of a backend to compute absolute positions.
  # create new DF object (underlying data same) so that we don't mess up
  # the table of the user
  result = GgPlot(data: data.shallowCopy,
                  backend: backend)
  result.aes = aes.fillIds({0'u16 .. high(uint16)})
  # TODO: fill others with defaults
  # add default theme
  result.theme = Theme(discreteScaleMargin: some(quant(0.2,
                                                       ukCentimeter)))

template assignBinFields(res: var Geom, stKind, bins,
                         binWidth, breaks, bbVal, density: untyped): untyped =
  case stKind
  of stBin:
    if breaks.len > 0:
      res.binEdges = some(breaks)
    if binWidth > 0.0:
      res.binWidth = some(binWidth)
    if bins > 0:
      res.numBins = bins
    res.binBy = bbVal
    res.density = density
  else: discard

func assignBreaks[T: int | seq[SomeNumber]](scale: var Scale, breaks: T) =
  ## Assings the correct field for the `Scale` related to the number of
  ## ticks or tick positions.
  when T is int:
    scale.numTicks = some(breaks)
  elif T is seq[float]:
    scale.breaks = breaks
  else:
    scale.breaks = breaks.mapIt(it.float)

proc geom_point*[
  C: PossibleColor;
  S: PossibleFloat;
  M: PossibleMarker;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(),
    size: S = missing(),
    marker: M = missing(),
    stat = "identity",
    bins = -1,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity", # the position kind, "identity", "stack" etc.
    binBy = "full",
    density = false,
    alpha: A = missing()
               ): Geom =
  ## NOTE: When using a different position than `identity`, be careful reading the plot!
  ## If N classes are stacked and an intermediate class has no entries, it will be drawn
  ## on top of the previous value!
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pSize = size, pMarker = marker, pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkPoint,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind,
                position: pKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_errorbar*[
  C: PossibleColor;
  S: PossibleFloat;
  LT: PossibleLineType](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(),
    size: S = missing(),
    lineType: LT = missing(),
    errorBarKind = ebLinesT,
    stat = "identity",
    bins = -1,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity", # the position kind, "identity", "stack" etc.
    binBy = "full",
    density = false
                   ): Geom =
  ## NOTE: When using a different position than `identity`, be careful reading the plot!
  ## If N classes are stacked and an intermediate class has no entries, it will be drawn
  ## on top of the previous value!
  ##
  ## Possible `LineTypes` are:
  ##
  ## `ltNone, ltSolid, ltDashed, ltDotted, ltDotDash, ltLongDash, ltTwoDash`
  ##
  ## Possible `ErrorBarKind` are:
  ##
  ## `ebLines`, `ebLinesT`
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pSize = size, pLineType = lineType, pErrorBarKind = errorBarKind
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkErrorBar,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind,
                position: pKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_linerange*[
  C: PossibleColor;
  S: PossibleFloat;
  LT: PossibleLineType](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(),
    size: S = missing(),
    lineType: LT = missing(),
    stat = "identity",
    bins = -1,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity", # the position kind, "identity", "stack" etc.
    binBy = "full",
    density = false
                      ): Geom =
  ## NOTE: When using a different position than `identity`, be careful reading the plot!
  ## If N classes are stacked and an intermediate class has no entries, it will be drawn
  ## on top of the previous value!
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pLineWidth = size, pLineType = lineType
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkErrorBar,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind,
                position: pKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_bar*[
  C: PossibleColor;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(), # color of the bars
    alpha: A = missing(),
    position = "stack",
    stat = "count",
                  ): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = color, pAlpha = alpha, pLineType = ltSolid,
    pLineWidth = 1.0 # draw 1 pt wide black line to avoid white pixels
                      # between bins at size of exactly 1.0 bin width
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkBar,
                aes: aes.fillIds({gid}),
                userStyle: style,
                position: pkKind,
                binPosition: bpNone,
                statKind: stKind)

proc geom_line*[
  C: PossibleColor;
  S: PossibleFloat;
  LT: PossibleLineType;
  FC: PossibleColor;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(), # color of the line
    size: S = missing(), # width of the line
    lineType: LT = missing(), # type of line
    fillColor: FC = missing(),
    alpha: A = missing(),
    stat = "identity",
    bins = -1,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity",
    binBy = "full",
    density = false
                  ): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = fillColor, pLineWidth = size, pLineType = lineType, pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkLine,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_smooth*[
  C: PossibleColor;
  S: PossibleFloat;
  LT: PossibleLineType;
  FC: PossibleColor;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(), # color of the line
    size: S = missing(), # width of the line
    lineType: LT = missing(), # type of line
    fillColor: FC = missing(),
    alpha: A = missing(),
    span = 0.7,
    smoother = "svg", ## the smoothing method to use `svg`, `lm`, `poly`
    polyOrder = 5,    ## polynomial order to use (no effect for `lm`)
    bins = -1,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity",
    binBy = "full",
    density = false
                 ): Geom =
  ## Draws a smooth line that is a filtered version of the `x` and `y` data given as
  ## the `aes` of the plot (or to this geom).
  ##
  ## Note: if the input data is considered to be discrete (either manually or automatically
  ## if it's an integer column), a `ValueError` will be raised at runtime as smoothing
  ## is incompatible with a discrete plot and thus would lead to an undesirable outcome.
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let smKind = parseEnum[SmoothMethodKind](smoother)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = fillColor, pLineWidth = size, pLineType = lineType,
    pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkLine,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stSmooth,
                methodKind: smKind,
                span: span,
                polyOrder: polyOrder)
  assignBinFields(result, stSmooth, bins, binWidth, breaks, bbKind, density)

proc geom_histogram*[
  C: PossibleColor;
  FC: PossibleColor;
  LW: PossibleFloat;
  LT: PossibleLineType;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    binWidth = 0.0, bins = 30,
    breaks: seq[float] = @[],
    color: C = missing(), # color of the bar outlines
    fillColor: FC = missing(), # color of the bars inners
    alpha: A = missing(),
    position = "stack", # default is `stack`
    stat = "bin",
    binPosition = "left",
    binBy = "full",
    density = false,
    lineWidth: LW = some(0.2),
    lineType: LT = some(ltSolid),
    hdKind: HistogramDrawingStyle = hdBars, # the drawing style of histo. Outline
                                            # does not allow `position = "stack"`.
                  ): Geom =
  let dfOpt = if data.len > 0: some(data.shallowCopy) else: none[DataFrame]()
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = fillColor,
    pLineWidth = lineWidth, # draw 0.2 pt wide black line to avoid white pixels
                            # between bins at size of exactly 1.0 bin width
    pLineType = lineType,
    pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkHistogram,
                aes: aes.fillIds({gid}),
                userStyle: style,
                position: pkKind,
                binPosition: bpKind,
                statKind: stKind,
                hdKind: hdKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_freqpoly*[
  C: PossibleColor;
  S: PossibleFloat;
  LT: PossibleLineType;
  FC: PossibleColor;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(), # color of the line
    size: S = missing(), # line width of the line
    lineType: LT = missing(),
    fillColor: FC = missing(),
    alpha: A = missing(),
    bins = 30,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    position = "identity",
    stat = "bin",
    binPosition = "center",
    binBy = "full",
    density = false
                  ): Geom =
  let dfOpt = if data.len > 0: some(data.shallowCopy) else: none[DataFrame]()
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = fillColor, pLineWidth = size, pLineType = lineType,
    pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkFreqPoly,
                aes: aes.fillIds({gid}),
                userStyle: style,
                position: pkKind,
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_density*[
  C: PossibleColor;
  S: PossibleFloat;
  LT: PossibleLineType;
  FC: PossibleColor;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(), # color of the line
    size: S = missing(), # line width of the line
    lineType: LT = missing(),
    fillColor: FC = missing(),
    alpha: A = missing(),
    kernel = "gauss", ## {"gauss", "box", "triangular", "trigonometric", "epanechnikov"}
    bandwidth = NaN,
    adjust = 1.0,
    samples = 1000,
    sampleSeq: seq[float] = @[], ## XXX: allow tensor?
    range = (NegInf, Inf),
    normalize = false,
    position = "identity",
    density = true
                  ): Geom =
  let dfOpt = if data.len > 0: some(data.shallowCopy) else: none[DataFrame]()
  let pkKind = parseEnum[PositionKind](position)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = fillColor, pLineWidth = size, pLineType = lineType,
    pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkLine,
                aes: aes.fillIds({gid}),
                userStyle: style,
                position: pkKind,
                statKind: stDensity,
                # density related fields
                bandwidth: bandwidth,
                adjust: adjust,
                normalize: normalize,
                samples: samples,
                sampleSeq: sampleSeq,
                range: range,
                kernel: kernel)

proc geom_tile*[
  C: PossibleColor;
  S: PossibleFloat;
  FC: PossibleColor;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(),
    fillColor: FC = missing(),
    alpha: A = missing(),
    size: S = missing(),
    stat = "identity",
    bins = 30,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity", # the position kind, "identity", "stack" etc.
    binBy = "full",
    density = false
                ): Geom =
  ## NOTE: When using a different position than `identity`, be careful reading the plot!
  ## If N classes are stacked and an intermediate class has no entries, it will be drawn
  ## on top of the previous value!
  let dfOpt = if data.len > 0: some(data.shallowCopy) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = fillColor, pSize = size, pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkTile,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind,
                position: pKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_raster*[
  C: PossibleColor;
  S: PossibleFloat;
  FC: PossibleColor;
  A: PossibleFloat](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(),
    fillColor: FC = missing(),
    alpha: A = missing(),
    size: S = missing(),
    stat = "identity",
    bins = 30,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity", # the position kind, "identity", "stack" etc.
    binBy = "full",
    density = false
                  ): Geom =
  ## NOTE: When using a different position than `identity`, be careful reading the plot!
  ## If N classes are stacked and an intermediate class has no entries, it will be drawn
  ## on top of the previous value!
  let dfOpt = if data.len > 0: some(data.shallowCopy) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pFillColor = fillColor, pSize = size, pAlpha = alpha
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkRaster,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind,
                position: pKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_text*[
  C: PossibleColor;
  S: PossibleFloat;
  M: PossibleMarker;
  A: PossibleFloat;
  F: PossibleFont](
    aes: Aesthetics = aes(),
    data = DataFrame(),
    color: C = missing(),
    size: S = missing(),
    marker: M = missing(),
    alpha: A = missing(),
    font: F = missing(),
    alignKind = taCenter,
    stat = "identity",
    bins = -1,
    binWidth = 0.0,
    breaks: seq[float] = @[],
    binPosition = "none",
    position = "identity", # the position kind, "identity", "stack" etc.
    binBy = "full",
    density = false
                 ): Geom =
  ## NOTE: When using a different position than `identity`, be careful reading the plot!
  ## If N classes are stacked and an intermediate class has no entries, it will be drawn
  ## on top of the previous value!
  let dfOpt = if data.len > 0: some(data.shallowCopy) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  let fontOpt = when F is Missing: font(12.0, alignKind = alignKind)
                else: font
  # modify `Aesthetics` for all identity scales (column references) & generate style
  var aes = aes
  let style = aes.assignIdentityScalesGetStyle(
    pColor = color, pSize = size, pAlpha = alpha, pMarker = marker, pFont = fontOpt
  )
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkText,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind,
                position: pKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc ggridges*[T: FormulaNode | string](col: T, overlap = 1.3,
                                        showTicks = false,
                                        labelOrder = initTable[Value, int]()): Ridges =
  ## `showTicks` decides whether we show ticks and labels along the y
  ## axis in between the ridge labels. This is disabled by default, because
  ## it makes the plot very busy.
  when T is string:
    let col = f{col}
  else:
    let col = col
  result = Ridges(col: col, overlap: overlap,
                  showTicks: showTicks,
                  labelOrder: labelOrder)


proc facet_wrap*[T: FormulaNode | string](fns: varargs[T],
                                          scales = "fixed",
                                          order: seq[Value] = @[]): Facet =
  ## Creates a facet plot (a grid based on grouped data given the arguments
  ## `fns`). `scales` decides whether the x or y or both axes are fixed between
  ## all plots or can be independent.
  ##
  ## `order` allows to customize the order in which the plots appear on the grid
  ## going from top left to bottom right (row wise).
  ##
  ## The order is given as a `seq[Value]`, which is one `VObejct` value for each
  ## subplot that will be produced.
  ##
  ## Say your DF contains a column Class with 3 distinct labels `["A", "B", "C"]` and
  ## many other entries for each label. Then `facet_wrap("Class")` will produce one
  ## subplot for each of the three labels.
  ##
  ## To decide the order in which they appear, you would define a seq containing
  ##
  ## ```nim
  ## let order = %~ { "Class" : "A", "Class" : "B", "Class" : "C" }
  ## ```
  ##
  ## The apparent duplication of `Class` is because it is possible to create a
  ## facet wrap due based on multiple classes.
  let sfKind = parseEnum[ScaleFreeKind](scales)
  result = Facet(sfKind: sfKind, order: order)
  for f in fns:
    when T is FormulaNode:
      doAssert f.kind == fkVariable
      let fn = f
    else:
      let fn = f{f}
    let sc = Scale(col: fn,
                   name: fn.name,
                   hasDiscreteness: true,
                   dcKind: dcDiscrete,
                   ids: {0'u16 .. high(uint16)}) # all geoms affected
    result.columns.add sc

proc scale_x_log10*[T: int | seq[SomeNumber]](breaks: T = newSeq[float]()): Scale =
  ## Sets the X scale of the plot to a log10 scale.
  ##
  ## `breaks` allows to specify either the number of ticks desired (in case
  ## an integer is given) or the exact locations of the ticks given in
  ## units of the data space belonging to this axis.
  ##
  ## Note that the exact number of desired ticks is usually not respected,
  ## rather a close number that yields "nice" tick labels is chosen.
  let trans = proc(v: float): float =
    result = log10(v)
  let invTrans = proc(v: float): float =
    result = pow(10, v)

  result = Scale(col: f{""}, # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akX,
                 dcKind: dcContinuous,
                 trans: trans,
                 invTrans: invTrans)
  result.assignBreaks(breaks)

proc scale_y_log10*[T: int | seq[SomeNumber]](breaks: T = newSeq[float]()): Scale =
  ## Sets the Y scale of the plot to a log10 scale.
  ##
  ## `breaks` allows to specify either the number of ticks desired (in case
  ## an integer is given) or the exact locations of the ticks given in
  ## units of the data space belonging to this axis.
  ##
  ## Note that the exact number of desired ticks is usually not respected,
  ## rather a close number that yields "nice" tick labels is chosen.
  let trans = proc(v: float): float =
    result = log10(v)
  let invTrans = proc(v: float): float =
    result = pow(10, v)

  result = Scale(col: f{""}, # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akY,
                 dcKind: dcContinuous,
                 trans: trans,
                 invTrans: invTrans)
  result.assignBreaks(breaks)

proc scale_x_log2*[T: int | seq[SomeNumber]](breaks: T = newSeq[float]()): Scale =
  ## Sets the X scale of the plot to a log2 scale.
  ##
  ## `breaks` allows to specify either the number of ticks desired (in case
  ## an integer is given) or the exact locations of the ticks given in
  ## units of the data space belonging to this axis.
  ##
  ## Note that the exact number of desired ticks is usually not respected,
  ## rather a close number that yields "nice" tick labels is chosen.
  let trans = proc(v: float): float =
    result = log2(v)
  let invTrans = proc(v: float): float =
    result = pow(2, v)

  result = Scale(col: f{""}, # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akX,
                 dcKind: dcContinuous,
                 trans: trans,
                 invTrans: invTrans)
  result.assignBreaks(breaks)

proc scale_y_log2*[T: int | seq[SomeNumber]](breaks: T = newSeq[float]()): Scale =
  ## Sets the Y scale of the plot to a log2 scale
  ##
  ## `breaks` allows to specify either the number of ticks desired (in case
  ## an integer is given) or the exact locations of the ticks given in
  ## units of the data space belonging to this axis.
  ##
  ## Note that the exact number of desired ticks is usually not respected,
  ## rather a close number that yields "nice" tick labels is chosen.
  let trans = proc(v: float): float =
    result = log2(v)
  let invTrans = proc(v: float): float =
    result = pow(2, v)

  result = Scale(col: f{""}, # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akY,
                 dcKind: dcContinuous,
                 trans: trans,
                 invTrans: invTrans)
  result.assignBreaks(breaks)

func sec_axis*(trans: FormulaNode = f{""},
               transFn: ScaleTransform = nil,
               invTransFn: ScaleTransform = nil,
               name: string = ""): SecondaryAxis =
  ## convenience proc to create a `SecondaryAxis`
  var fn: Option[FormulaNode]
  if trans.name.len > 0:
    fn = some(trans)
  if not transFn.isNil and not invTransFn.isNil:
    result = SecondaryAxis(scKind: scTransformedData,
                           transFn: transFn, invTransFn: invTransFn,
                           name: name)
  elif not transFn.isNil or not invTransFn.isNil:
    raise newException(Exception, "In case of using a transformed secondary scale, both the " &
      "forward and reverse transformations have to be provided!")
  else:
    result = SecondaryAxis(scKind: scLinearData,
                           trans: fn,
                           name: name)

proc scale_x_discrete*[
  P: PossibleSecondaryAxis](
    name: string = "",
    secAxis: P = missing(),
    labels: proc(x: Value): string = nil
                          ): Scale =
  ## creates a discrete x axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the x axis to the tick label that should be shown for it.
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akX,
                 dcKind: dcDiscrete,
                 hasDiscreteness: true,
                 secondaryAxis: secAxis.toOptSecAxis(akX),
                 formatDiscreteLabel: labels)

proc scale_x_discrete*[
  P: PossibleSecondaryAxis;
  T;
  U](
    name: string = "",
    labels: OrderedTable[T, U],
    secAxis: P = missing(),
                          ): Scale =
  ## creates a discrete x axis with a possible secondary axis.
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akX,
                 dcKind: dcDiscrete,
                 hasDiscreteness: true,
                 secondaryAxis: secAxis.toOptSecAxis(akX),
                 formatDiscreteLabel: labels)
  result.labelSeq = newSeq[Value](labels.len)
  let keys = toSeq(keys(labels))
  for i, k in keys:
    let kVal = %~ k
    result.valueMap[kVal] = ScaleValue(kind: scLinearData, val: %~ labels[kVal])
    result.labelSeq[i] = kVal

proc scale_x_continuous*[
  P: PossibleSecondaryAxis,
  T: int | seq[SomeNumber]](
    secAxis: P = missing(),
    name: string = "",
    breaks: T = newSeq[float](),
    labels: proc(x: float): string = nil,
    trans: proc(x: float): float = nil,
    invTrans: proc(x: float): float = nil
                          ): Scale =
  ## Creates a continuous x axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the x axis to the tick label that should be shown for it.
  ##
  ## `breaks` allows to specify either the number of ticks desired (in case
  ## an integer is given) or the exact locations of the ticks given in
  ## units of the data space belonging to this axis.
  ##
  ## Note that the exact number of desired ticks is usually not respected,
  ## rather a close number that yields "nice" tick labels is chosen.
  if not trans.isNil and not invTrans.isNil:
    result = Scale(name: name,
                   scKind: scTransformedData,
                   axKind: akX,
                   dcKind: dcContinuous,
                   hasDiscreteness: true,
                   secondaryAxis: secAxis.toOptSecAxis(akX),
                   formatContinuousLabel: labels,
                   trans: trans,
                   invTrans: invTrans)
  elif trans.isNil xor invTrans.isNil:
    raise newException(Exception, "If `scale_y_continuous` is used for a custom " &
      "transformed data scale both the transformation and inverse have to be defined!")
  else:
    result = Scale(name: name,
                   scKind: scLinearData,
                   axKind: akX,
                   dcKind: dcContinuous,
                   hasDiscreteness: true,
                   secondaryAxis: secAxis.toOptSecAxis(akX),
                   formatContinuousLabel: labels)
  result.assignBreaks(breaks)

proc scale_x_date*[T: seq[SomeNumber]](
    name: string = "",
    breaks: T = newSeq[float](),
    isTimestamp = false, # if true, `x` column is assumed to be a unix timestamp
    parseDate: proc(x: string): DateTime = nil, # else it should be a string
    formatString: string = "yyyy-MM-dd",
    dateSpacing: Duration = initDuration(days = 1),
    dateAlgo: DateTickAlgorithmKind = dtaFilter,
    timeZone: Timezone = utc()): DateScale =
  ## Creates a continuous `x` axis that generates labels according to the desired date time
  ## information.
  ##
  ## If `breaks` is given as a sequence of unix timestamps, these will be used over those
  ## computed via `dateSpacing`.
  ##
  ## `isTimestamp` means the corresponding `x` column of the input data is a unix timestamp,
  ## either as an integer or a floating value.
  ##
  ## `parseDate` is required if the data is ``not`` a timestamp. It needs to handle the parsing
  ## of the stored string data in the `x` column to convert it to `DateTime` objects.
  ##
  ## `dateSpacing` is the desired distance between each tick. It is used as a reference
  ## taking into account the given `formatString`. Of all possible ticks allowed by
  ## `formatString` those ticks are used that have the closest distance to `dateSpacing`,
  ## starting with the first tick in the date range that can be represented by `formatString`.
  ##
  ## The `dateAlgo` argument is an experimental argument that should not be required. It changes
  ## the algorithm that is used to determine sensible tick labels based on the given `dateSpacing`.
  ## In the case of `dtaFilter` (default) we compute the parsed dates for all elements in the
  ## date time column first and then attempt to filter out all values to leave those that
  ## match the `dateSpacing`. This works well for densely packed timestamps in a column and
  ## deals better with rounding of e.g. 52 weeks =~= 1 year like tick labels.
  ## `dateAlgo` is overwritten if `breaks` is given.
  ##
  ## For sparser time data, use the `dtaAddDuration` algoritm, which simply determines the
  ## first suitable date based on the format string and adds the `dateSpacing` to each of
  ## these. The next matching date based on the `formatString` is used. This does not handle
  ## rounding of dates well (4 weeks =~= 1 month will produce mismatches at certain points
  ## for example), but should be more robust.
  ##
  ## The `timeZone` can be given for the case where the input is a timestamp. It will then be used
  ## to convert the given input timestamps according to the given timezone.
  # NOTE: because we add this to every linear scale, we can use this in post processing to
  # parse string based dates into unix timestamps with a simple check on `scale.dateScale.isSome`
  if not isTimestamp and parseDate.isNil:
    raise newException(ValueError, "A `DateScale` needs either `isTimestamp = true` " &
      "or a `parseDate` procedure.")
  result = DateScale(name: name,
                     axKind: akX,
                     breaks: breaks,
                     isTimestamp: isTimestamp,
                     parseDate: parseDate,
                     formatString: formatString,
                     dateSpacing: dateSpacing,
                     dateAlgo: if breaks.len == 0: dateAlgo else: dtaCustomBreaks,
                     timeZone: timeZone)

proc scale_y_date*[T: seq[SomeNumber]](
    name: string = "",
    breaks: T = newSeq[float](),
    isTimestamp = false, # if true, `y` column is assumed to be a unix timestamp
    parseDate: proc(x: string): DateTime = nil, # else it should be a string
    formatString: string = "yyyy-MM-dd",
    dateSpacing: Duration = initDuration(days = 1),
    dateAlgo: DateTickAlgorithmKind = dtaFilter,
    timeZone: Timezone = utc()): DateScale =
  ## Creates a continuous `y` axis that generates labels according to the desired date time
  ## information.
  ##
  ## If `breaks` is given as a sequence of unix timestamps, these will be used over those
  ## computed via `dateSpacing`.
  ##
  ## `isTimestamp` means the corresponding `y` column of the input data is a unix timestamp,
  ## either as an integer or a floating value.
  ##
  ## `parseDate` is required if the data is ``not`` a timestamp. It needs to handle the parsing
  ## of the stored string data in the `y` column to convert it to `DateTime` objects.
  ##
  ## `dateSpacing` is the desired distance between each tick. It is used as a reference
  ## taking into account the given `formatString`. Of all possible ticks allowed by
  ## `formatString` those ticks are used that have the closest distance to `dateSpacing`,
  ## starting with the first tick in the date range that can be represented by `formatString`.
  ##
  ## The `dateAlgo` argument is an experimental argument that should not be required. It changes
  ## the algorithm that is used to determine sensible tick labels based on the given `dateSpacing`.
  ## In the case of `dtaFilter` (default) we compute the parsed dates for all elements in the
  ## date time column first and then attempt to filter out all values to leave those that
  ## match the `dateSpacing`. This works well for densely packed timestamps in a column and
  ## deals better with rounding of e.g. 52 weeks =~= 1 year like tick labels.
  ## `dateAlgo` is overwritten if `breaks` is given.
  ##
  ## For sparser time data, use the `dtaAddDuration` algoritm, which simply determines the
  ## first suitable date based on the format string and adds the `dateSpacing` to each of
  ## these. The next matching date based on the `formatString` is used. This does not handle
  ## rounding of dates well (4 weeks =~= 1 month will produce mismatches at certain points
  ## for example), but should be more robust.
  ##
  ## The `timeZone` can be given for the case where the input is a timestamp. It will then be used
  ## to convert the given input timestamps according to the given timezone.
  # NOTE: because we add this to every linear scale, we can use this in post processing to
  # parse string based dates into unix timestamps with a simple check on `scale.dateScale.isSome`
  if not isTimestamp and parseDate.isNil:
    raise newException(ValueError, "A `DateScale` needs either `isTimestamp = true` " &
      "or a `parseDate` procedure.")
  result = DateScale(name: name,
                     axKind: akY,
                     breaks: breaks,
                     isTimestamp: isTimestamp,
                     parseDate: parseDate,
                     formatString: formatString,
                     dateSpacing: dateSpacing,
                     dateAlgo: if breaks.len == 0: dateAlgo else: dtaCustomBreaks,
                     timeZone: timeZone)

proc scale_y_continuous*[
  P: PossibleSecondaryAxis,
  T: int | seq[SomeNumber]](
    name: string = "",
    breaks: T = newSeq[float](),
    secAxis: P = missing(),
    labels: proc(x: float): string = nil,
    trans: proc(x: float): float = nil,
    invTrans: proc(x: float): float = nil): Scale =
  ## Creates a continuous y axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the y axis to the tick label that should be shown for it.
  ##
  ## `breaks` allows to specify either the number of ticks desired (in case
  ## an integer is given) or the exact locations of the ticks given in
  ## units of the data space belonging to this axis.
  ##
  ## Note that the exact number of desired ticks is usually not respected,
  ## rather a close number that yields "nice" tick labels is chosen.
  # Also the possible transformation for the secondary axis is ignored!
  if not trans.isNil and not invTrans.isNil:
    result = Scale(name: name,
                   scKind: scTransformedData,
                   axKind: akY,
                   dcKind: dcContinuous,
                   hasDiscreteness: true,
                   secondaryAxis: secAxis.toOptSecAxis(akX),
                   formatContinuousLabel: labels,
                   trans: trans,
                   invTrans: invTrans)
  elif trans.isNil xor invTrans.isNil:
    raise newException(Exception, "If `scale_y_continuous` is used for a custom " &
      "transformed data scale both the transformation and inverse have to be defined!")
  else:
    result = Scale(name: name,
                   scKind: scLinearData,
                   axKind: akY,
                   dcKind: dcContinuous,
                   hasDiscreteness: true,
                   secondaryAxis: secAxis.toOptSecAxis(akY),
                   formatContinuousLabel: labels)
  result.assignBreaks(breaks)

proc scale_y_discrete*[
  P: PossibleSecondaryAxis](
    name: string = "",
    secAxis: P = missing(),
    labels: proc(x: Value): string = nil
                          ): Scale =
  ## creates a discrete y axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the x axis to the tick label that should be shown for it.
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akY,
                 dcKind: dcDiscrete,
                 hasDiscreteness: true,
                 secondaryAxis: secAxis.toOptSecAxis(akY),
                 formatDiscreteLabel: labels)

proc scale_y_discrete*[
  P: PossibleSecondaryAxis;
  T;
  U](
    name: string = "",
    labels: OrderedTable[T, U],
    secAxis: P = missing(),
                          ): Scale =
  ## creates a discrete x axis with a possible secondary axis.
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akY,
                 dcKind: dcDiscrete,
                 hasDiscreteness: true,
                 secondaryAxis: secAxis.toOptSecAxis(akY),
                 formatDiscreteLabel: labels)
  result.labelSeq = newSeq[Value](labels.len)
  let keys = toSeq(keys(labels))
  for i, k in keys:
    let kVal = %~ k
    result.valueMap[kVal] = ScaleValue(kind: scLinearData, val: %~ labels[kVal])
    result.labelSeq[i] = kVal

proc scale_x_reverse*[
  P: PossibleSecondaryAxis](
    name: string = "",
    secAxis: P = missing(),
    dcKind: DiscreteKind = dcContinuous
                          ): Scale =
  ## creates a continuous x axis with a possible secondary axis, which
  ## is reversed
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akX,
                 dcKind: dcKind,
                 reversed: true,
                 hasDiscreteness: true,
                 secondaryAxis: secAxis.toOptSecAxis(akX))

proc scale_y_reverse*[
  P: PossibleSecondaryAxis](
    name: string = "",
    secAxis: P = missing(),
    dcKind: DiscreteKind = dcContinuous
                          ): Scale =
  ## creates a continuous y axis with a possible secondary axis, which
  ## is reversed
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akY,
                 dcKind: dcKind,
                 reversed: true,
                 hasDiscreteness: true,
                 secondaryAxis: secAxis.toOptSecAxis(akY))

proc scale_fill_log10*[S: ColorScale | seq[uint32] | Missing;
                       T: int | seq[SomeNumber]](
                         breaks: T = newSeq[float](),
                         scale: ginger.Scale = (low: 0.0, high: 0.0),
                         colorScale: S = missing()): Scale =
  ## Sets the fill scale of the plot to a log10 scale.
  ##
  ## `breaks` allows to specify either the number of ticks desired (in case
  ## an integer is given) or the exact locations of the ticks given in
  ## units of the data space belonging to this axis.
  ##
  ## Note that the exact number of desired ticks is usually not respected,
  ## rather a close number that yields "nice" tick labels is chosen.
  ##
  ## If you wish to customize the color scale of the filling, use the `colorScale`
  ## argument here instead of applying another `scale_fill_gradient` as they
  ## will overwrite each other at the moment!
  let trans = proc(v: float): float =
    result = log10(v)
  let invTrans = proc(v: float): float =
    result = pow(10, v)

  result = Scale(col: f{""}, # will be filled when added to GgPlot obj
                 scKind: scFillColor,
                 dcKind: dcContinuous,
                 dataScale: scale,
                 transC: trans,
                 invTransC: invTrans,
                 colorScale: DefaultColorScale,
                 hasDiscreteness: true)
  result.assignBreaks(breaks)
  when S isnot Missing:
    when S is ColorScale:
      result.colorScale = colorScale
    else:
      result.colorScale = ColorScale(name: name, colors: colorScale)

proc scale_fill_continuous*(colormap: ColorScale | seq[uint32] = DefaultColorScale,
                            name: string = "",
                            scale: ginger.Scale = (low: 0.0, high: 0.0)): Scale =
  ## Forces the fill scale to be continuous.
  ##
  ## If a `scale` is given, the fill scale range will be drawn in the given range.
  result = Scale(name: name,
                 scKind: scFillColor,
                 dcKind: dcContinuous,
                 dataScale: scale,
                 hasDiscreteness: true)
  when colormap is ColorScale:
    result.colorScale = colormap
  else:
    result.colorScale = ColorScale(name: name, colors: colormap)


proc scale_fill_discrete*(name: string = ""): Scale =
  ## Forces the fill scale to be discrete.
  result = Scale(name: name,
                 scKind: scFillColor,
                 dcKind: dcDiscrete,
                 hasDiscreteness: true)

proc scale_fill_manual*[T](values: Table[T, Color]): Scale =
  ## allows to set custom fill colors, by handing a table mapping the
  ## keys found in the fill column to colors.
  result = Scale(scKind: scFillColor,
                 hasDiscreteness: true,
                 dcKind: dcDiscrete)
  result.labelSeq = newSeq[Value](values.len)
  let keys = toSeq(keys(values)).sorted
  for i, k in keys:
    let kVal = %~ k
    result.valueMap[kVal] = ScaleValue(kind: scFillColor, color: values[k])
    result.labelSeq[i] = kVal

proc scale_color_continuous*(colormap: ColorScale | seq[uint32] = DefaultColorScale,
                             name: string = "",
                             scale: ginger.Scale = (low: 0.0, high: 0.0)): Scale =
  result = Scale(name: name,
                 scKind: scColor,
                 dcKind: dcContinuous,
                 dataScale: scale,
                 hasDiscreteness: true)
  when colormap is ColorScale:
    result.colorScale = colormap
  else:
    result.colorScale = ColorScale(name: name, colors: colormap)

proc scale_color_manual*[T](values: Table[T, Color]): Scale =
  ## allows to set custom colors, by handing a table mapping the
  ## keys found in the color column to colors.
  result = Scale(scKind: scColor,
                 hasDiscreteness: true,
                 dcKind: dcDiscrete)
  result.labelSeq = newSeq[Value](values.len)
  let keys = toSeq(keys(values)).sorted
  for i, k in keys:
    let kVal = %~ k
    result.valueMap[kVal] = ScaleValue(kind: scColor, color: values[k])
    result.labelSeq[i] = kVal

proc scale_color_gradient*(scale: ColorScale | seq[uint32],
                           name: string = "custom",
                           dataScale: ginger.Scale = (low: 0.0, high: 0.0)): Scale =
  ## Allows to customize the color gradient used for the `color` aesthetic.
  ##
  ## Either call one of:
  ## - `viridis()`, `magma()`, `plasma()`, `inferno()`
  ## as an argument to the `scale` argument or hand your own custom
  ## color scale (as `uint32` colors).
  ##
  ## Construction of `uint32` colors can be done "by hand":
  ## Assuming `alpha`, `r`, `g`, `b` are `uint8` values:
  ##
  ## `let c = alpha shl 24 or r shl 16 or g shl 8 or b`
  ##
  ## or by constructing it as a hex color directly:
  ##
  ## `let c = 0xaa_ff_00_ff'u32` # hex color "#FF00FF" with `AA` alpha
  ##
  ## or finally by converting from a `chroma` or a `stdlib/colors` color.
  ##
  ## Note: `chroma` does not have a type that stores `uint32` colors, so the
  ## conversion must be done by hand. The `stdlib / colors` module uses `int64`
  ## values for whatever reason. For those you can just convert them to `uint32`.
  ## Both of these options provide your typical "named CSS-like" colors.
  ##
  ## The `name` argumnet is only used in case a `seq[uint32]` is given and
  ## is not particularly important.
  result = Scale(scKind: scColor,
                 dcKind: dcContinuous,
                 dataScale: dataScale,
                 hasDiscreteness: true)
  when scale is ColorScale:
    result.colorScale = scale
  else:
    result.colorScale = ColorScale(name: name, colors: scale)

proc scale_color_identity*(col = ""): Scale =
  ## Given a column `col`, will treat the values inside the column as
  ## 'identity values'. I.e. the values will be used for the values
  ## of the associated aesthetic, even if it's a size or color scale.
  ## (in this case the color scale).
  result = Scale(name: col,
                 col: f{ col },
                 scKind: scColor,
                 dataKind: dkSetting)

proc scale_fill_identity*(col = ""): Scale =
  ## Given a column `col`, will treat the values inside the column as
  ## 'identity values'. I.e. the values will be used for the values
  ## of the associated aesthetic, even if it's a size or color scale.
  ## (in this case the fill color scale).
  result = Scale(name: col,
                 col: f{ col },
                 scKind: scFillColor,
                 dataKind: dkSetting)

proc scale_size_identity*(col = ""): Scale =
  ## Given a column `col`, will treat the values inside the column as
  ## 'identity values'. I.e. the values will be used for the values
  ## of the associated aesthetic, even if it's a size or color scale.
  ## (in this case the size scale).
  result = Scale(name: col,
                 col: f{ col },
                 scKind: scSize,
                 dataKind: dkSetting)

proc scale_alpha_identity*(col = ""): Scale =
  ## Given a column `col`, will treat the values inside the column as
  ## 'identity values'. I.e. the values will be used for the values
  ## of the associated aesthetic, even if it's a size or color scale.
  ## (in this case the alpha scale).
  result = Scale(name: col,
                 col: f{ col },
                 scKind: scAlpha,
                 dataKind: dkSetting)

proc scale_fill_gradient*(scale: ColorScale | seq[uint32],
                          name: string = "custom",
                          dataScale: ginger.Scale = (low: 0.0, high: 0.0),
                         ): Scale =
  ## Allows to customize the color gradient used for the `color` aesthetic.
  ##
  ## Either call one of:
  ## - `viridis()`, `magma()`, `plasma()`, `inferno()`
  ## as an argument to the `scale` argument or hand your own custom
  ## color scale (as `uint32` colors).
  ##
  ## Construction of `uint32` colors can be done "by hand":
  ## Assuming `alpha`, `r`, `g`, `b` are `uint8` values:
  ##
  ## `let c = alpha shl 24 or r shl 16 or g shl 8 or b`
  ##
  ## or by constructing it as a hex color directly:
  ##
  ## `let c = 0xaa_ff_00_ff'u32` # hex color "#FF00FF" with `AA` alpha
  ##
  ## or finally by converting from a `chroma` or a `stdlib/colors` color.
  ##
  ## Note: `chroma` does not have a type that stores `uint32` colors, so the
  ## conversion must be done by hand. The `stdlib / colors` module uses `int64`
  ## values for whatever reason. For those you can just convert them to `uint32`.
  ## Both of these options provide your typical "named CSS-like" colors.
  ##
  ## The `name` argument is only used in case a `seq[uint32]` is given and
  ## is not particularly important.
  result = Scale(scKind: scFillColor,
                 dcKind: dcContinuous,
                 dataScale: dataScale,
                 hasDiscreteness: true)
  when scale is ColorScale:
    result.colorScale = scale
  else:
    result.colorScale = ColorScale(name: name, colors: scale)

proc scale_size_manual*[T](values: Table[T, float]): Scale =
  ## allows to set custom sizes, by handing a table mapping the
  ## keys found in the size column to sizes.
  result = Scale(scKind: scSize,
                 hasDiscreteness: true,
                 dcKind: dcDiscrete)
  result.labelSeq = newSeq[Value](values.len)
  let keys = toSeq(keys(values)).sorted
  var min: float
  var max: float
  for i, k in keys:
    let kVal = %~ k
    let val = values[k]
    result.valueMap[kVal] = ScaleValue(kind: scSize, size: val)
    result.labelSeq[i] = kVal
    min = min(val, min)
    max = max(val, max)
  # set the size range, not really needed, but good for sanity
  result.sizeRange = (low: min, high: max)

proc scale_size_discrete*(sizeRange = DefaultSizeRange): Scale =
  ## Allows set the `size` scale to discrete values & optionally set the
  ## range of allowed values in `sizeRange`.
  result = Scale(scKind: scSize,
                 hasDiscreteness: true,
                 dcKind: dcDiscrete,
                 sizeRange: sizeRange)

proc scale_size_continuous*(sizeRange = DefaultSizeRange): Scale =
  ## Allows set the `size` scale to continuous values & optionally set the
  ## range of allowed values in `sizeRange`.
  result = Scale(scKind: scSize,
                 hasDiscreteness: true,
                 dcKind: dcContinuous,
                 sizeRange: sizeRange)

proc scale_alpha_discrete*(alphaRange = DefaultAlphaRange): Scale =
  ## Allows set the `alpha` scale to discrete values & optionally set the
  ## range of allowed values in `alphaRange`.
  result = Scale(scKind: scAlpha,
                 hasDiscreteness: true,
                 dcKind: dcDiscrete,
                 alphaRange: alphaRange)

proc scale_alpha_continuous*(alphaRange = DefaultAlphaRange): Scale =
  ## Allows set the `alpha` scale to continuous values & optionally set the
  ## range of allowed values in `alphaRange`.
  result = Scale(scKind: scAlpha,
                 hasDiscreteness: true,
                 dcKind: dcContinuous,
                 alphaRange: alphaRange)

proc ggtitle*(title: string, subTitle = "",
              titleFont = font(), subTitleFont = font(8.0)): Theme =
  result = Theme(title: some(title))
  if subTitle.len > 0:
    result.subTitle = some(subTitle)
  if titleFont != font():
    result.titleFont = some(titleFont)
  if subTitleFont != font():
    result.subTitleFont = some(subTitleFont)

proc generateLegendMarkers(plt: Viewport, scale: Scale,
                           theme: Theme,
                           geomKind: GeomKind): seq[GraphObject]
proc genDiscreteLegend(view: var Viewport,
                       cat: Scale,
                       theme: Theme,
                       geomKind: GeomKind) =
  # TODO: add support for legend font in Theme / `let label` near botton!
  # _______________________
  # |   | Headline        |
  # |______________________
  # |1cm| 1cm |  | space  |
  # |   |grad.|.5| for    |
  # |   |     |cm| leg.   |
  # |   |     |  | labels |
  # -----------------------
  let markers = view.generateLegendMarkers(cat, theme, geomKind)
  let numElems = cat.valueMap.len
  ## XXX: Still need to be scaled by `baseScale`!
  let bScale = theme.baseScale.get(1.0)
  let width = theme.discreteLegendWidth.get(1.0) * bScale
  let height = theme.discreteLegendHeight.get(1.0) * bScale
  let legendHeaderHeight = if theme.legendTitleFont.isSome: theme.legendTitleFont.get.size / 12.0 else: 1.0
  let spacingOffset = 0.05 * bScale
  let spacing = (1 + spacingOffset) * height
  let totalBlockHeight = spacing * numElems.float

  view.layout(2, 2,
              colWidths = @[quant(0.5 * bScale, ukCentimeter), # for space to plot
                            quant(0.0, ukRelative)], # for legend. incl header
              rowHeights = @[quant(legendHeaderHeight, ukCentimeter), # for header
                             quant(totalBlockHeight, ukCentimeter)],
              ignoreOverflow = true) # for act. legend
  # now set the `height` according to the real legend height. This important
  # to get proper alignment of the scale / multiple scales in `finalizeLegend`!
  view.height = quant(legendHeaderHeight + totalBlockHeight, ukCentimeter)
  var leg = view[3]

  let rH = newSeqWith(numElems, quant(spacing, ukCentimeter))

  leg.layout(3, rows = numElems,
             colWidths = @[quant(width, ukCentimeter),
                           quant(0.3 * bScale, ukCentimeter),
                           quant(0.0, ukRelative)],
             rowHeights = rH)
  # iterate only over added children, skip first, because we actual legend first
  var j = 0

  for i in countup(0, leg.children.len - 1, 3):
    # create rectangle showing style of data points
    var legBox = leg[i]
    var legLabel = leg[i + 2]
    #let viewRatio = ch.hView.val / ch.wView.val
    let style = Style(lineType: ltSolid,
                      lineWidth: 1.0,
                      color: color(1.0, 1.0, 1.0),
                      fillColor: grey92)
    let rect = legBox.initRect(Coord(x: c1(0.0),
                                     y: c1(0.0) + legBox.c1(spacingOffset / 2.0, akY, ukCentimeter)),
                               quant(width, ukCentimeter),
                               quant(height, ukCentimeter),
                               style = some(style),
                               name = "markerRectangle")
    # add marker ontop of rect
    var labelText = ""
    case cat.scKind
    of scColor, scFillColor, scShape, scSize:
      labelText = markers[j].name
    else:
      raise newException(Exception, "`createLegend` unsupported for " & $cat.scKind)
    let legendFont = theme.legendFont.get(font(12.0))
    let label = legLabel.initText(
      Coord(
        x: c1(0.0),
        y: c1(0.5)),
      labelText,
      textKind = goText,
      alignKind = taLeft,
      font = some(legendFont),
      name = "markerText"
    )
    legBox.addObj [rect, markers[j]]
    legLabel.addObj label
    leg[i] = legBox
    leg[i + 2] = legLabel
    inc j
  view[3] = leg

proc genContinuousLegend(view: var Viewport,
                         cat: Scale,
                         theme: Theme,
                         geomKind: GeomKind) =
  case cat.scKind
  of scSize:
    view.layout(1, rows = 5 + 1)
  of scColor, scFillColor:
    # create following legend layout
    # _______________________
    # |   | Headline        |
    # |______________________
    # |1cm| 1cm |  | space  |
    # |   |grad.|.5| for    |
    # |   |     |cm| leg.   |
    # |   |     |  | labels |
    # -----------------------
    let bScale = theme.baseScale.get(1.0)
    let height = theme.continuousLegendHeight.get(4.5)
    let legendHeaderHeight = if theme.legendTitleFont.isSome: theme.legendTitleFont.get.size / 12.0 else: 1.0
    let legendHeight = if theme.legendFont.isSome: theme.legendFont.get.size / 12.0 * height else: height
    view.layout(2, 2,
                colWidths = @[quant(0.5 * bScale, ukCentimeter), # for space to plot
                              quant(0.0, ukRelative)], # for legend. incl header
                rowHeights = @[quant(legendHeaderHeight, ukCentimeter), # for header
                               quant(legendHeight, ukCentimeter)]) # for act. legend
    var legView = view[3] # bottom right
    legView.yScale = cat.dataScale
    let width = theme.continuousLegendWidth.get(1.0)
    legView.layout(3, 1, colWidths = @[quant(width * bScale, ukCentimeter),
                                       quant(0.5 * bScale, ukCentimeter),
                                       quant(0.0, ukRelative)])
    var legGrad = legView[0]
    # add markers
    let markers = legGrad.generateLegendMarkers(cat, theme, geomKind)
    legGrad.addObj markers
    let cmap = cat.colorScale
    let colors = cmap.colors.mapIt(it.toColor)
    let cc = some(Gradient(colors: colors))
    let gradRect = legGrad.initRect(c(0.0, 0.0),
                                    quant(1.0, ukRelative),
                                    quant(1.0, ukRelative),
                                    name = "legendGradientBackground",
                                    gradient = cc)
    legGrad.addObj gradRect
    legView[0] = legGrad
    view[3] = legView

    let totalHeight = legendHeaderHeight + legendHeight
    view.height = quant(totalHeight, ukCentimeter)
  else:
    discard

proc createLegend(view: var Viewport,
                  cat: Scale,
                  theme: Theme,
                  geomKind: GeomKind) =
  ## creates a full legend within the given viewport based on the categories
  ## in `cat` with a headline `title` showing data points of `markers`
  let startIdx = view.len
  case cat.dcKind
  of dcDiscrete:
    view.genDiscreteLegend(cat, theme, geomKind)
  of dcContinuous:
    # for now 5 sizes...
    view.genContinuousLegend(cat, theme, geomKind)

  # get the first viewport for the header
  if startIdx < view.len:
    var header = view[1]
    # TODO: add support to change font of legend
    let legendTitleFont = theme.legendTitleFont.get(font(12.0, bold = true))
    var label = header.initText(
      Coord(x: c1(0.0),
            y: c1(0.5)),
      evaluate(cat.col).toStr,
      textKind = goText,
      alignKind = taLeft,
      font = some(legendTitleFont),
      name = "legendHeader")
    header.addObj label
    view[1] = header

proc finalizeLegend(view: var Viewport,
                    legends: seq[Viewport]) =
  ## finalizes the full legend from the given seq of legends
  ## such that the spacing between them is even
  # generate such layout
  # _________________
  # | relative space |
  # ------------------
  # | Legend 1       |
  # ------------------
  # | Legend 2       |
  # ------------------
  # ...
  # | relative space |
  # ------------------
  # calc number of spacings between legends
  var rowHeights = @[quant(0.0, ukRelative)]
  for i, l in legends:
    rowHeights.add l.height
  rowHeights.add quant(0.0, ukRelative)
  view.layout(1, rowHeights.len, rowHeights = rowHeights,
              ignoreOverflow = true)
  for i in countup(1, rowHeights.len - 2):
    var ml = legends[i - 1]
    ml.origin = view[i].origin
    view[i] = ml

proc legendFont*[F1: PossibleFont; F2: PossibleFont](font: F1 = missing(), tickFont: F2 = missing()): Theme =
  ## Adjusts the font of the legend title and / or the legend tick label font.
  result = Theme(legendTitleFont: toOptFont(font), legendFont: toOptFont(tickFont))

proc legendPosition*(x = 0.0, y = 0.0): Theme =
  ## puts the legend at position `(x, y)` in relative coordinates of
  ## the plot viewport in range (0.0 .. 1.0)
  result = Theme(legendPosition: some(c(x, y)))

proc continuousLegendHeight*(height: float): Theme =
  ## Sets the height of a continuous legend to this height in Centimeter
  ##
  ## Default is 4.5 cm.
  result = Theme(continuousLegendHeight: some(height))

proc continuousLegendWidth*(width: float): Theme =
  ## Sets the width of a continuous legend to this width in Centimeter
  ##
  ## Default is 1.0 cm.
  result = Theme(continuousLegendWidth: some(width))

proc discreteLegendHeight*(height: float): Theme =
  ## Sets the height of a discrete legend to this height in Centimeter.
  ##
  ## This is the height of a single block.
  ##
  ## Default is 1.0 cm.
  result = Theme(discreteLegendHeight: some(height))

proc discreteLegendWidth*(width: float): Theme =
  ## Sets the width of a discrete legend to this width in Centimeter
  ##
  ## This is the width of a single block.
  ##
  ## Default is 1.0 cm.
  result = Theme(discreteLegendWidth: some(width))

proc legendOrder*(idx: seq[int]): Theme =
  ## uses the ordering given by the indices `idx` to arrange the order of
  ## the label.
  ## `idx` needs to have as many elements as there are legend entries.
  ## The default ordering is lexical ordering. Any custom ordering is a
  ## custom permutation of that.
  ## TODO: instead of this the legend creation needs to be refactored!
  ## This is an experimental, untested feature. A better solution that does
  ## not require the user to be keenly aware of the "correct" order of the
  ## legend is required. For the time being this is better than nothing though.
  result = Theme(legendOrder: some(idx))

proc titlePosition*(x = 0.0, y = 0.0): Theme =
  ## puts the title at position `(x, y)` in relative coordinates of
  ## the title viewport in range (0.0 .. 1.0). The title viewport is the
  ## region above the plot
  ##
  ## |   | Title viewport |        |
  ## |   |----------------|        |
  ## | y |     Plot       | legend |
  ## |   |________________|        |
  ## |   |       x        |        |
  result = Theme(titlePosition: some(c(x, y)))

proc hideLegend*(): Theme =
  ## hides the legend, even if it would otherwise be required
  result = Theme(hideLegend: some(true))

proc hideLabels*(): Theme = Theme(hideLabels: some(true))
proc hideXLabels*(): Theme = Theme(hideXLabels: some(true))
proc hideYLabels*(): Theme = Theme(hideYLabels: some(true))
proc hideTicks*(): Theme = Theme(hideTicks: some(true))
proc hideTickLabels*(): Theme = Theme(hideTickLabels: some(true))
proc hideXTickLabels*(): Theme = Theme(hideXTickLabels: some(true))
proc hideYTickLabels*(): Theme = Theme(hideYTickLabels: some(true))

func facetHeaderText*[F: PossibleFont](font: F = missing(), x = 0.5, y = 0.5): Theme =
  ## Adjusts the facet header text. The `f` argument adjusts the font used
  ## while the `x`, `y` adjusts the placement of the text in relative coordinates
  ## within the header of each facet. The default is (0.5, 0.5), i.e. the  center.
  let fontOpt = when F is Missing: font(8.0, alignKind = taCenter)
                else: font
  result = Theme(facetHeaderFont: some(fontOpt), facetHeaderPos: some(c(x, y)))

proc canvasColor*[C: PossibleColor](color: C): Theme =
  ## sets the canvas color of the plot to the given color
  let colorOpt = toOptColor(color)
  result = Theme(canvasColor: colorOpt)

func theme_opaque*(): Theme =
  ## returns the "opaque" theme. For the time being this only means the
  ## canvas of the plot is white, which is the default starting from version
  ## `v0.4.0`. For the old behavior add `theme_transparent`.
  result = Theme(canvasColor: some(white))

func theme_transparent*(): Theme =
  ## returns the "transparent" theme. This is the default for plots before
  ## version `v0.4.0`.
  result = Theme(canvasColor: some(transparent))

func theme_void*[C: PossibleColor](color: C = white): Theme =
  ## returns the "void" theme. This means:
  ## - white background
  ## - no grid lines
  ## - no ticks
  ## - no tick labels
  ## - no labels
  let colorOpt = toOptColor(color)
  result = Theme(canvasColor: colorOpt,
                 plotBackgroundColor: colorOpt,
                 hideTicks: some(true),
                 hideTickLabels: some(true),
                 hideLabels: some(true))

func gridLines*[C: PossibleColor](enable = true, width = Inf,
                                  color: C = white,
                                  onlyAxes = false
                                 ): Theme =
  ## Adds major grid lines to a plot
  ##
  ## If width `!= Inf` will use the value, else default of 1pt.
  ##
  ## The `color` may also be changed with this proc if multiple changes are to be
  ## made.
  ##
  ## If `onlyAxes` is true and `enable` is `false`, we will *only* draw the actual
  ## axes and no grid lines.
  let colorOpt = toOptColor(color)
  result = Theme(gridLines: some(enable),
                 gridLineColor: colorOpt,
                 onlyAxes: some(onlyAxes))
  if classify(width) == fcNormal:
    result.gridLineWidth = some(width)

func minorGridLines*(enable = true, width = Inf): Theme =
  ## Adds minor grid lines to a plot (i.e. between the major grid lines of half width)
  ##
  ## If width is `!= Inf` will use the given width. Else will compute to half the width of
  ## the major lines.
  result = Theme(minorGridLines: some(enable))
  if classify(width) == fcNormal:
    result.minorGridLineWidth = some(width)

func tickLength*(length: float): Theme =
  ## Sets the tick length to this many points. The default is 5 pixels at
  ## 640x480. The tick width is 1/5 of the tick length
  result = Theme(tickLength: some(length))

func tickWidth*(width: float): Theme =
  ## Sets the tick width to this many points. The default is 1 pixels at
  ## 640x480 and it is 1/5 of the tick length.
  result = Theme(tickWidth: some(width))

func tickColor*[C: PossibleColor](color: C = black): Theme =
  ## Sets the tick color to this many points. The default is 1 pixels at
  ## 640x480 and it is 1/5 of the tick length.
  let colorOpt = toOptColor(color)
  result = Theme(tickColor: colorOpt)

func tickKind*(kind: TickKind): Theme =
  ## Sets the kind of tick. One sided `tkOneSide` (default) or both sides `tkBothSides`.
  result = Theme(tickKind: some(kind))

func backgroundColor*[C: PossibleColor](color: C = grey92): Theme =
  ## Sets the background color of the plotting area to `color`.
  let colorOpt = toOptColor(color)
  result = Theme(plotBackgroundColor: colorOpt)

func gridLineColor*[C: PossibleColor](color: C = white): Theme {.deprecated: "Use the `gridLines` procedure to set " &
  "the grid line color among other things.".} =
  ## Sets the color of the grid lines.
  let colorOpt = toOptColor(color)
  result = Theme(gridLineColor: colorOpt)

func default_scale*(): Theme =
  result = Theme(titleFont: some(font(16.0)),
                 labelFont: some(font(12.0)),
                 tickLabelFont: some(font(8.0)),
                 tickLength: some(5.0),
                 tickWidth: some(1.0),
                 gridLineWidth: some(1.0),
                 legendFont: some(font(12.0)),
                 legendTitleFont: some(font(12.0, bold = true)),
                 facetHeaderFont: some(font(8.0, alignKind = taCenter)),
                 baseLabelMargin: some(0.3),
                 baseScale: some(1.0))

proc theme_scale*(scale: float, family = "", baseTheme: (proc(): Theme) = nil): Theme =
  ## Returns a theme that scales all fonts, tick sizes etc. by the given factor compared
  ## to the default values.
  ##
  ## If `family` given will overwrite the font family of all fonts to this.
  ##
  ## `baseTheme` is a procedure which is called that yields the base sizes and settings
  ## to scale. I.e. an equivalent of `default_scale` above.
  result = if baseTheme != nil: baseTheme() else: default_scale()
  proc `*`(x: Option[float], s: float): Option[float] =
    if x.isSome:
      result = some(x.get * s)
  proc `*`(x: Option[Font], s: float): Option[Font] =
    if x.isSome:
      let f = x.get
      let fam = if family.len > 0: family else: f.family
      result = some(font(f.size * s, bold = f.bold, family = fam, alignKind = f.alignKind))
  result.titleFont = result.titleFont * scale
  result.labelFont = result.labelFont * scale
  result.tickLabelFont = result.tickLabelFont * scale
  result.tickLength = result.tickLength * scale
  result.tickWidth = result.tickWidth * scale
  result.gridLineWidth = result.gridLineWidth * scale
  result.legendFont = result.legendFont * scale
  result.legendTitleFont = result.legendTitleFont * scale
  result.facetHeaderFont = result.facetHeaderFont * scale
  result.annotationFont = result.annotationFont * scale
  result.baseLabelMargin = result.baseLabelMargin * scale
  result.baseScale = result.baseScale * scale

proc theme_font_scale*(scale: float, family = "", baseTheme: (proc(): Theme) = nil): Theme =
  ## Returns a theme similar to `theme_scale` but in which the margins are not
  ## scaled.
  ##
  ## The margins can be scaled optionally using `baseScale` to a custom value.

  let bt = if baseTheme == nil: default_scale()
           else: baseTheme()
  result = theme_scale(scale, family, baseTheme)
  result.baseScale = bt.baseScale

func sideBySide*(): Theme =
  result = Theme(titleFont: some(font(9.0)),
                 labelFont: some(font(9.0)),
                 tickLabelFont: some(font(7.0)),
                 tickLength: some(5.0),
                 tickWidth: some(1.0),
                 gridLineWidth: some(1.0),
                 legendFont: some(font(7.0)),
                 legendTitleFont: some(font(7.0, bold = true)),
                 facetHeaderFont: some(font(7.0, alignKind = taCenter)),
                 baseLabelMargin: some(0.25),
                 annotationFont: some(font(7.0, family = "monospace")),
                 baseScale: some(1.25)) # won't be scaled!

func singlePlot*(): Theme =
  result = Theme(titleFont: some(font(10.0)),
                 labelFont: some(font(10.0)),
                 tickLabelFont: some(font(9.0)),
                 tickLength: some(5.0),
                 tickWidth: some(1.0),
                 gridLineWidth: some(1.0),
                 legendFont: some(font(9.0)),
                 legendTitleFont: some(font(9.0, bold = true)),
                 facetHeaderFont: some(font(9.0, alignKind = taCenter)),
                 baseLabelMargin: some(0.35),
                 annotationFont: some(font(9.0, family = "monospace")),
                 baseScale: some(1.0)) # won't be scaled!

proc toTeXOptions*(useTeX, onlyTikZ, standalone: bool, texTemplate: string,
                   caption, label, placement: string): TeXOptions =
  result = TeXOptions(
      texTemplate: if texTemplate.len > 0: some(texTemplate)
                   else: none(string),
      standalone: standalone,
      onlyTikZ: onlyTikZ,
      useTeX: useTeX,
      caption: if caption.len > 0: some(caption) else: none(string),
      label: if label.len > 0: some(label) else: none(string),
      placement: placement
  )

proc themeLatex*(fWidth: float, width: float,
                 baseTheme: (proc(): Theme),
                 height = -1.0, ratio = -1.0,
                 textWidth = 458.29268, # 455.24411
                 useTeX = true,
                 texOptions = toTeXOptions(true, false, true, "", "", "", ""),
                 useWithoutTeX = false
                ): Theme =
  ## This is a good theme to use in LaTeX documents. It handles scaling the
  ## plot to the desired size, where `fWidth` is the size it will be inserted
  ## by into the TeX document via
  ##
  ## `\includegraphics[width=fWidth\textwidth]{...}`
  ##
  ## and `width`, `height` or `ratio` for the produced PDF in pixel. The sizes
  ## are scaled according to a text width corresponding to an A4 paper at a margin
  ## of 2.5cm on each side, resulting in 455.24411pt (TeX `pt`!) in width for
  ## a full `\textwidth` (determine for your document using `\the\textwidth`).
  ##
  ## For KOMAscript A4 scrbook with DIV=14, BOC=5mm it yields 458.29268pt.
  ## The `textheight` comes out to 677.3971pt yielding a ratio of 1.47808841285.
  ##
  ## If a ratio is given it is interpreted as ``width to height``,
  ## i.e. ``height = width / ratio``.
  ## Text width / line width in `bp` (TeX pixels of 72 dpi)
  ##
  ## If `useTeX` is `true`, we assign the `TeXOptions` for a standalone PDF using the
  ## TikZ backend (or the `texOptions` given). If `useWithoutTeX` is true, we *also*
  ## apply the same theme even on the non TeX backend (i.e. `useTeX = false`).
  # rescale text width to `bp` (72 DPI pixels)
  let textWidth = textWidth / 72.27 * 72.0
  const goldenRatio = (sqrt(5.0) + 1.0) / 2.0 # Aesthetic ratio, φ
  var h = 0.0
  if height < 0.0 and ratio < 0.0:
    echo "[INFO]: No plot ratio given, using golden ratio."
    h = width / goldenRatio
  elif height > 0.0:
    h = height
  else:
    h = height / ratio

  # with width and height adjust the scale factor to use for all fonts
  # For a `textWidth` wide plot then, a font size of 11 pt would be exactly
  # like 11pt in the TeX document itself. So at e.g. 600 `width`, it needs to
  # be scaled up by that ratio
  # `factor` used as `fontSize * factor` to get final font size
  var factor = width / textWidth
  # scale by the fractional width on the page (e.g. `fWidth = 0.5` for 50%
  # of `\textwidth`:
  factor /= fWidth
  # We hand a custom `baseTheme`
  if useTeX or useWithoutTeX:
    result = theme_font_scale(factor, baseTheme = baseTheme)
    result.width = some(width)
    result.height = some(h) # Note: use `h`!

  if useTeX: ## `useTeX` allows to disable it easily
    result.texOptions = some(texOptions)

proc tomlTheme*(fname: string): Theme =
  ## Reads a custom theme at runtime from a TOML file
  result = parseTheme(fname.expandTilde)

func coord_fixed*(ratio: float): Theme =
  ## Produces a plot where the ratio of the plot itself is the given ratio.
  ## Useful to produce a plot where x and y axes have a scale of e.g. 1:1 or 1:2.
  result = Theme(fixedRatio: some(ratio))

func baseLabelMargin*(margin: float): Theme =
  ## Base label margin between the width / height of a tick label and the
  ## axis label. By default 0.3 cm (added to the width / height of the tick labels).
  result = Theme(baseLabelMargin: some(margin))

proc prefer_columns*(): Theme =
  ## Sets the preference in a facet to be num(cols) > num(rows)
  result = Theme(preferRowsOverColumns: some(false))

proc prefer_rows*(): Theme =
  ## Sets the preference in a facet to be num(rows) > num(cols)
  result = Theme(preferRowsOverColumns: some(true))

proc parseTextAlignString(alignTo: string): Option[TextAlignKind] =
  case alignTo.normalize
  of "none": result = none[TextAlignKind]()
  of "left": result = some(taLeft)
  of "right": result = some(taRight)
  of "center": result = some(taCenter)
  else: result = none[TextAlignKind]()

proc xlab*(
  label = "", margin = NaN, rotate = NaN,
  alignTo = "none", font = font(), tickFont = font(),
  tickMargin = NaN,
  tickLength = NaN): Theme =
  if label.len > 0:
    result.xLabel = some(label)
  if classify(margin) != fcNan:
    result.xLabelMargin = some(margin)
  if classify(tickMargin) != fcNan:
    result.xTickLabelMargin = some(tickMargin)
  if classify(tickLength) != fcNan:
    result.tickLength = some(tickMargin)
  if classify(rotate) != fcNan:
    result.xTicksRotate = some(rotate)
  if font != font():
    result.labelFont = some(font)
  if tickFont != font():
    result.tickLabelFont = some(tickFont)
  result.xTicksTextAlign = parseTextAlignString(alignTo)

proc ylab*(
  label = "", margin = NaN, rotate = NaN,
  alignTo = "none", font = font(), tickFont = font(),
  tickMargin = NaN,
  tickLength = NaN): Theme =
  if label.len > 0:
    result.yLabel = some(label)
  if classify(margin) != fcNan:
    result.yLabelMargin = some(margin)
  if classify(tickMargin) != fcNan:
    result.yTickLabelMargin = some(tickMargin)
  if classify(tickLength) != fcNan:
    result.tickLength = some(tickMargin)
  if classify(rotate) != fcNan:
    result.yTicksRotate = some(rotate)
  if font != font():
    result.labelFont = some(font)
  if tickFont != font():
    result.tickLabelFont = some(font)
  result.yTicksTextAlign = parseTextAlignString(alignTo)

func xlim*[T, U: SomeNumber](low: T, high: U, outsideRange = ""): Theme =
  ## Sets the limits of the plot range in data scale. This overrides the
  ## calculation of the data range, which by default is just
  ## `(min(dataX), max(dataX))` while ignoring `inf` values.
  ## If the given range is smaller than the actual underlying data range,
  ## `outsideRange` decides how data outside the range is treated.
  ##
  ## Supported values are `"clip"`, `"drop"` and `"none"`:
  ## - `"clip"`: clip all larger values (e.g. `inf` or all values larger than a
  ##   user defined `xlim`) to limit + xMargin (see below).
  ## - `"drop"`: remove all values larger than range
  ## - `"none"`: leave as is. Might result in values outside the plot area. Also `-inf`
  ##   values may be shown as large positive values. This is up to the drawing backend!
  ## It defaults to `"clip"`.
  ##
  ## Be aware however that the given limit is still subject to calculation of
  ## sensible tick values. The algorithm tries to make the plot start and end
  ## at "nice" values (either 1/10 or 1/4 steps). Setting the limit to some
  ## arbitrary number may not result in the expected plot. If a limit is to be
  ## forced, combine this with `xMargin`! (Note: if for some reason you want more
  ## control over the precise limits, please open an issue).
  ##
  ## NOTE: for a discrete axis the "data scale" is (0.0, 1.0). You can change
  ## it here, but it will probably result in an ugly plot!
  let orOpt = if outsideRange.len > 0: some(parseEnum[OutsideRangeKind](outsideRange))
              else: none[OutsideRangeKind]()
  let range = if low.float != high.float: some((low: low.float, high: high.float)) else: none(ginger.Scale)
  result = Theme(xRange: range, xOutsideRange: orOpt)

func ylim*[T, U: SomeNumber](low: T, high: U, outsideRange = ""): Theme =
  ## Sets the limits of the plot range in data scale. This overrides the
  ## calculation of the data range, which by default is just
  ## `(min(dataY), max(dataY))` while ignoring `inf` values.
  ## If the given range is smaller than the actual underlying data range,
  ## `outsideRange` decides how data outside the range is treated.
  ##
  ## Supported values are `"clip"`, `"drop"` and `"none"`:
  ## - `"clip"`: clip all larger values (e.g. `inf` or all values larger than a
  ##   user defined `ylim`) to limit + yMargin (see below).
  ## - `"drop"`: remove all values larger than range
  ## - `"none"`: leave as is. Might result in values outside the plot area. Also `-inf`
  ##   values may be shown as large positive values. This is up to the drawing backend!
  ## It defaults to `"clip"`.
  ##
  ## Be aware however that the given limit is still subject to calculation of
  ## sensible tick values. The algorithm tries to make the plot start and end
  ## at "nice" values (either 1/10 or 1/4 steps). Setting the limit to some
  ## arbitrary number may not result in the expected plot. If a limit is to be
  ## forced, combine this with `yMargin`! (Note: if for some reason you want more
  ## control over the precise limits, please open an issue).
  ##
  ## NOTE: for a discrete axis the "data scale" is (0.0, 1.0). You can change
  ## it here, but it will probably result in an ugly plot!
  let orOpt = if outsideRange.len > 0: some(parseEnum[OutsideRangeKind](outsideRange))
              else: none[OutsideRangeKind]()
  let range = if low.float != high.float: some((low: low.float, high: high.float)) else: none(ginger.Scale)
  result = Theme(yRange: range, yOutsideRange: orOpt)

proc xMargin*[T: SomeNumber](margin: T, outsideRange = ""): Theme =
  ## Sets a margin on the ``plot data scale`` for the X axis relative to the
  ## full data range. `margin = 0.05` extends the data range by 5 % of the
  ## difference of `xlim.high - xlim.low` (see `xlim` proc) on the left
  ## and right side.
  ## `outsideRange` determines the behavior of all points which lie outside the
  ## plot data range. If not set via `xlim` the plot data range is simply the
  ## full range of all x values, ignoring all `inf` values.
  ## Supported values are `"clip"`, `"drop"` and `"none"`:
  ## - `"clip"`: clip all larger values (e.g. `inf` or all values larger than a
  ##   user defined `xlim`) to limit + xMargin.
  ## - `"drop"`: remove all values larger than range
  ## - `"none"`: leave as is. Might result in values outside the plot area. Also `-inf`
  ##   values may be shown as large positive values. This is up to the drawing backend!
  ## It defaults to `"clip"`.
  ##
  ## NOTE: negative margins are not supported at the moment! They would result in
  ## ticks and labels outside the plot area.
  if margin.float < 0.0:
    raise newException(ValueError, "Margins must be positive! To make the plot " &
      "range smaller use `xlim`!")
  let orOpt = if outsideRange.len > 0: some(parseEnum[OutsideRangeKind](outsideRange))
              else: none[OutsideRangeKind]()
  result = Theme(xMargin: some(margin.float),
                 xOutsideRange: orOpt)

proc yMargin*[T: SomeNumber](margin: T, outsideRange = ""): Theme =
  ## Sets a margin on the ``plot data scale`` for the Y axis relative to the
  ## full data range. `margin = 0.05` extends the data range by 5 % of the
  ## difference of `ylim.high - ylim.low` (see `ylim` proc) on the top
  ## and bottom side.
  ## `outsideRange` determines the behavior of all points which lie outside the
  ## plot data range. If not set via `ylim` the plot data range is simply the
  ## full range of all y values, ignoring all `inf` values.
  ## Supported values are `"clip"`, `"drop"` and `"none"`:
  ## - `"clip"`: clip all larger values (e.g. `inf` or all values larger than a
  ##   user defined `ylim`) to limit + yMargin.
  ## - `"drop"`: remove all values larger than range
  ## - `"none"`: leave as is. Might result in values outside the plot area. Also `-inf`
  ##   values may be shown as large positive values. This is up to the drawing backend!
  ## It defaults to `"clip"`.
  ##
  ## NOTE: negative margins are not supported at the moment! They would result in
  ## ticks and labels outside the plot area.
  if margin.float < 0.0:
    raise newException(ValueError, "Margins must be positive! To make the plot " &
      "range smaller use `ylim`!")
  let orOpt = if outsideRange.len > 0: some(parseEnum[OutsideRangeKind](outsideRange))
              else: none[OutsideRangeKind]()
  result = Theme(yMargin: some(margin.float),
                 yOutsideRange: orOpt)

proc margin*[T: string | UnitKind](left, right, top, bottom = NaN,
                                   unit: T = ukCentimeter): Theme =
  ## Sets the margin around the actual plot. By default the given values are
  ## interpreted as `cm`. This can be changed using the `unit` argument, either
  ## directly as a `UnitKind` (`ukCentimeter`, `ukInch`, `ukPoint`) or as a
  ## string. Allowed string inputs:
  ## - cm: margin quantity in centimeter
  ## - in, inch: margin quantity in inch
  ## - pt, point, px, pixel: margin quantity in points
  ## - r, rel, relative: margin quantity as relative values
  when T is string:
    let unitKind = case unit.normalize
                   of "cm": ukCentimeter
                   of "in", "inch": ukInch
                   of "pt", "point", "px", "pixel": ukPoint
                   of "r", "rel", "relative": ukRelative
                   else: ukCentimeter
  else:
    let unitKind = unit
  proc noneIfNan(f: float, unit: UnitKind): Option[Quantity] =
    result = if classify(f) != fcNan: some(quant(f, unit))
             else: none(Quantity)

  result = Theme(plotMarginLeft: noneIfNan(left, unitKind),
                 plotMarginRight: noneIfNan(right, unitKind),
                 plotMarginTop: noneIfNan(top, unitKind),
                 plotMarginBottom: noneIfNan(bottom, unitKind))

proc facetMargin*[T: Quantity | SomeNumber](margin: T, quantityKind = ukCentimeter): Theme =
  ## Sets the margin around each subplot when using faceting. The value
  ## can either be given directly as a `Quantity`, in which case the user
  ## has control over absolute / relative quantities or as a number. In the
  ## latter case the number is interpreted in centimeter!
  var m: Quantity
  when T is SomeNumber:
    m = quant(margin, quantityKind)
  else:
    m = margin
  result = Theme(facetMargin: some(m))

proc annotate*(text: string,
               left = NaN,
               bottom = NaN,
               x = NaN,
               y = NaN,
               top = NaN,
               right = NaN,
               font = font(12.0),
               rotate = 0.0,
               backgroundColor = white,
               alignKind = taLeft): Annotation =
  ## creates an annotation of `text` with a background
  ## `backgroundColor` (by default white) using the given
  ## `font`. Line breaks are supported.
  ## It is placed either at:
  ## - `(left, bottom)`, where these correspond to relative coordinates
  ##   mapping out the plot area as (0.0, 1.0). NOTE: smaller and larger
  ##   values than 0.0 and 1.0 are supported and will put the annotation outside
  ##   the plot area. Alternatively (!), either can be replaced by `right` or `top`.
  ## - `(x, y)` where `x` and `y` are values in the scale of the data
  ##   being plotted. This is useful if the annotation is to be placed relative
  ##   to specific data points. NOTE: for a discrete axis data scale is not
  ##   well defined, thus we fall back to relative scaling on that axis!
  ## In principle you can mix and match left/x and bottom/y! If both are given
  ## the former will be prioritized.
  ##
  ## NOTE: using `rotate` together with a background is currently broken.
  result = Annotation(left: left.orNone,
                      bottom: bottom.orNone,
                      x: x.orNone,
                      y: y.orNone,
                      top: top.orNone,
                      right: right.orNone,
                      text: text,
                      font: font,
                      rotate: some(rotate),
                      alignKind: alignKind,
                      backgroundColor: backgroundColor)
  if result.x.isNone and result.left.isNone and result.right.isNone or
     result.y.isNone and result.bottom.isNone and result.top.isNone:
    raise newException(ValueError, "Both an x/left/right and y/bottom/top position has to " &
      "given to `annotate`!")

proc `+`*(p: GgPlot, geom: Geom): GgPlot =
  ## adds the given geometry to the GgPlot object
  result = p
  result.geoms.add geom

proc `+`*(p: GgPlot, facet: Facet): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.facet = some(facet)

proc `+`*(p: GgPlot, ridges: Ridges): GgPlot =
  ## adds the given ridges to the GgPlot object
  result = p
  result.ridges = some(ridges)

proc `+`*(p: GgPlot, annot: Annotation): GgPlot =
  ## adds the given Annotation to the GgPlot object
  result = p
  result.annotations.add annot

proc applyTheme(pltTheme: var Theme, theme: Theme) =
  ## applies all elements of `theme`, which are `Some` to
  ## the same fields of `pltTheme`
  template ifSome(it: untyped): untyped =
    if theme.it.isSome:
      pltTheme.it = theme.it
  for field, v1, v2 in fieldPairs(pltTheme, theme):
    when typeof(v1) is Option: ## Copy all option fields
      if v2.isSome:
        v1 = v2

proc `+`*(p: GgPlot, theme: Theme): GgPlot =
  ## adds the given theme (or theme element) to the GgPlot object
  result = p
  applyTheme(result.theme, theme)
  # TODO: Maybe move these two completely out of `GgPlot` object
  if result.theme.title.isSome:
    result.title = result.theme.title.get
  if result.theme.subTitle.isSome:
    result.subTitle = result.theme.subTitle.get

proc `+`*(t1: Theme, t2: Theme): Theme =
  ## Helper to just combine two themes in the same way as adding them to a `GgPlot`
  ##
  ## For all fields defined in `t1` and `t2`, `t2` overwrites the fields of `t1`. This is
  ## to keep the sane order of 'last wins'.
  result = t1
  applyTheme(result, t2)

proc applyScale(aes: Aesthetics, scale: Scale): Aesthetics =
  ## applies the given `scale` to the `aes` by returning a modified
  ## `aes`
  func clone(s: Scale): Scale =
    ## Clones the given scale
    when defined(gcDestructors):
      result = new Scale
      result[] = scale[]
    else:
      result = deepCopy(scale)
  template assignCopyScale(field: untyped): untyped =
    if aes.field.isSome:
      var mscale: Scale
      mscale = clone(scale)
      mscale.col = aes.field.get.col
      mscale.ids = aes.field.get.ids
      result.field = some(mscale)
  result = aes
  case scale.scKind
  of scLinearData, scTransformedData:
    # potentially `scale` has no `column` asigned yet, read from
    # `axKind` from the given `aes`. If `aes` has no `x`/`y` scale,
    # `mscale` will remain unchanged
    case scale.axKind
    of akX:
      assignCopyScale(x)
      assignCopyScale(xMin)
      assignCopyScale(xMax)
    of akY:
      assignCopyScale(y)
      assignCopyScale(yMin)
      assignCopyScale(yMax)
  of scColor:
    assignCopyScale(color)
  of scFillColor:
    assignCopyScale(fill)
  of scAlpha:
    assignCopyScale(alpha)
  of scSize:
    assignCopyScale(size)
  of scShape:
    assignCopyScale(shape)
  of scText:
    assignCopyScale(text)

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

proc `+`*(p: GgPlot, dateScale: DateScale): GgPlot =
  ## Add the given `DateScale` to the plot, which means filling the optional
  ## `dateScale` field
  template clone(newScale, oldScale: untyped): untyped =
    when defined(gcDestructors):
      newScale =  new Scale
      newScale[] = oldScale[]
    else:
      `newScale` = deepCopy(oldScale)

  template assignCopyScale(arg, field, ds: untyped): untyped =
    if arg.field.isSome:
      var mscale: Scale
      clone(mscale, arg.field.get)
      mscale.dateScale = some(ds)
      arg.field = some(mscale)

  result = p
  case dateScale.axKind
  of akX: assignCopyScale(result.aes, x, dateScale)
  of akY: assignCopyScale(result.aes, y, dateScale)
  for g in mitems(result.geoms):
    case dateScale.axKind
    of akX: assignCopyScale(g.aes, x, dateScale)
    of akY: assignCopyScale(g.aes, y, dateScale)

template anyScale(arg: untyped): untyped =
  if arg.main.isSome or arg.more.len > 0:
    true
  else:
    false

proc requiresLegend(filledScales: FilledScales,
                    theme: Theme): bool =
  ## returns true if the plot requires a legend to be drawn
  if theme.hideLegend.isNone and
     (anyScale(filledScales.color) or
      anyScale(filledScales.fill) or
      anyScale(filledScales.size) or
      anyScale(filledScales.shape)):
    result = true
  else:
    result = false

proc initThemeMarginLayout(theme: Theme,
                           tightLayout: bool,
                           requiresLegend: bool): ThemeMarginLayout =
  let sc = theme.baseScale.get(1.0)
  result = ThemeMarginLayout(
    left: if theme.plotMarginLeft.isSome: theme.plotMarginLeft.get
          elif tightLayout: quant(0.2 * sc, ukCentimeter)
          else: quant(2.5 * sc, ukCentimeter),
    right: if theme.plotMarginRight.isSome: theme.plotMarginRight.get
           elif tightLayout: quant(0.2 * sc, ukCentimeter)
           elif requiresLegend: quant(5.0 * sc, ukCentimeter)
           else: quant(1.0 * sc, ukCentimeter),
    top: if theme.plotMarginTop.isSome: theme.plotMarginTop.get
         # this is not really a good solution. Legacy. Should depend on whether
         # there is a title instead!
         elif tightLayout: quant(0.2 * sc, ukCentimeter)
         elif requiresLegend: quant(1.0 * sc, ukCentimeter)
         else: quant(1.0 * sc, ukCentimeter),
    bottom: if theme.plotMarginBottom.isSome: theme.plotMarginBottom.get
            elif tightLayout: quant(0.2 * sc, ukCentimeter)
            else: quant(2.0 * sc, ukCentimeter),
    requiresLegend: requiresLegend
  )

proc plotLayout(view: var Viewport,
                layout: ThemeMarginLayout) =
  ## creates a layout for a plot in the current viewport that potentially
  ## leaves space for a legend. Important indices of the created viewports:
  ## If `tightLayout` is `true`, the left hand side will only have 0.2 cm
  ## of spacing.
  ## - main plot: idx = 4
  ## - legend: idx = 5
  # TODO: Make relative to image size!
  view.layout(3, 3, colwidths = @[layout.left,
                                  quant(0.0, ukRelative),
                                  layout.right],
              rowheights = @[layout.top,
                             quant(0.0, ukRelative),
                             layout.bottom])
  view[0].name = "topLeft"
  view[1].name = "title"
  view[2].name = "topRight"
  view[3].name = "yLabel"
  view[4].name = "plot"
  view[5].name = if layout.requiresLegend: "legend" else: "noLegend"
  view[6].name = "bottomLeft"
  view[7].name = "xLabel"
  view[8].name = "bottomRight"

proc createLayout(view: var Viewport,
                  filledScales: FilledScales, theme: Theme) =
  let hideTicks = theme.hideTicks.get(false)
  let hideLabels = theme.hideLabels.get(false)
  let tightLayout = hideLabels and hideTicks
  let layout = initThemeMarginLayout(theme, tightLayout,
                                     filledScales.requiresLegend(theme))
  view.plotLayout(layout)

proc generateLegendMarkers(plt: Viewport,
                           scale: Scale,
                           theme: Theme,
                           geomKind: GeomKind): seq[GraphObject] =
  ## generate the required Legend Markers for the given `aes`
  ## TODO: add different objects to be shown depending on the scale and geom.
  ## E.g. in case of `fill` fill the whole rectangle with the color. In case
  ## of geom_line only draw a line etc.
  ## Thus also put the rectangle drawing here.
  case scale.dcKind
  of dcDiscrete:
    let accessIdx = theme.legendOrder
    let idx = if accessIdx.isNone: toSeq(0 ..< scale.valueMap.len) else: accessIdx.get
    doAssert idx.len == scale.valueMap.len,
      "Custom ordering of legend keys must assign each key only once! " &
      "Assigned keys: " & $accessIdx & " for num keys: " & $scale.valueMap.len
    case scale.scKind
    of scColor, scFillColor:
      for i in idx:
        let color = scale.getValue(scale.getLabelKey(i)).color
        case geomKind
        of gkLine, gkHistogram:
          var st = LineDefaultStyle
          st.color = color
          st.lineWidth = 2.0
          result.add initLine(plt,
                              c(0.0, 0.5), c(1.0, 0.5),
                              style = some(st),
                              name = $scale.getLabelKey(i)) # assign same marker as above
        of gkTile:
          var st = HistoDefaultStyle
          st.color = color
          st.fillColor = color
          result.add initRect(plt,
                              c(0.05, 0.05), quant(0.9, ukRelative), quant(0.9, ukRelative),
                              style = some(st),
                              name = $scale.getLabelKey(i)) # assign same marker as above
        else: # of gkPoint:
          result.add initPoint(plt,
                               c(0.5, 0.5),
                               marker = mkCircle,
                               color = color,
                               name = $scale.getLabelKey(i)) # assign same marker as above
    of scShape:
      for i in idx:
        case geomKind
        of gkLine:
          var st = LineDefaultStyle
          st.lineWidth = 3.0
          st.lineType = scale.getValue(scale.getLabelKey(i)).lineType
          result.add initLine(plt,
                              c(0.0, 0.5), c(1.0, 0.5),
                              style = some(st),
                              name = $scale.getLabelKey(i)) # assign same marker as above
        else: # of gkPoint:
          result.add initPoint(plt,
                               c(0.5, 0.5),
                               marker = scale.getValue(scale.getLabelKey(i)).marker,
                               name = $scale.getLabelKey(i))
    of scSize:
      for i in idx:
        let size = scale.getValue(scale.getLabelKey(i)).size
        case geomKind
        of gkLine:
          var st = LineDefaultStyle
          st.lineWidth = size
          result.add initLine(plt,
                              c(0.0, 0.5), c(1.0, 0.5),
                              style = some(st),
                              name = $scale.getLabelKey(i)) # assign same marker as above
        # XXX:  collect all sizes in a loop first & then adjust all sizes in relative.
        # starting from center and take % away from left / bottom / width / height
        #of gkTile:
        #  var st = HistoDefaultStyle
        #  result.add initRecangle(plt,
        #                          c(0.0, 0.0), quant(1.0, ukRelative), quant(1.0, ukRelative),
        #                          style = some(st))
        else: # of gkPoint:
          result.add initPoint(plt,
                               c(0.5, 0.5),
                               marker = mkCircle,
                               size = size,
                               name = $scale.getLabelKey(i))

    else:
      raise newException(Exception, "`createLegend` unsupported for " & $scale.scKind)
  of dcContinuous:
    case scale.scKind
    of scColor, scFillColor:
      # replace yScale by scale of `scale`
      let tStyle = tickStyle(theme)
      var mplt = plt
      mplt.yScale = scale.dataScale
      # use 5 ticks by default
      # define as "secondary" because then ticks will be on the RHS
      if scale.transC == nil: # linear ticks
        #doAssert scale.numTicks > 0, "Default tick value for color scale is 0!"
        ## XXX: hand a number of ticks from the scale
        let ticks = mplt.initTicks(akY, 5, boundScale = some(scale.dataScale),
                                   isSecondary = true,
                                   style = tStyle)
        let tickLabs = mplt.tickLabels(ticks, isSecondary = true,
                                       #margin = some(plt.c1(0.3, akX, ukCentimeter)),
                                       format = scale.formatContinuousLabel,
                                       font = theme.legendFont)
        result = concat(tickLabs, ticks)
      else:
        doAssert scale.invTransC != nil, "Inverse transformation must exist if forward exists."
        let trans = scale.transC
        let invTrans = scale.invTransC
        # `scale.dataScale` is not transformed. Hand to `*Pow` as is
        let minVal = invTrans.smallestPow(scale.dataScale.low)
        let maxVal = invTrans.largestPow(scale.dataScale.high)
        # compute transformed scale to assign to viewport
        let sTrans = (low: trans(scale.dataScale.low), high: trans(scale.dataScale.high))
        ## XXX: handle format and tick labels !
        let (labs, labelPos) = tickPosTransformed(sTrans,
                                                  trans, invTrans,
                                                  breaks = scale.breaks,
                                                  format = nil)
                                                  #hideTickLabels = hideTickLabels,
                                                  #format = format)

        let tickLocs = labelPos.toCoord1D(akY, sTrans)
        mplt.yScale = (low: scale.transC(minVal), high: scale.transC(maxVal))
        let (tickObjs, labObjs) = mplt.tickLabels(tickLocs, labs, akY, isSecondary = true,
                                                  #rotate = rotate,
                                                  style = tStyle,
                                                  alignToOverride = some(taLeft),
                                                  font = theme.legendFont)
                                                  #margin = margin)
        if true: # not hideTickLabels:
          mplt.addObj concat(tickObjs, labObjs)
        result = tickObjs


    else:
      raise newException(Exception, "Continuous legend unsupported for scale kind " &
        $scale.scKind)

proc handleGridLines(view: Viewport,
                     xticks, yticks: seq[GraphObject],
                     theme: Theme): seq[GraphObject] =
  ## Handles the addition of grid lines. Applies the `theme` given the `xticks` / `yticks`
  ## and adds potential minor grid lines
  # get the grid lines style from the given theme
  let gridLineStyle = theme.getGridLineStyle()
  if theme.gridLines.isNone or theme.gridLines.get:
    result = @[view.initGridLines(some(xticks), some(yticks),
                                  style = some(gridLineStyle))]
  elif theme.onlyAxes.isSome and theme.onlyAxes.get:
    # only draw axes with grid line style
    result = @[view.xaxis(gridLineStyle.lineWidth, gridLineStyle.color),
               view.yaxis(gridLineStyle.lineWidth, gridLineStyle.color)]
  if theme.minorGridLines.isSome and theme.minorGridLines.get:
    # want minor grid lines
    # minor lines are half width
    let minorGridLineStyle = gridLineStyle.getMinorGridLineStyle(theme)
    result.add view.initGridLines(some(xticks), some(yticks),
                                  major = false,
                                  style = some(minorGridLineStyle))

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

proc handleLabels(view: Viewport, theme: Theme) =
  ## potentially moves the label positions and enlarges the areas (not yet)
  ## potentially moves the label positions and enlarges the areas (not yet)
  ## for the y label / tick label column or x row.
  # TODO: clean this up!
  let hideX = theme.hideXLabels.get(false)
  let hideY = theme.hideYLabels.get(false)
  var
    xLabObj: GraphObject
    yLabObj: GraphObject
    xMargin: Coord1D
    yMargin: Coord1D
  let
    xLabTxt = theme.xLabel.unwrap()
    yLabTxt = theme.yLabel.unwrap()
  template getMargin(marginVar, themeField, nameVal, axKind: untyped): untyped =
    ## looks at all *tick labels* to determine the longest (string length) label to
    ## place x / y label such that it avoids overlap
    if not themeField.isSome:
      let labs = view.objects.filterIt(it.name == nameVal)
      let labNames = labs.mapIt(it.txtText)
      let labLens = labNames.argMaxIt(len(it))
      # TODO: use custom label font for margin calc?
      let font = theme.labelFont.get(font(8.0))
      let tlOffset = theme.baseLabelMargin.get(0.3) ## Default: 0.3 cm
      # if `theme_scale` used, already pre multiplied by `baseScale`
      case axKind
      of akX:
        marginVar = Coord1D(pos: 1.5, kind: ukStrHeight,
                            backend: view.backend,
                            text: labNames[labLens], font: font) +
                    Coord1D(pos: tlOffset, kind: ukCentimeter)
      of akY:
        marginVar = Coord1D(pos: 1.0, kind: ukStrWidth,
                            backend: view.backend,
                            text: labNames[labLens], font: font) +
                    Coord1D(pos: tlOffset, kind: ukCentimeter)
    else:
      marginVar = Coord1D(pos: themeField.get, kind: ukCentimeter)

  template createLabel(label, labproc, labTxt, themeField, marginVal: untyped,
                       isSecond = false, rot = none[float]()): untyped =
    let fnt = if theme.labelFont.isSome: theme.labelFont.get else: font()
    if themeField.isSome:
      label = labproc(view,
                      labTxt,
                      margin = get(themeField),
                      isCustomMargin = true,
                      isSecondary = isSecond,
                      font = fnt)
    else:
      label = labproc(view,
                      labTxt,
                      margin = marginVal,
                      isSecondary = isSecond,
                      font = fnt)

  if not hideX:
    getMargin(xMargin, theme.xLabelMargin, "xtickLabel", akX)
    createLabel(xLabObj, xlabel, xLabTxt, theme.xLabelMargin, xMargin)
    view.addObj xLabObj
  if not hideY:
    getMargin(yMargin, theme.yLabelMargin, "ytickLabel", akY)
    createLabel(yLabObj, ylabel, yLabTxt, theme.yLabelMargin, yMargin)
    view.addObj yLabObj

  if theme.hasSecondary(akX):
    let secAxisLabel = theme.xLabelSecondary.unwrap()
    var labSec: GraphObject
    getMargin(xMargin, theme.xLabelMargin, "xtickLabelSecondary", akX)
    createLabel(labSec, xlabel, secAxisLabel, theme.yLabelMargin, xMargin,
                true)
    view.addObj @[labSec]
  if theme.hasSecondary(akY):
    let secAxisLabel = theme.yLabelSecondary.unwrap()
    getMargin(yMargin, theme.yLabelMargin, "ytickLabelSecondary", akY)
    var labSec: GraphObject
    createLabel(labSec, ylabel, secAxisLabel, theme.yLabelMargin, yMargin,
                true)
    view.addObj @[labSec]

proc calcRidgeViewMap(ridge: Ridges,
                      labelSeq: var seq[Value]): Table[Value, int] =
  ## calculates the table mapping label `Values` to viewport
  ## indices of the ridges. Also modifies the sequence of
  ## labels to be sorted by the viewport indices.
  ## Takes into account the possible user map provided as
  ## `ridge.labelOrder`.
  ## TODO: better handle invalid `labelOrder` tables! Duplicates
  ## holes etc.
  let numLabels = labelSeq.len
  if ridge.labelOrder.len == 0:
    for i, label in labelSeq:
      result[label] = i + 1
  else:
    # add 1 to the label order given by the user
    # override labelSeq
    labelSeq.setLen(0)
    let pairIdx = block:
      var res: seq[(Value, int)]
      for label, idx in ridge.labelOrder:
        res.add (label, idx)
      res.sortedByIt(it[1])
    for (label, idx) in pairIdx:
      if idx >= numLabels:
        raise newException(ValueError, "Given `labelOrder` indices must not exceed the " &
          "number of labels! Max index: " & $idx & ", number of labels: " & $numLabels)
      result[label] = idx + 1
      labelSeq.add label

proc createRidgeLayout(view: Viewport, theme: Theme, numLabels: int) =
  ## creates the layout for ridgeline plots
  # The layout is essentially the user defined margin for
  # discrete scales + equal spacing for each ridge
  let discrMarginOpt = theme.discreteScaleMargin
  var discrMargin = quant(0.0, ukRelative)
  if discrMarginOpt.isSome:
    discrMargin = discrMarginOpt.unsafeGet
  var indHeights = newSeqWith(numLabels, quant(0.0, ukRelative))
  view.layout(cols = 1, rows = numLabels + 2,
                     rowHeights = concat(@[discrMargin],
                                         indHeights,
                                         @[discrMargin]),
                     ignoreOverflow = true)

proc generateRidge*(view: Viewport, ridge: Ridges, p: GgPlot, filledScales: FilledScales,
                    theme: Theme,
                    hideLabels = false,
                    hideTicks = false) =
  let yRidgeScale = getYRidgesScale(filledScales)
  var yLabelSeq = yRidgeScale.labelSeq
  let numLabels = yLabelSeq.len
  var yScale = if theme.yRange.isSome: theme.yRange.unsafeGet else: filledScales.yScale
  # set `yScale` to overlap

  yScale = (low: yScale.low, high: yScale.high / ridge.overlap)
  view.yScale = yScale
  let viewMap = calcRidgeViewMap(ridge, yLabelSeq)
  view.createRidgeLayout(theme, numLabels)
  for label, idx in pairs(viewMap):
    var viewLabel = view[idx]
    for fg in filledScales.geoms:
      var pChild = viewLabel.addViewport(name = "data")
      # create a theme, which ignores points outside the scale (which happens
      # due to overlap!)
      var mtheme = theme
      mtheme.xOutsideRange = some(orkNone)
      mtheme.yOutsideRange = some(orkNone)
      pChild.createGobjFromGeom(fg, mtheme, labelVal = some(toObject(($ridge.col, label))))
      # add the data viewport to the view
      viewLabel.children.add pChild
    if ridge.showTicks:
      ## TODO: fix the hack using `1e-5`!. Needed for the side effect of adding to viewport!
      discard viewLabel.handleTicks(filledScales, akY, theme = theme,
                                    numTicksOpt = some(5),
                                    boundScaleOpt = some(
                                              (low: yScale.low + 1e-5,
                                               high: yScale.high - 1e-5)),
                                    label = label)

  if not hideTicks:
    let hideX = theme.hideXTickLabels.get(false)
    let hideY = theme.hideYTickLabels.get(false)
    var xticks = view.handleTicks(filledScales, akX, theme = theme, hideTickLabels = hideX)
    let format =
      if yRidgeScale.formatDiscreteLabel != nil: yRidgeScale.formatDiscreteLabel
      else: (proc(x: Value): string = $x)
    # we create the ticks manually with `discreteTickLabels` to set the labels
    let tStyle = tickStyle(theme) # wImg/hImg is in points
    let marginOpt = some(view.getTickLabelMargin(theme, akY))
    var yticks = view.handleDiscreteTicks(akY, yLabelSeq,
                                          theme = theme, centerTicks = false,
                                          format = format,
                                          hideTickLabels = hideY,
                                          margin = marginOpt,
                                          tStyle = tStyle)
    let grdLines = view.handleGridLines(xticks, yticks, theme)
    view.addObj grdLines
  if not hideLabels:
    view.handleLabels(theme)
  view.xScale = theme.xMarginRange

  if not filledScales.discreteX and filledScales.reversedX:
    view.xScale = (low: view.xScale.high, high: view.xScale.low)
  if not filledScales.discreteY and filledScales.reversedY:
    view.yScale = (low: view.yScale.high, high: view.yScale.low)
  #view.updateDataScale()

proc generatePlot(view: Viewport, p: GgPlot, filledScales: FilledScales,
                  theme: Theme,
                  hideLabels = false,
                  hideTicks = false,
                  dataAsBitmap = false) =
  # first write all plots into dummy viewport
  view.background(style = some(getPlotBackground(theme)))

  # change scales to user defined if desired
  view.xScale = if theme.xRange.isSome: theme.xRange.unsafeGet else: filledScales.xScale

  if p.ridges.isSome:
    let ridge = p.ridges.unsafeGet
    view.generateRidge(ridge, p, filledScales, theme, hideLabels, hideTicks)
  else:
    view.yScale = if theme.yRange.isSome: theme.yRange.unsafeGet else: filledScales.yScale
    var fullPlot = view.addViewport(name = "full_GGPLOT", dataAsBitmap = dataAsBitmap)
    for fg in filledScales.geoms:
      # for each geom, we create a child viewport of `view` covering
      # the whole viewport, which will house the data we just created.
      # Due to being a child, if will be drawn *after* its parent. This way things like
      # ticks will be below the data.
      # On the other hand this allows us to draw several geoms in on a plot and have the
      # order of the function calls `geom_*` be preserved
      var pChild = fullPlot.addViewport(name = "data")
      # DF here not needed anymore!
      pChild.createGobjFromGeom(fg, theme)
      # add the data viewport to the view
      fullPlot.children.add pChild
    # add the child that contains all plot data
    view.children.add fullPlot

    var
      xticks: seq[GraphObject]
      yticks: seq[GraphObject]
    if not hideTicks:
      xticks = view.handleTicks(filledScales, akX,
                                numTicksOpt = some(filledScales.getXTicks()),
                                theme = theme)
      yticks = view.handleTicks(filledScales, akY,
                                numTicksOpt = some(filledScales.getYTicks()),
                                theme = theme)

    # after creating all GraphObjects and determining tick positions based on
    # (possibly) user defined plot range, set the final range of the plot to
    # the range taking into account the given margin
    view.xScale = theme.xMarginRange
    view.yScale = theme.yMarginRange

    if not filledScales.discreteX and filledScales.reversedX:
      view.xScale = (low: view.xScale.high, high: view.xScale.low)
    if not filledScales.discreteY and filledScales.reversedY:
      view.yScale = (low: view.yScale.high, high: view.yScale.low)

    # TODO: Make sure we still have to do this. I think not!
    view.updateDataScale()
    if not hideTicks:
      view.updateDataScale(xticks)
      view.updateDataScale(yticks)

    # get the user desired grid lines
    let grdLines = view.handleGridLines(xticks, yticks, theme)

    # given the just created plot and tick labels, have to check
    # whether we should enlarge the column / row for the y / x label and
    # move the label
    if not hideLabels:
      # TODO: why do we add labels to child 4 and not directly into the viewport we
      # use to provide space for it, i.e. 3?
      view.handleLabels(theme)
    view.addObj grdLines

proc generateFacetPlots(view: Viewport, p: GgPlot,
                        filledScales: FilledScales,
                        theme: Theme,
                        hideLabels = false,
                        hideTicks = false,
                        dataAsBitmap = false) =
  var p = p
  doAssert p.facet.isSome
  let facet = filledScales.facets
  # combine scales / plot theme for final theme
  let numExist = facet.combinations.card
  # set margin of plot to avoid tick labels getting too close
  var theme = theme
  if theme.xMargin.isNone:
    theme.xMargin = some(0.05)
  if theme.yMargin.isNone:
    theme.yMargin = some(0.05)
  # create a theme, which ignores points outside the scale (which happens
  # due to overlap!)
  theme.xTickLabelMargin = if theme.xTickLabelMargin.isSome: theme.xTickLabelMargin
                           else: some(1.75)
  theme.yTickLabelMargin = if theme.yTickLabelMargin.isSome: theme.yTickLabelMargin
                           else: some(-1.25)
  theme.xTicksRotate = p.theme.xTicksRotate
  theme.yTicksRotate = p.theme.yTicksRotate
  theme.xTicksTextAlign = p.theme.xTicksTextAlign
  theme.yTicksTextAlign = p.theme.yTicksTextAlign

  # calculate number of rows and columns based on numGroups
  var
    rows: int
    cols: int
  template getOrFalse(t): untyped = t.isSome and t.get
  if getOrFalse(theme.preferRowsOverColumns):
    # calcRowsCols prefers columns over rows. For this we prefer rows over cols. That's
    (cols, rows) = calcRowsColumns(0, 0, numExist)
  else:
    # default
    (rows, cols) = calcRowsColumns(0, 0, numExist)
  if facet.sfKind in {sfFreeX, sfFreeY, sfFree}:
    let margin = if theme.facetMargin.isSome: theme.facetMargin.get
                 else: quant(0.015, ukRelative)
    view.layout(cols, rows, margin = margin)
  else:
    let margin = if theme.facetMargin.isSome: theme.facetMargin.get
                 else: quant(0.001, ukRelative)
    view.layout(cols, rows, margin = margin)

  var
    xticks: seq[GraphObject]
    yticks: seq[GraphObject]
  let lastCol = numExist mod cols

  for (label, sFacet) in pairs(facet.facets):
    let idx = sFacet.idx
    var viewLabel = view[idx]
    ## perform steps only required `once` for each label
    # create the layout for a facet + header
    ## XXX: make ratio of plot to header adjustable
    # viewLabel.layout(1, 2, rowHeights = @[quant(0.075, ukRelative), quant(0.925, ukRelative)],
    viewLabel.layout(1, 2, rowHeights = @[quant(0.1, ukRelative), quant(0.9, ukRelative)],
                     margin = quant(0.01, ukRelative))
    var headerView = viewLabel[0]
    # set the background of the header
    headerView.background()
    # put in the text
    let text = $label #pair.mapIt($it[0] & ": " & $it[1]).join(", ")
    let facetFont = theme.facetHeaderFont.get(font(8.0, alignKind = taCenter))
    let facetPos = theme.facetHeaderPos.get(c(0.5, 0.5))
    let headerText = headerView.initText(facetPos,
                                         text,
                                         textKind = goText,
                                         alignKind = facetFont.alignKind,
                                         font = some(facetFont),
                                         name = "facetHeaderText")
    headerView.addObj headerText
    headerView.name = "facetHeader"
    # fill the plot
    var plotView = viewLabel[1]
    plotView.background(style = some(getPlotBackground(theme)))

    let curRow = idx div cols
    let curCol = idx mod cols
    # hide the labels if scales not free and plot not in bottom row or
    # left most column
    let hideXLabels = if facet.sfKind in {sfFreeX, sfFree} or
                         curRow == rows - 1 or
                         (curRow == rows - 2 and
                          curCol >= lastCol and
                          lastCol > 0):
                        false
                      else: true
    let hideYLabels = if facet.sfKind in {sfFreeX, sfFree} or
                         curCol == 0: false
                      else: true
    # assign names
    plotView.name = "facetPlot"
    viewLabel.name = "facet_" & text
    # generate the actual plot viewport
    var fullPlot = view.addViewport(name = "full_GGPLOT", dataAsBitmap = dataAsBitmap)

    var setGridAndTicks = false
    for fg in filledScales.geoms:
      if not setGridAndTicks:
        ## TODO: this means we currently use the margin range of the ``first``
        ## geom in a single facet. They should all agree I guess?
        # compute the data scale with a small margin used for facets
        # This changes `x/yMarginRange` of the theme
        let xScale = if facet.sfKind in {sfFreeX, sfFree}: sFacet.xScale else: filledScales.xScale
        let yScale = if facet.sfKind in {sfFreeY, sfFree}: sFacet.yScale else: filledScales.yScale

        theme.xMarginRange = calculateMarginRange(theme, xScale, akX)
        theme.yMarginRange = calculateMarginRange(theme, yScale, akY)
        # assign theme ranges to this views scale
        plotView.xScale = theme.xMarginRange
        plotView.yScale = theme.yMarginRange

        # change number of ticks from default 10 if numbers too large (i.e. we
        # print between 100 and 9000); get's too crowded along x axis
        let xTickNum = if theme.xMarginRange.high > 1000.0 and
                        theme.xMarginRange.high < 1e5:
                       5
                     else:
                       filledScales.getXTicks()

        xticks = plotView.handleTicks(filledScales, akX, theme = theme,
                                      numTicksOpt = some(xTickNum),
                                      hideTickLabels = hideXLabels,
                                      label = label)
        yticks = plotView.handleTicks(filledScales, akY, theme = theme,
                                      hideTickLabels = hideYLabels,
                                      label = label)

        let grdLines = plotView.handleGridLines(xticks, yticks, theme)
        plotView.addObj grdLines
        setGridAndTicks = true

      # create a child viewport to which we add the data to be able to stack multiple geom layers
      var pChild = plotView.addViewport(name = "data")
      pChild.createGobjFromGeom(fg, theme, labelVal = some(label))
      # add the data viewport to the view
      fullPlot.children.add pChild

    plotView.children.add fullPlot

    # possibly update x/y scale of parent
    viewLabel.xScale = plotView.xScale
    viewLabel.yScale = plotView.yScale

    if not filledScales.discreteX and filledScales.reversedX:
      viewLabel.xScale = (low: view.xScale.high, high: view.xScale.low)
    if not filledScales.discreteY and filledScales.reversedY:
      viewLabel.yScale = (low: view.yScale.high, high: view.yScale.low)

  if not hideLabels:
    # set the theme margins to defaults since `view` does not have any tick label texts
    # which can be used to determine the margin
    theme.xLabelMargin = if theme.xLabelMargin.isSome: theme.xLabelMargin
                         else: some(1.0)
    theme.yLabelMargin = if theme.yLabelMargin.isSome: theme.yLabelMargin
                         else: some(1.5)
    view.handleLabels(theme)

proc customPosition(t: Theme): bool =
  ## returns true if `legendPosition` is set and thus legend sits at custom pos
  result = t.legendPosition.isSome

proc getLeftBottom(view: Viewport, annot: Annotation,
                   height, width: Quantity
                  ): tuple[left: float, bottom: float] =
  ## Given an annotation this proc returns the relative `(left, bottom)`
  ## coordinates of either the `(x, y)` values in data space converted
  ## using the `x, y: ginger.Scale` of the viewport or directly using
  ## the annotations `(left, bottom)` pair if available
  if annot.left.isSome:
    result.left = toPoints(quant(annot.left.unsafeGet, ukRelative),
                           length = some(pointWidth(view))
    ).val
  elif annot.right.isSome:
    let right = toPoints(quant(annot.left.unsafeGet, ukRelative),
                         length = some(pointWidth(view)))
    result.left = sub(right, width).val
  else:
    # NOTE: we make sure in during `annotate` that either `left` or
    # `x` is defined!
    result.left = toPoints(Coord1D(pos: annot.x.unsafeGet,
                                   kind: ukData,
                                   axis: akX,
                                   scale: view.xScale),
                           length = some(pointWidth(view))
    ).pos
  if annot.bottom.isSome:
    result.bottom = toPoints(quant(annot.bottom.unsafeGet, ukRelative),
                             length = some(pointHeight(view))
    ).val
  elif annot.top.isSome:
    let top = toPoints(quant(annot.top.unsafeGet, ukRelative),
                             length = some(pointHeight(view)))
    result.bottom = add(top, height).val
  else:
    # NOTE: we make sure in during `annotate` that either `bottom` or
    # `y` is defined!
    result.bottom = toPoints(Coord1D(pos: annot.y.unsafeGet,
                                     kind: ukData,
                                     axis: akY,
                                     scale: view.yScale),
                             length = some(pointHeight(view))
    ).pos


proc drawAnnotations*(view: var Viewport, p: GgPlot) =
  ## draws all annotations from `p` onto the mutable view `view`.
  # this is 0.5 times the string height of `M` character. Margin between text and
  # the background rectangle
  const AnnotRectMargin = 0.5
  let backend = view.backend
  let fType = view.fType

  for annot in p.annotations:
    # style to use for this annotation
    let rectStyle = Style(fillColor: annot.backgroundColor,
                          color: annot.backgroundColor)
    # Use font specified by theme, if any
    let annotFont = p.theme.annotationFont.get(annot.font)

    # Get a well defined margin based on `M` character at current font
    let marginH = strHeight(view, AnnotRectMargin, annotFont)
      .toPoints()
    # use same amount of space for width (x) margin
    let marginW = strHeight(view, AnnotRectMargin, annotFont)
      .toPoints()
    # Total height of the text + 2 margin
    let totalHeight = quant(
      getStrHeight(view, annot.text, annotFont).val + marginH.pos * 2.0,
      unit = ukPoint)

    # find longest line of annotation to base background on
    let maxLine = annot.text.splitLines.sortedByIt(
      getStrWidth(backend, fType, it, annotFont).val
    )[^1]
    # and get its actual width
    let maxWidth = getStrWidth(view, maxLine, annotFont)

    # get the left and bottom position
    let (left, bottom) = view.getLeftBottom(annot, totalHeight, maxWidth)

    # calculate required width for background rectangle. string width + 2 * margin
    let rectWidth = quant(
      maxWidth.val + marginW.pos * 2.0,
      unit = ukPoint
    )
    # left and bottom positions, shifted each by one margin
    let rectX = left - marginW.pos
    let rectY = bottom - totalHeight.val + marginH.pos
    # create background rectangle
    var annotRect: GraphObject
    if annot.backgroundColor != transparent:
      annotRect = view.initRect(
        Coord(x: Coord1D(pos: rectX, kind: ukPoint),
              y: Coord1D(pos: rectY, kind: ukPoint)),
        rectWidth,
        totalHeight,
        style = some(rectStyle),
        rotate = annot.rotate,
        name = "annotationBackground")
    # create actual annotation
    let annotText = view.initMultiLineText(
      origin = c(left, bottom, ukPoint),
      text = annot.text,
      textKind = goText,
      alignKind = annot.alignKind,
      rotate = annot.rotate,
      fontOpt = some(annotFont),
      useRealText = false) # use `My` to determine height of single line to get consistent line spacing
    if not annotRect.isNil:
      view.addObj concat(@[annotRect], annotText)
    else:
      view.addObj annotText

proc drawTitle(view: Viewport, title: string, theme: Theme, width: Quantity) =
  ## Draws a title onto the `view` (which should be the header of the plot).
  ## If the length of the title exceeds the `width` (should be the width of
  ## the header viewport + right side margin), we automatically wrap the
  ## title to multiple lines.
  var title = title
  let font = theme.titleFont.get(font(16.0))
  if "\n" notin title and view.backend != bkTikZ: # for tikZ let LaTeX handle line wrapping
    # user does not do manual wrapping. Check if needs to be wrapped.
    let strWidth = getStrWidth(view, title, font)
    if strWidth > width:
      # rebuild and wrap
      var line: string
      var mTitle: string
      for word in title.split(' '):
        let lineWidth = getStrWidth(view, line & word, font)
        if lineWidth < width:
          line.add word & " " # we add a space even at the end of line...
        else:
          mTitle.add line & "\n"
          line = word & " "
      mTitle.add line
      title = mTitle
  else:
    # user is manually wrapping and responsible
    discard
  # NOTE: on the TikZ backend `0.7` works better. Make adjustable?
  let titlePos = theme.titlePosition.get(c(0.0, 0.9))
  let titleObj = view.initMultiLineText(titlePos,
                                        title,
                                        textKind = goText,
                                        alignKind = taLeft,
                                        fontOpt = some(font),
                                        useRealText = false) # use `My` to determine height of single line to get consistent line spacing
  view.addObj titleObj

proc determinePlotHeight(theme: Theme, filledScales: FilledScales, width, height: float, hideLabels, hideTicks: bool): float =
  ## Determines the correct plot height. This is:
  ##
  ## - calculated based on margins and plot scales if `fixedRatio` is set in `Theme`
  ## - or value given as `height` field to theme
  ## - or argument given to `ggcreate`
  let tightLayout = hideLabels and hideTicks
  let layout = initThemeMarginLayout(theme, tightLayout,
                                     filledScales.requiresLegend(theme))
  # if `OneToOne` adjust height to match one to one data
  if theme.fixedRatio.isSome:
    let xS = if theme.xRange.isSome: theme.xRange.unsafeGet else: filledScales.xScale
    let xD = xS.high - xS.low
    let yS = if theme.yRange.isSome: theme.yRange.unsafeGet else: filledScales.yScale
    let yD = ys.high - ys.low
    let ratio = yD / xD
    let spacingLR = add(layout.left, layout.right)
    doAssert spacingLR.unit == ukCentimeter
    let spacingTB = add(layout.top, layout.bottom)
    doAssert spacingTB.unit == ukCentimeter
    proc toPt(x: float): float = x / 2.54 * DPI
    result = theme.fixedRatio.get * (spacingTB.val.toPt() + ratio * (width - spacingLR.val.toPt()))
  else:
    result = theme.height.get(height.float) # use theme height or else `ggcreate` `height` argument

proc ggcreate*[T: SomeNumber](p: GgPlot, width: T = 640.0, height: T = 480.0, dataAsBitmap = false): PlotView =
  ## Applies all calculations to the `GgPlot` object required to draw
  ## the plot with the selected backend (either determined via filetype in `ggsave`,
  ## handed manually to `ggplot`) and returns a `PlotView`.
  ##
  ## The `PlotView` contains the final `Scales` built from the `GgPlot` object and all its geoms
  ## plus the final `ginger.Viewport` which only has to be drawn to produce the
  ## plot.
  ##
  ## This proc is useful to investigate the data structure that results before
  ## actually producing an output file or to combine multiple plots into a combined
  ## viewport.
  if p.geoms.len == 0:
    raise newException(ValueError, "Please use at least one `geom`!")

  var filledScales: FilledScales
  if p.ridges.isSome:
    # update all aesthetics to include the `yRidges` scale
    filledScales = collectScales(updateAesRidges(p))
  else:
    filledScales = collectScales(p)
  let theme = buildTheme(filledScales, p)
  let hideTicks = if theme.hideTicks.isSome: theme.hideTicks.unsafeGet
                   else: false
  let hideLabels = if theme.hideLabels.isSome: theme.hideLabels.unsafeGet
                   else: false

  let width = p.theme.width.get(width.float)
  let height = determinePlotHeight(p.theme, filledScales, width, height.float, hideTicks, hideLabels)
  # create the plot
  var img = initViewport(name = "root",
                         wImg = width,
                         hImg = height,
                         backend = p.backend,
                         fType = p.fType)

  # set color of canvas background
  img.background(style = some(getCanvasBackground(theme)), name = "canvasBackground")

  img.createLayout(filledScales, theme)
  # get viewport of plot
  var pltBase = img[4]

  if p.facet.isSome:
    pltBase.generateFacetPlots(p, filledScales, theme,
                               hideLabels = hideLabels,
                               hideTicks = hideTicks,
                               dataAsBitmap = dataAsBitmap)
  else:
    pltBase.generatePlot(p, filledScales, theme,
                         hideLabels = hideLabels,
                         hideTicks = hideTicks,
                         dataAsBitmap = dataAsBitmap)
  let xScale = pltBase.xScale
  let yScale = pltBase.yScale
  img.xScale = xScale
  img.yScale = yScale
  #img.updateDataScale()

  # possibly correct the yScale assigned to the root Viewport
  img.yScale = pltBase.yScale

  # draw legends
  # store each type of drawn legend. only one type for each kind
  var drawnLegends = initHashSet[(DiscreteKind, ScaleKind, GeomKind)]()
  ## TODO: consider if this is such a stable thing to do. Useful for now.
  var scaleNames = initHashSet[string]()
  var legends: seq[Viewport]
  for scale in enumerateScalesByIds(filledScales):
    let scaleCol = evaluate(scale.col).toStr
    ## XXX: need to look up the geom based on the geom ID. Then
    ## hand the geom kind to create legend
    let geomKind = block:
      var kind: GeomKind
      for g in p.geoms:
        if g.gid in scale.ids:
          kind = g.kind
          break
      kind

    if theme.hideLegend.isNone and
       scale.scKind notin {scLinearData, scTransformedData} and
       (scale.dcKind, scale.scKind, geomKind) notin drawnLegends:
      # create deep copy of the original legend pane
      var lg: Viewport
      when defined(gcDestructors):
        new(lg)
        lg[] = img[5][]
      else:
        lg = deepCopy(img[5])

      ## XXX: not only hand geom kind, but also the used setting, e.g. the marker style if constant
      ## or a constant color
      lg.createLegend(scale, theme, geomKind)
      if scaleCol notin scaleNames:
        legends.add lg
        drawnLegends.incl (scale.dcKind, scale.scKind, geomKind)
      scaleNames.incl scaleCol

  # now create final legend
  if legends.len > 0:
    img[5].finalizeLegend(legends)
    if customPosition(p.theme):
      let pos = p.theme.legendPosition.get
      img[5].origin.x = pos.x
      img[5].origin.y = pos.y

  # draw available annotations,
  img[4].drawAnnotations(p)

  if p.title.len > 0:
    img[1].drawTitle(p.title, theme, add(img[1].pointWidth, img[2].pointWidth))

  result.filledScales = filledScales
  result.view = img

proc ggmulti*(plts: openArray[GgPlot], fname: string, width = 640, height = 480,
              widths: seq[int] = @[],
              heights: seq[int] = @[],
              useTeX = false,
              onlyTikZ = false,
              standalone = false,
              texTemplate = "",
              caption = "",
              label = "",
              placement = "htbp",
              dataAsBitmap = false
             ) =
  ## Creates a simple multi plot in a grid. Currently no smart layouting.
  ## If `widths` and `heights` is given, expects a sequence of numbers of the length
  ## of given plots. It will use the width/height of the same index for the corresponding
  ## plot.
  ##
  ## For an explanaiton of the TeX arguments, see the `ggsave` docstring.
  let width = if widths.len == 1: widths[0] else: width
  let height = if heights.len == 1: heights[0] else: height
  template raiseIfNotMatching(arg: untyped): untyped =
    if arg.len > 1 and arg.len != plts.len:
      raise newException(ValueError, "Incorrect number of " & $astToStr(arg) & " in call to " &
        "`ggmulti`. Has " & $arg.len & ", but needs: " & $plts.len)
  raiseIfNotMatching(widths)
  raiseIfNotMatching(heights)

  # determine file type (neeeded fro backend in `ggcreate`) and create tex options
  let texOptions = toTeXOptions(useTeX, onlyTikZ, standalone, texTemplate,
                                caption, label, placement)
  let fType = parseFilename(fname)
  let backend = fType.toBackend(texOptions)

  # calcRowsCols prefers columns over rows. For this we prefer rows over cols. That's
  # why the args are inverted! (it returns (rows, cols))
  var img: Viewport
  if widths.len > 0 or heights.len > 0:
    # use it & number of elemnts as sizes for the columns
    let wVal = if widths.len == 0: width else: widths.sum
    let hVal = if heights.len == 0: height else: heights.sum
    img = initViewport(wImg = wVal.float, hImg = hVal.float, backend = backend, fType = fType)
    let widthsQ = widths.mapIt(quant(it.float, ukPoint))
    let heightsQ = heights.mapIt(quant(it.float, ukPoint))
    img.layout(cols = max(widths.len, 1), rows = max(heights.len, 1),
               colWidths = widthsQ, rowHeights = heightsQ)
  else:
    # compute number of required columns
    let (cols, rows) = calcRowsColumns(0, 0, plts.len)
    img = initViewport(wImg = (width * cols).float, hImg = (height * rows).float,
                       backend = backend, fType = fType)
    img.layout(cols = cols, rows = rows)

  for i, plt in plts:
    # assign backend (required for things like `strWidth`
    var pltB = plt
    pltB.backend = backend
    # get correct width / height.
    let wVal = if i < widths.len: widths[i] else: width
    let hVal = if i < heights.len: heights[i] else: height
    let pp =  ggcreate(pltB, width = wVal, height = hVal, dataAsBitmap = dataAsBitmap)
    # embed the finished plots into the the new viewport
    img.embedAt(i, pp.view)

  # combine both into a single viewport to draw as one image
  img.draw(fname, texOptions)

proc ggdraw*(view: Viewport, fname: string,
             texOptions: TeXOptions = TeXOptions()) =
  ## draws the given viewport and stores it in `fname`.
  ## It assumes that the `view` was created as the field of
  ## a `PlotView` object from a `GgPlot` object with `ggcreate`
  view.draw(fname, texOptions)

proc ggdraw*(plt: PlotView, fname: string,
             texOptions: TeXOptions = TeXOptions()) =
  ## draws the viewport of the given `PlotView` and stores it in `fname`.
  ## It assumes that the `plt`` was created from a `GgPlot` object with
  ## `ggcreate`
  plt.view.draw(fname, texOptions)

proc assignBackend(p: GgPlot, fname: string, texOptions: TeXOptions,
                   backend: BackendKind): GgPlot =
  ## assigns the correct backend based on filename `fname` and `texOptions`
  result = p
  case backend
  of bkNone: # no user given backend?
    if p.backend != bkNone: # User given to `ggplot()`!
      result = p.assignBackend(fname, texOptions, p.backend)
    else: # really no user given
      let fType = parseFilename(fname)
      result.backend = fType.toBackend(texOptions)
  else:
    result.backend = backend

when defined(WritePlotCsv):
  from std / os import getEnv, expandTilde
  import std / strformat
proc ggsave*(
  p: GgPlot, fname: string, width = 640.0, height = 480.0,
  texOptions: TeXOptions, backend: BackendKind = bkNone,
  dataAsBitmap = false) =
  ## This is the same as the `ggsave` proc below for the use case of calling it
  ## directly on a `GgPlot` object using a possible TeX options object.
  ##
  ## See the docstring there.
  let texOptions = p.theme.texOptions.get(texOptions)
  let p = p.assignBackend(fname, texOptions, backend) # local copy w/ correct backend
  let plt = p.ggcreate(width = width, height = height, dataAsBitmap = dataAsBitmap)
  # make sure the target directory exists, create if not
  createDir(fname.expandTilde().splitFile().dir)
  plt.view.ggdraw(fname, texOptions)

  when defined(WritePlotCsv):
    ## You can activate this branch by compiling with `-d:WritePlotCsv`. If you then
    ## define the environment variable `WRITE_PLOT_CSV` `ggplotnim` will write the a
    ## CSV file for every plot you create with the same path and name as the output plot
    ## but `.csv` suffix. This is extremely useful if you wish to make the data for all
    ## your plots easily available (and for all their annoyances this is one of the use
    ## cases where CSV files are handy :/)
    let toWrite = getEnv("WRITE_PLOT_CSV", "false").parseBool
    if toWrite:
      proc write(df: DataFrame, fname: string) =
        echo "[INFO] Writing CSV file: ", fname
        df.writeCsv(fname)

      let csvName = fname & ".csv"
      write(p.data, csvName)
      # now write any geom associated DF with a `geom_<idx>_<geomKind>.csv` suffix
      for i, g in p.geoms:
        if g.data.isSome:
          write(g.data.get, csvName.replace(".csv", &"_geom_{i}_{g.kind}.csv"))

proc ggsave*(p: GgPlot, fname: string, width = 640.0, height = 480.0,
             useTeX = false,
             onlyTikZ = false,
             standalone = false,
             texTemplate = "",
             caption = "",
             label = "",
             placement = "htbp",
             dataAsBitmap = false
            ) =
  ## This is the same as the `ggsave` proc below for the use case of calling it
  ## directly on a `GgPlot` object with the possible TeX options.
  ##
  ## See the docstring below.
  let texOptions = toTeXOptions(useTeX, onlyTikZ, standalone, texTemplate,
                                caption, label, placement)
  p.ggsave(fname, width, height, texOptions, dataAsBitmap = dataAsBitmap)

proc ggsave*(fname: string, width = 640.0, height = 480.0,
             useTeX = false,
             onlyTikZ = false,
             standalone = false,
             texTemplate = "",
             caption = "",
             label = "",
             placement = "htbp",
             backend = bkNone,
             dataAsBitmap = false
            ): Draw =
  ## Generates the plot and saves it as `fname` with the given
  ## `width` and `height`.
  ##
  ## Possible file types:
  ## - `png`
  ## - `svg`
  ## - `pdf`
  ## - `tex`
  ##
  ##
  ## The `backend` argument can be used to overwrite the logic that is normally used to
  ## determine the backend used to save the figures. Note that different backends only
  ## support different file types. Currently there are no safeguards for mismatching
  ## backends and file types!
  ## Available backends:
  ##
  ## - `bkCairo`: default backend, supports `png`, `pdf`, `svg`, `jpg`
  ## - `bkTikZ`: TikZ backend to generate native LaTeX files or PDFs from TeX
  ## - `bkPixie`: pure Nim backend supporting `png`
  ## - `bkVega:`: Vega-Lite backend for interactive graphs in the browser
  ##
  ## If the output file is to be stored as a `pdf`, `useTeX` decides whether to
  ## create the file using Cairo or a local LaTeX installation (by default system
  ## `xelatex` if available, with `pdflatex` as the fallback). In case `useTeX` is
  ## `true`, `standalone` is always taken to be `true` (unless a `texTemplate` is given).
  ##
  ## If the File type is `tex`, `onlyTikZ` determines whether to output ``only`` the
  ## TikZ code to a file or create a full TeX document (that can be directly compiled).
  ##
  ## Further, `standalone` decides what kind of document is created in case `onlyTikZ`
  ## is false. `standalone` means it's meant as a TeX file that only contains the plot
  ## and produces a cropped plot upon compilation. If `standalone` is `false` the document
  ## is an `article`.
  ##
  ## The priority of `onlyTikZ`, `standalone` and `texTemplate` is as follows:
  ## 1. `texTemplate`: if given will be used regardless of the others
  ## 2. `onlyTikZ`: higher precedence than standalone
  ## 3. `standalone`: only chosen if above two are `false` / empty
  ##
  ## Further, if a `texTemplate` is given that template is used to embed the `TikZ` code.
  ## The template ``must`` contain three `$#` for the location at which the `TikZ` code
  ## is to be embeded. An example (using `latexdsl`) of where these must be:
  ##
  ## ```latex
  ##    \documentclass[a4paper]{article}
  ##    \usepackage[utf8]{inputenc}
  ##    \usepackage[margin="2.5cm"]{geometry}
  ##    \usepackage{unicode-math} # for unicode support in math environments
  ##    \usepackage{amsmath}
  ##    \usepackage{siunitx}
  ##    \usepackage{tikz}
  ##    "$#"
  ##    document:
  ##      "$#"
  ##      center:
  ##        tikzpicture:
  ##          "$#"
  ##
  ## ```
  ##
  ## The first is an additional header (currently unused). The second is a header for the document body.
  ## This is where for example the page color is set based on the plot background. Finally the third is
  ## the actual TikZ code inserted. Generally it is a good idea to start with the above template and
  ## adjust the type of article, packages to use etc. as needed.
  ##
  ##
  ## Finally, if a `caption` and / or `label` are given, the output will wrap the `tikzpicture`
  ## in a figure environment, with placement options `placement`.
  ##
  ## The default TeX templates are found here:
  ## https://github.com/Vindaar/ginger/blob/master/src/ginger/backendTikZ.nim#L244-L274
  ##
  ## The required TeX packages are thus: `inputenc, geometry, unicode-math, amsmath, siunitx, tikz`.
  ##
  ## Note: `unicode-math` is incompatible with `pdflatex`!
  ##
  ## Note 2: Placing text on the TikZ backend comes with some quirks:
  ##
  ## 1. text placement may be *slightly* different than on the Cairo backend, as we currently
  ##   use a hack to determine string widths / heights based on font size alone. `ginger` needs
  ##   an overhaul to handle embedding of coordinates into viewports to keep string width / height
  ##   information until the locations are written to the output file (so that we can make use
  ##   of text size information straight from TeX)
  ## 2. Text is placed into TikZ `node` elements. These have some quirky behavior for more
  ##   complex LaTeX constructs. E.g. it is not really possible to use an `equation` environment
  ##   in them (leads to "Missing $ inserted" errors).
  ## 3. because of hacky string width / height determination placing a non transparent background
  ##   for annotations leads to background rectangles that are too small. Keep the background color
  ##   transparent for the time being.
  ## 4. Do not include line breaks `\n` in your annotations if you wish to
  ##   let LaTeX handle line breaks for you. Any manual line break `\n`
  ##   will be handled by `ginger`. Due to the string height hack, this
  ##   can give somewhat ugly results.
  let texOptions = toTeXOptions(useTeX, onlyTikZ, standalone, texTemplate,
                                caption, label, placement)
  Draw(fname: fname,
       width: some(width),
       height: some(height),
       texOptions: texOptions,
       backend: backend,
       dataAsBitmap: dataAsBitmap)

proc `+`*(p: GgPlot, d: Draw) =
  if d.width.isSome and d.height.isSome:
    p.ggsave(d.fname,
             width = d.width.get,
             height = d.height.get,
             texOptions = d.texOptions,
             backend = d.backend,
             dataAsBitmap = d.dataAsBitmap)
  else:
    p.ggsave(d.fname, texOptions = d.texOptions,
             backend = d.backend,
             dataAsBitmap = d.dataAsBitmap)

from json import `%`
proc ggvega*[PB: PossibleBool](
  fname = "", width = 640.0, height = 480.0,
  pretty: PB = missing(),
  show = true,
  backend = "webview",
  removeFile = true,
  divName = "vis",
  vegaLibsPath = "https://cdn.jsdelivr.net/npm/",
  vegaVersion = "5",
  vegaLiteVersion = "4",
  vegaEmbedVersion = "6"
                             ): VegaDraw =
  ## If a filename with `.json` ending is given we simply generate a Vega compatible
  ## JSON. In case no filename is given a temporary HTML file in the temp directory
  ## (obtained via `getTempDir`) is created and the plot is shown, unless the
  ## `show` flag is `false`.
  ##
  ## For other given directories, intermediate directories will be created.
  ##
  ## If no filename is given and the plot is shown immediately, the temporary file will
  ## be removed after unless `removeFile` is `false`.
  ##
  ## If `pretty` is given, overwrites defaults for pretty printing of JSON. By default
  ## pretty printing is used when generating a JSON file and no pretty printing is used
  ## when generating an HTML file.
  ##
  ## The backend to show the Vega plot in can be chosen using `backend`. Valid backends
  ## are `"webview"` and `"browser"`.
  let backend = parseEnum[VegaBackend](backend)
  let optPretty = pretty.toOptBool()
  VegaDraw(fname: fname, width: some(width),
           height: some(height), asPrettyJson: optPretty,
           show: show,
           backend: backend,
           removeFile: removeFile,
           divName: divName,
           vegaLibsPath: vegaLibsPath,
           vegaVersion: vegaVersion,
           vegaLiteVersion: vegaLiteVersion,
           vegaEmbedVersion: vegaEmbedVersion)

proc ggvegatex*(fname: string, width = 640.0, height = 480.0,
                caption = "",
                label = "",
                placement = "htbp"): VegaTeX =
  ## Generates two versions of of the given plot. The filename should `not` contain any
  ## extension. We will generate a `.tex` file and a `.json` file.
  ##
  ## The TeX file is suppposed to be inserted (using `\input`) into the LaTeX file.
  ##
  ## The JSON file should be stored as a GitHub gist from which it can be imported into the
  ## Vega-Lite viewer.
  ##
  ## NOTE: The `width` and `height` arguments don't have any purpose at this time.
  let texOptions = toTeXOptions(true, true, false, "",
                                caption, label, placement)
  VegaTeX(fname: fname,
          width: some(width), height: some(height),
          texOptions: texOptions)

proc `%`*(t: tuple): json.JsonNode =
  result = json.newJObject()
  for k, v in t.fieldPairs:
    json.`[]=`(result, k, %v)

proc ggjson*(fname: string, width = 640.0, height = 480.0, backend = bkCairo): JsonDummyDraw =
  let (_, _, fext) = fname.splitFile
  JsonDummyDraw(fname: fname.replace(fext, ".json"),
                width: some(width),
                height: some(height),
                backend: backend)

proc `%`(fn: proc(): seq[uint32] {.closure.}): json.JsonNode =
  result = % fn()

proc `+`*(p: GgPlot, jsDraw: JsonDummyDraw) =
  ## generate a JSON file from the given filename by replacing the file
  ## extension by `.json` and converting the `Viewport` to JSON after
  ## calling `ggcreate`. Used for CI.
  doAssert jsDraw.width.isSome and jsDraw.height.isSome
  var p = p
  p.backend = jsDraw.backend
  let plt = p.ggcreate(width = jsDraw.width.get,
                       height = jsDraw.height.get)
  writeFile(jsDraw.fname, json.`$`(% plt.view))

when defined(experimentalSDL2):
  import ggplotnim/ggplot_sdl2
  export ggplot_sdl2
