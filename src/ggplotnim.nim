## .. include:: ./docs/ggplotnim.rst

import sequtils, tables, sets, algorithm, strutils, macros
import parsecsv, streams, hashes, sugar, math
from os import createDir, splitFile

when (NimMajor, NimMinor, NimPatch) > (1, 3, 0):
  export strutils.nimIdentNormalize # for parseEnum to work

import ginger except Scale
export ginger.types

from seqmath import linspace

import persvector
export persvector

when defined(defaultBackend):
  import ./ggplotnim/dataframe/fallback/formula
  export formula
else:
  import ./ggplotnim/dataframe/arraymancer_backend
  export arraymancer_backend

import ggplotnim / [
  ggplot_utils, ggplot_types,
  # utils dealing with scales
  ggplot_scales,
  # first stage of drawing: collect and fill `Scales`:
  collect_and_fill,
  # second stage of drawing: post process the scales (not required to be imported
  # in this module)
  # postprocess_scales,
  ggplot_drawing, # third stage: the actual drawing
  # vega backend
  vega_utils
]
export ggplot_types
export ggplot_utils

import ggplotnim / colormaps / viridisRaw

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

proc orNone(s: string): Option[string] =
  ## returns either a `some(s)` if s.len > 0 or none[string]()
  if s.len == 0: none[string]()
  else: some(s)

proc orNone(f: float): Option[float] =
  ## returns either a `some(f)` if `classify(f) != NaN` or none[float]()
  if classify(f) != fcNaN: some(f)
  else: none[float]()

proc orNoneScale*[T: string | SomeNumber | FormulaNode](
  s: T, scKind: static ScaleKind,
  axKind = akX,
  hasDiscreteness = false): Option[Scale] =
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
                          hasDiscreteness: hasDiscreteness))
    of scTransformedData:
      result = some(Scale(scKind: scTransformedData, col: fs, axKind: axKind,
                          hasDiscreteness: hasDiscreteness))
    else:
      result = some(Scale(scKind: scKind, col: fs,
                          hasDiscreteness: hasDiscreteness))
  else:
    result = none[Scale]()

template hasFactor(n: NimNode): untyped = n.kind == nnkCall and n[0].strVal == "factor"

proc initField*(name: string, val: NimNode): NimNode =
  # determine if magic `factor` used to designate a scale as discrete
  let hasDiscreteness = hasFactor(val)
  # if so, use the actual arg as value
  var val = if hasDiscreteness: val[1] else: val
  template call(kind: untyped, ax = akX): untyped =
    result = nnkCall.newTree(ident"orNoneScale", val, newLit kind, newLit ax,
                             newLit hasDiscreteness)
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
    # either function call returning some normal value or magic `factor`
    if hasFactor(n):
      result = nnkCall.newTree(n[0], getArgValue(n[1], arg))
    else:
      result = n
  of nnkCommand:
    # might be an argument like `fn {someFormula}`
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
  expectKind(args, nnkArglist)
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
             numXTicks = 10, numYTicks = 10): GgPlot =
  result = GgPlot(data: data,
                  numXticks: numXTicks,
                  numYticks: numYTicks)
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

func initGgStyle(color = none[Color](),
                 size = none[float](),
                 marker = none[MarkerKind](),
                 lineType = none[LineType](),
                 lineWidth = none[float](),
                 fillColor = none[Color](),
                 errorBarKind = none[ErrorBarKind](),
                 alpha = none[float](),
                 font = none[Font]()): GgStyle =
  result = GgStyle(color: color,
                   size: size,
                   marker: marker,
                   lineType: lineType,
                   lineWidth: lineWidth,
                   fillColor: fillColor,
                   errorBarKind: errorBarKind,
                   alpha: alpha,
                   font: font)

proc geom_point*(aes: Aesthetics = aes(),
                 data = DataFrame(),
                 color = none[Color](),
                 size = none[float](),
                 marker = none[MarkerKind](),
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
  let style = initGgStyle(color = color, size = size, marker = marker)
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

proc geom_errorbar*(aes: Aesthetics = aes(),
                    data = DataFrame(),
                    color = none[Color](),
                    size = none[float](),
                    lineType = none[LineType](),
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
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  let style = initGgStyle(color = color, size = size, lineType = lineType,
                          errorBarKind = some(errorBarKind))
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

proc geom_linerange*(aes: Aesthetics = aes(),
                     data = DataFrame(),
                     color = none[Color](),
                     size = none[float](),
                     lineType = none[LineType](),
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
  let style = initGgStyle(color = color, size = size, lineType = lineType,
                          errorBarKind = some(ebLines))
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


proc geom_bar*(aes: Aesthetics = aes(),
               data = DataFrame(),
               color = none[Color](), # color of the bars
               alpha = none[float](),
               position = "stack",
               stat = "count",
              ): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  let style = initGgStyle(lineType = some(ltSolid),
                          lineWidth = some(1.0), # draw 1 pt wide black line to avoid white pixels
                                                 # between bins at size of exactly 1.0 bin width
                          color = color,
                          fillColor = color,
                          alpha = alpha)
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkBar,
                aes: aes.fillIds({gid}),
                userStyle: style,
                position: pkKind,
                binPosition: bpNone,
                statKind: stKind)

proc geom_line*(aes: Aesthetics = aes(),
                data = DataFrame(),
                color = none[Color](), # color of the line
                size = none[float](), # width of the line
                lineType = none[LineType](), # type of line
                fillColor = none[Color](),
                alpha = none[float](),
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
  let style = initGgStyle(color = color, lineWidth = size, lineType = lineType,
                          fillColor = fillColor, alpha = alpha)
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkLine,
                userStyle: style,
                aes: aes.fillIds({gid}),
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_histogram*(aes: Aesthetics = aes(),
                     data = DataFrame(),
                     binWidth = 0.0, bins = 30,
                     breaks: seq[float] = @[],
                     color = none[Color](), # color of the bars
                     alpha = none[float](),
                     position = "stack",
                     stat = "bin",
                     binPosition = "left",
                     binBy = "full",
                     density = false
                    ): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let bbKind = parseEnum[BinByKind](binBy)
  let style = initGgStyle(lineType = some(ltSolid),
                          lineWidth = some(0.2), # draw 0.2 pt wide black line to avoid white pixels
                                                 # between bins at size of exactly 1.0 bin width
                          color = color, # default color
                          fillColor = color,
                          alpha = alpha)
  let gid = incId()
  result = Geom(gid: gid,
                data: dfOpt,
                kind: gkHistogram,
                aes: aes.fillIds({gid}),
                userStyle: style,
                position: pkKind,
                binPosition: bpKind,
                statKind: stKind)
  assignBinFields(result, stKind, bins, binWidth, breaks, bbKind, density)

proc geom_freqpoly*(aes: Aesthetics = aes(),
                    data = DataFrame(),
                    color = none[Color](), # color of the line
                    size = none[float](), # line width of the line
                    lineType = none[LineType](),
                    fillColor = none[Color](),
                    alpha = none[float](),
                    bins = 30,
                    binWidth = 0.0,
                    breaks: seq[float] = @[],
                    position = "identity",
                    stat = "bin",
                    binPosition = "center",
                    binBy = "full",
                    density = false
                   ): Geom =
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let pkKind = parseEnum[PositionKind](position)
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let bbKind = parseEnum[BinByKind](binBy)
  let style = initGgStyle(lineType = lineType,
                          lineWidth = size,
                          color = color,
                          fillColor = fillColor,
                          alpha = alpha)
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

proc geom_tile*(aes: Aesthetics = aes(),
                data = DataFrame(),
                color = none[Color](),
                fillColor = none[Color](),
                alpha = none[float](),
                size = none[float](),
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
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  let style = initGgStyle(color = color, fillColor = fillColor, size = size,
                          alpha = alpha)
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

proc geom_raster*(aes: Aesthetics = aes(),
                  data = DataFrame(),
                  color = none[Color](),
                  fillColor = none[Color](),
                  alpha = none[float](),
                  size = none[float](),
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
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  let style = initGgStyle(color = color, fillColor = fillColor, size = size,
                          alpha = alpha)
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


proc geom_text*(aes: Aesthetics = aes(),
                data = DataFrame(),
                color = none[Color](),
                size = none[float](),
                marker = none[MarkerKind](),
                font = none[Font](),
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
  let dfOpt = if data.len > 0: some(data) else: none[DataFrame]()
  let stKind = parseEnum[StatKind](stat)
  let bpKind = parseEnum[BinPositionKind](binPosition)
  let pKind = parseEnum[PositionKind](position)
  let bbKind = parseEnum[BinByKind](binBy)
  let fontOpt = if font.isSome: font
                else: some(font(12.0, alignKind = alignKind))
  let style = initGgStyle(color = color, size = size,
                          marker = marker, font = fontOpt)
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
                                          scales = "fixed"): Facet =
  let sfKind = parseEnum[ScaleFreeKind](scales)
  result = Facet(sfKind: sfKind)
  for f in fns:
    when T is FormulaNode:
      when defined(defaultBackend):
        doAssert f.kind == fkTerm
        doAssert f.rhs.val.kind == VString
        result.columns.add f.rhs.val.str
      else:
        doAssert f.kind == fkVariable
        result.columns.add f.name
    else:
      result.columns.add f

proc scale_x_log10*(): Scale =
  ## sets the X scale of the plot to a log10 scale
  when defined(defaultBackend):
    let trans = proc(v: Value): Value =
      result = %~ log10(v.toFloat)

  else:
    let trans = proc(v: float): float =
      result = log10(v)

  result = Scale(col: f{""}, # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akX,
                 dcKind: dcContinuous,
                 trans: trans)

proc scale_y_log10*(): Scale =
  ## sets the Y scale of the plot to a log10 scale
  when defined(defaultBackend):
    let trans = proc(v: Value): Value =
      result = %~ log10(v.toFloat)

  else:
    let trans = proc(v: float): float =
      result = log10(v)

  result = Scale(col: f{""}, # will be filled when added to GgPlot obj
                 scKind: scTransformedData,
                 axKind: akY,
                 dcKind: dcContinuous,
                 trans: trans)

func sec_axis*(trans: FormulaNode = f{""}, name: string = ""): SecondaryAxis =
  ## convenience proc to create a `SecondaryAxis`
  var fn: Option[FormulaNode]
  when defined(defaultBackend):
    if not trans.isNil:
      fn = some(trans)
  else:
    if trans.name.len > 0:
      fn = some(trans)
  result = SecondaryAxis(trans: fn,
                         name: name)

proc scale_x_discrete*(name: string = "",
                       secAxis: SecondaryAxis = sec_axis(),
                       labels: proc(x: Value): string = nil): Scale =
  ## creates a discrete x axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the x axis to the tick label that should be shown for it.
  var msecAxis: SecondaryAxis
  var secAxisOpt: Option[SecondaryAxis]
  if secAxis.name.len > 0:
    msecAxis = secAxis
    msecAxis.axKind = akX
    secAxisOpt = some(msecAxis)
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akX,
                 dcKind: dcDiscrete,
                 hasDiscreteness: true,
                 secondaryAxis: secAxisOpt,
                 formatDiscreteLabel: labels)

proc scale_x_continuous*(name: string = "",
                         secAxis: SecondaryAxis = sec_axis(),
                         labels: proc(x: float): string = nil): Scale =
  ## creates a continuous x axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the x axis to the tick label that should be shown for it.
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
                 hasDiscreteness: true,
                 secondaryAxis: secAxisOpt,
                 formatContinuousLabel: labels)

proc scale_y_continuous*(name: string = "",
                         secAxis: SecondaryAxis = sec_axis(),
                         labels: proc(x: float): string = nil): Scale =
  ## creates a continuous y axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the y axis to the tick label that should be shown for it.
  # Also the possible transformation for the secondary axis is ignored!
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
                 hasDiscreteness: true,
                 secondaryAxis: secAxisOpt,
                 formatContinuousLabel: labels)

proc scale_y_discrete*(name: string = "",
                       secAxis: SecondaryAxis = sec_axis(),
                       labels: proc(x: Value): string = nil): Scale =
  ## creates a discrete y axis with a possible secondary axis.
  ## `labels` allows to hand a procedure, which maps the values
  ## found on the x axis to the tick label that should be shown for it.
  var msecAxis: SecondaryAxis
  var secAxisOpt: Option[SecondaryAxis]
  if secAxis.name.len > 0:
    msecAxis = secAxis
    msecAxis.axKind = akY
    secAxisOpt = some(msecAxis)
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akY,
                 dcKind: dcDiscrete,
                 hasDiscreteness: true,
                 secondaryAxis: secAxisOpt,
                 formatDiscreteLabel: labels)

proc scale_x_reverse*(name: string = "",
                      secAxis: SecondaryAxis = sec_axis(),
                      dcKind: DiscreteKind = dcContinuous): Scale =
  ## creates a continuous x axis with a possible secondary axis, which
  ## is reversed
  var msecAxis: SecondaryAxis
  var secAxisOpt: Option[SecondaryAxis]
  if secAxis.name.len > 0:
    msecAxis = secAxis
    msecAxis.axKind = akX
    secAxisOpt = some(msecAxis)
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akX,
                 dcKind: dcKind,
                 reversed: true,
                 hasDiscreteness: true,
                 secondaryAxis: secAxisOpt)

proc scale_y_reverse*(name: string = "",
                      secAxis: SecondaryAxis = sec_axis(),
                      dcKind: DiscreteKind = dcContinuous): Scale =
  ## creates a continuous y axis with a possible secondary axis, which
  ## is reversed
  var msecAxis: SecondaryAxis
  var secAxisOpt: Option[SecondaryAxis]
  if secAxis.name.len > 0:
    msecAxis = secAxis
    msecAxis.axKind = akY
    secAxisOpt = some(msecAxis)
  result = Scale(name: name,
                 scKind: scLinearData,
                 axKind: akY,
                 dcKind: dcKind,
                 reversed: true,
                 hasDiscreteness: true,
                 secondaryAxis: secAxisOpt)

proc scale_fill_continuous*(name: string = ""): Scale =
  result = Scale(name: name,
                 scKind: scFillColor,
                 dcKind: dcContinuous,
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

proc scale_size_manual*[T](values: Table[T, float]): Scale =
  ## allows to set custom sizes, by handing a table mapping the
  ## keys found in the size column to sizes.
  result = Scale(scKind: scSize,
                 hasDiscreteness: true,
                 dcKind: dcDiscrete)
  result.labelSeq = newSeq[Value](values.len)
  let keys = toSeq(keys(values)).sorted
  for i, k in keys:
    let kVal = %~ k
    result.valueMap[kVal] = ScaleValue(kind: scSize, size: values[k])
    result.labelSeq[i] = kVal

proc ggtitle*(title: string, subtitle = "",
              titleFont = font(), subTitleFont = font(8.0)): Theme =
  result = Theme(title: some(title))
  if subtitle.len > 0:
    result.subTitle = some(subTitle)
  if titleFont != font():
    result.titleFont = some(titleFont)
  if subTitleFont != font():
    result.subTitleFont = some(subTitleFont)

proc generateLegendMarkers(plt: Viewport, scale: Scale,
                           accessIdx: Option[seq[int]]): seq[GraphObject]
proc genDiscreteLegend(view: var Viewport,
                       cat: Scale,
                       accessIdx: Option[seq[int]]) =
  # TODO: add support for legend font in Theme / `let label` near botton!
  # _______________________
  # |   | Headline        |
  # |______________________
  # |1cm| 1cm |  | space  |
  # |   |grad.|.5| for    |
  # |   |     |cm| leg.   |
  # |   |     |  | labels |
  # -----------------------
  let markers = view.generateLegendMarkers(cat, accessIdx)
  let numElems = cat.valueMap.len
  view.layout(2, 2,
              colWidths = @[quant(0.5, ukCentimeter), # for space to plot
                            quant(0.0, ukRelative)], # for legend. incl header
              rowHeights = @[quant(1.0, ukCentimeter), # for header
                             quant(1.05 * numElems.float, ukCentimeter)]) # for act. legend
  var i = 0
  # now set the `height` according to the real legend height. This important
  # to get proper alignment of the scale / multiple scales in `finalizeLegend`!
  view.height = quant(1.0 + 1.05 * numElems.float, ukCentimeter)
  var leg = view[3]

  let rH = toSeq(0 ..< numElems).mapIt(quant(1.05, ukCentimeter))

  leg.layout(3, rows = numElems,
             colWidths = @[quant(1.0, ukCentimeter),
                           quant(0.3, ukCentimeter),
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
                                     y: c1(0.0) + legBox.c1(0.025, akY, ukCentimeter)),
                               quant(1.0, ukCentimeter),
                               quant(1.0, ukCentimeter),
                               style = some(style),
                               name = "markerRectangle")
    # add marker ontop of rect
    ## TODO: choose marker type based on geom!
    let point = legBox.initPoint(Coord(x: c1(0.5),
                                       y: c1(0.5)),
                                 marker = markers[j].ptMarker,
                                 size = markers[j].ptSize,
                                 color = markers[j].ptColor,
                                 name = "markerPoint")
    var labelText = ""
    case cat.scKind
    of scColor, scFillColor, scShape, scSize:
      labelText = markers[j].name
    else:
      raise newException(Exception, "`createLegend` unsupported for " & $cat.scKind)
    let label = legLabel.initText(
      Coord(
        x: c1(0.0),
        y: c1(0.5)),
      labelText,
      textKind = goText,
      alignKind = taLeft,
      name = "markerText"
    )
    legBox.addObj [rect, point]
    legLabel.addObj label
    leg[i] = legBox
    leg[i + 2] = legLabel
    inc j
  view[3] = leg

proc genContinuousLegend(view: var Viewport,
                         cat: Scale,
                         accessIdx: Option[seq[int]]) =
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
    view.layout(2, 2,
                colWidths = @[quant(0.5, ukCentimeter), # for space to plot
                              quant(0.0, ukRelative)], # for legend. incl header
                rowHeights = @[quant(1.0, ukCentimeter), # for header
                               quant(4.5, ukCentimeter)]) # for act. legend
    var legView = view[3]
    legView.yScale = cat.dataScale
    legView.layout(3, 1, colWidths = @[quant(1.0, ukCentimeter),
                                       quant(0.5, ukCentimeter),
                                       quant(0.0, ukRelative)])
    var legGrad = legView[0]
    # add markers
    let markers = legGrad.generateLegendMarkers(cat, accessIdx)
    legGrad.addObj markers
    let viridis = ViridisRaw.mapIt(color(it[0], it[1], it[2]))
    let cc = some(Gradient(colors: viridis))
    let gradRect = legGrad.initRect(c(0.0, 0.0),
                                    quant(1.0, ukRelative),
                                    quant(1.0, ukRelative),
                                    name = "legendGradientBackground",
                                    gradient = cc)
    legGrad.addObj gradRect
    legView[0] = legGrad
    view[3] = legView
    view.height = quant(5.5, ukCentimeter)
  else:
    discard

proc createLegend(view: var Viewport,
                  cat: Scale,
                  accessIdx: Option[seq[int]]) =
  ## creates a full legend within the given viewport based on the categories
  ## in `cat` with a headline `title` showing data points of `markers`
  let startIdx = view.len
  case cat.dcKind
  of dcDiscrete:
    view.genDiscreteLegend(cat, accessIdx)
  of dcContinuous:
    # for now 5 sizes...
    view.genContinuousLegend(cat, accessIdx)

  # get the first viewport for the header
  if startIdx < view.len:
    var header = view[1]
    # TODO: add support to change font of legend
    var label = header.initText(
      Coord(x: c1(0.0),
            y: c1(0.5)),
      evaluate(cat.col).toStr,
      textKind = goText,
      alignKind = taLeft,
      name = "legendHeader")
    # set to bold
    label.txtFont.bold = true
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

proc legendPosition*(x = 0.0, y = 0.0): Theme =
  ## puts the legend at position `(x, y)` in relative coordinates of
  ## the plot viewport in range (0.0 .. 1.0)
  result = Theme(legendPosition: some(Coord(x: c1(x),
                                            y: c1(y))))

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

proc canvasColor*(color: Color): Theme =
  ## sets the canvas color of the plot to the given color
  result = Theme(canvasColor: some(color))

func theme_opaque*(): Theme =
  ## returns the "opaque" theme. For the time being this only means the
  ## canvas of the plot is white instead of transparent
  result = Theme(canvasColor: some(white))

func theme_void*(): Theme =
  ## returns the "void" theme. This means:
  ## - white background
  ## - no grid lines
  ## - no ticks
  ## - no tick labels
  ## - no labels
  result = Theme(canvasColor: some(white),
                 plotBackgroundColor: some(white),
                 hideTicks: some(true),
                 hideTickLabels: some(true),
                 hideLabels: some(true))

proc parseTextAlignString(alignTo: string): Option[TextAlignKind] =
  case alignTo.normalize
  of "none": result = none[TextAlignKind]()
  of "left": result = some(taLeft)
  of "right": result = some(taRight)
  of "center": result = some(taCenter)
  else: result = none[TextAlignKind]()

proc xlab*(label = "", margin = NaN, rotate = NaN,
           alignTo = "none", font = font(), tickFont = font()): Theme =
  if label.len > 0:
    result.xlabel = some(label)
  if classify(margin) != fcNaN:
    result.xlabelMargin = some(margin)
  if classify(rotate) != fcNaN:
    result.xTicksRotate = some(rotate)
  if font != font():
    result.labelFont = some(font)
  if tickFont != font():
    result.tickLabelFont = some(tickFont)
  result.xTicksTextAlign = parseTextAlignString(alignTo)

proc ylab*(label = "", margin = NaN, rotate = NaN,
           alignTo = "none", font = font(), tickFont = font()): Theme =
  if label.len > 0:
    result.ylabel = some(label)
  if classify(margin) != fcNaN:
    result.ylabelMargin = some(margin)
  if classify(rotate) != fcNaN:
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
  result = Theme(xRange: some((low: low.float, high: high.float)),
                 xOutsideRange: orOpt)

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
  result = Theme(yRange: some((low: low.float, high: high.float)),
                 yOutsideRange: orOpt)

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

proc annotate*(text: string,
               left = NaN,
               bottom = NaN,
               x = NaN,
               y = NaN,
               font = font(12.0),
               backgroundColor = white): Annotation =
  ## creates an annotation of `text` with a background
  ## `backgroundColor` (by default white) using the given
  ## `font`. Line breaks are supported.
  ## It is placed either at:
  ## - `(left, bottom)`, where these correspond to relative coordinates
  ##   mapping out the plot area as (0.0, 1.0). NOTE: smaller and larger
  ##   values than 0.0 and 1.0 are supported and will put the annotation outside
  ##   the plot area.
  ## - `(x, y)` where `x` and `y` are values in the scale of the data
  ##   being plotted. This is useful if the annotation is to be placed relative
  ##   to specific data points. NOTE: for a discrete axis data scale is not
  ##   well defined, thus we fall back to relative scaling on that axis!
  ## In principle you can mix and match left/x and bottom/y! If both are given
  ## the former will be prioritized.
  result = Annotation(left: left.orNone,
                      bottom: bottom.orNone,
                      x: x.orNone,
                      y: y.orNone,
                      text: text,
                      font: font,
                      backgroundColor: backgroundColor)
  if result.x.isNone and result.left.isNone or
     result.y.isNone and result.bottom.isNone:
    raise newException(ValueError, "Both an x/left and y/bottom position has to " &
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
  # TODO: about time we make this a macro...
  ifSome(xlabelMargin)
  ifSome(ylabelMargin)
  ifSome(xLabel)
  ifSome(yLabel)
  ifSome(xTicksTextAlign)
  ifSome(yTicksTextAlign)
  ifSome(xTicksRotate)
  ifSome(yTicksRotate)
  ifSome(legendPosition)
  ifSome(legendOrder)
  ifSome(labelFont)
  ifSome(tickLabelFont)
  ifSome(titleFont)
  ifSome(subTitleFont)
  ifSome(tickLabelFont)
  ifSome(title)
  ifSome(subTitle)
  ifSome(plotBackgroundColor)
  ifSome(canvasColor)
  ifSome(xRange)
  ifSome(yRange)
  ifSome(xMargin)
  ifSome(yMargin)
  ifSome(xOutsideRange)
  ifSome(yOutsideRange)
  ifSome(hideTicks)
  ifSome(hideTickLabels)
  ifSome(hideLabels)
  ifSome(plotMarginLeft)
  ifSome(plotMarginRight)
  ifSome(plotMarginTop)
  ifSome(plotMarginBottom)

proc `+`*(p: GgPlot, theme: Theme): GgPlot =
  ## adds the given theme (or theme element) to the GgPlot object
  result = p
  applyTheme(result.theme, theme)
  # TODO: Maybe move these two completely out of `GgPlot` object
  if result.theme.title.isSome:
    result.title = result.theme.title.get
  if result.theme.subTitle.isSome:
    result.subTitle = result.theme.subTitle.get

proc applyScale(aes: Aesthetics, scale: Scale): Aesthetics =
  ## applies the given `scale` to the `aes` by returning a modified
  ## `aes`
  template clone(newScale: untyped): untyped =
    when defined(gcDestructors):
      `newScale`[] = scale[]
    else:
      `newScale` = deepCopy(scale)
  template assignCopyScale(field: untyped): untyped =
    if aes.field.isSome:
      var mscale: Scale
      clone(mscale)
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

proc initThemeMarginLayout(theme: Theme,
                           tightLayout: bool,
                           requiresLegend: bool): ThemeMarginLayout =
  result = ThemeMarginLayout(
    left: if theme.plotMarginLeft.isSome: theme.plotMarginLeft.get
          elif tightLayout: quant(0.2, ukCentimeter)
          else: quant(2.5, ukCentimeter),
    right: if theme.plotMarginRight.isSome: theme.plotMarginRight.get
           elif requiresLegend: quant(5.0, ukCentimeter)
           else: quant(1.0, ukCentimeter),
    top: if theme.plotMarginTop.isSome: theme.plotMarginTop.get
         # this is not really a good solution. Legacy. Should depend on whether
         # there is a title instead!
         elif requiresLegend: quant(1.25, ukCentimeter)
         else: quant(1.0, ukCentimeter),
    bottom: if theme.plotMarginBottom.isSome: theme.plotMarginBottom.get
            else: quant(2.0, ukCentimeter),
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
  let hideTickLabels = theme.hideTickLabels.get(false)
  let hideLabels = theme.hideLabels.get(false)
  let tightLayout = hideLabels and hideTicks
  let layout = initThemeMarginLayout(theme, tightLayout, filledScales.requiresLegend)
  view.plotLayout(layout)

proc generateLegendMarkers(plt: Viewport,
                           scale: Scale,
                           accessIdx: Option[seq[int]]): seq[GraphObject] =
  ## generate the required Legend Markers for the given `aes`
  ## TODO: add different objects to be shown depending on the scale and geom.
  ## E.g. in case of `fill` fill the whole rectangle with the color. In case
  ## of geom_line only draw a line etc.
  ## Thus also put the rectangle drawing here.
  case scale.dcKind
  of dcDiscrete:
    let idx = if accessIdx.isNone: toSeq(0 ..< scale.valueMap.len) else: accessIdx.get
    doAssert idx.len == scale.valueMap.len,
      "Custom ordering of legend keys must assign each key only once! " &
      "Assigned keys: " & $accessIdx & " for num keys: " & $scale.valueMap.len
    case scale.scKind
    of scColor, scFillColor:
      for i in idx:
        let color = scale.getValue(scale.getLabelKey(i)).color
        result.add initPoint(plt,
                             (0.0, 0.0), # dummy coordinates
                             marker = mkCircle,
                             color = color,
                             name = $scale.getLabelKey(i)) # assign same marker as above

    of scShape:
      for i in idx:
        result.add initPoint(plt,
                             (0.0, 0.0), # dummy coordinates
                             marker = scale.getValue(scale.getLabelKey(i)).marker,
                             name = $scale.getLabelKey(i))
    of scSize:
      for i in idx:
        let size = scale.getValue(scale.getLabelKey(i)).size
        result.add initPoint(plt,
                             (0.0, 0.0), # dummy coordinates
                             marker = mkCircle,
                             size = size,
                             name = $scale.getLabelKey(i))
    else:
      raise newException(Exception, "`createLegend` unsupported for " & $scale.scKind)
  of dcContinuous:
    case scale.sckind
    of scColor, scFillColor:
      # replace yScale by scale of `scale`
      var mplt = plt
      mplt.yScale = scale.dataScale
      # use 5 ticks by default
      # define as "secondary" because then ticks will be on the RHS
      let ticks = mplt.initTicks(akY, 5, boundScale = some(scale.dataScale),
                                 isSecondary = true)
      let tickLabs = mplt.tickLabels(ticks, isSecondary = true,
                                     margin = some(plt.c1(0.3, akX, ukCentimeter)),
                                     format = scale.formatContinuousLabel)
      result = concat(tickLabs, ticks)
    else:
      raise newException(Exception, "Continuous legend unsupported for scale kind " &
        $scale.scKind)

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

proc tickposlog(numTicks: int, minv, maxv: float,
                boundScale: ginger.Scale,
                hideTickLabels = false,
                format: proc(x: float): string): (seq[string], seq[float]) =
  ## Calculates the positions and labels of a log10 data scale given
  ## a min and max value. Takes into account a final bound scale outside
  ## of which no ticks may lie.
  let numTicks = numTicks * (log10(maxv) - log10(minv)).round.int
  var
    labs = newSeq[string]()
    labPos = newSeq[float]()
  for i in 0 ..< numTicks div 10:
    let base = (minv * pow(10, i.float))
    if not hideTickLabels:
      labs.add format(base)
    else:
      labs.add ""
    let minors = linspace(base, 9 * base, 9)
    labPos.add minors.mapIt(it.log10)
    if pow(10, boundScale.high) / pow(10, boundScale.low) > 10:
      labs.add toSeq(0 ..< 8).mapIt("")
    else:
      labs.add minors.mapIt(it.format)
  if not hideTickLabels: labs.add $maxv
  else: labs.add ""
  labPos.add log10(maxv)
  # for simplicity apply removal afterwards
  let filterIdx = toSeq(0 ..< labPos.len).filterIt(
    labPos[it] >= boundScale.low and
    labPos[it] <= boundScale.high
  )
  # apply filters to `labs` and `labPos`
  labs = filterIdx.mapIt(labs[it])
  labPos = filterIdx.mapIt(labPos[it])
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

proc handleContinuousTicks(view: Viewport, p: GgPlot, axKind: AxisKind,
                           scale: Scale, numTicks: int, theme: Theme,
                           isSecondary = false,
                           hideTickLabels = false,
                           margin = none[Coord1D]()): seq[GraphObject] =
  let boundScale = if axKind == akX: theme.xMarginRange else: theme.yMarginRange
  var rotate: Option[float]
  var alignTo: Option[TextAlignKind]
  case axKind
  of akX:
    rotate = theme.xTicksRotate
    alignTo = theme.xTicksTextAlign
  of akY:
    rotate = theme.yTicksRotate
    alignTo = theme.yTicksTextAlign

  case scale.scKind
  of scLinearData:
    let ticks = view.initTicks(axKind, numTicks, isSecondary = isSecondary,
                               boundScale = some(boundScale))
    var tickLabs: seq[GraphObject]
    tickLabs = view.tickLabels(ticks, isSecondary = isSecondary,
                               font = theme.tickLabelFont,
                               margin = margin,
                               rotate = rotate,
                               alignToOverride = alignTo,
                               format = scale.formatContinuousLabel)
    if not hideTickLabels:
      view.addObj concat(ticks, tickLabs)
    result = ticks
  of scTransformedData:
    # for now assume log10 scale
    let minVal = pow(10, scale.dataScale.low).smallestPow
    let maxVal = pow(10, scale.dataScale.high).largestPow
    let format = if scale.formatContinuousLabel != nil: scale.formatContinuousLabel
                 else: (proc(x: float): string = formatTickValue(x))
    let (labs, labelpos) = tickposlog(numTicks, minVal, maxVal, boundScale,
                                      hideTickLabels = hideTickLabels,
                                      format = format)
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

    let (tickObjs, labObjs) = view.tickLabels(tickLocs, labs, axKind, isSecondary = isSecondary,
                                              rotate = rotate,
                                              alignToOverride = alignTo,
                                              font = theme.tickLabelFont,
                                              margin = margin)
    if not hideTickLabels:
      view.addObj concat(tickObjs, labObjs)
    result = tickObjs
  else: discard

proc handleDiscreteTicks(view: Viewport, p: GgPlot, axKind: AxisKind,
                         labelSeq: seq[Value],
                         theme: Theme,
                         isSecondary = false,
                         hideTickLabels = false,
                         centerTicks = true,
                         margin = none[Coord1D](),
                         format: proc(x: Value): string): seq[GraphObject] =
  # create custom tick labels based on the possible labels
  # and assign tick locations based on ginger.Scale for
  # linear/trafo kinds and evenly spaced based on string?
  # start with even for all
  if isSecondary:
    raise newException(Exception, "Secondary axis for discrete axis not yet implemented!")
  let numTicks = labelSeq.len
  var tickLabels: seq[string]
  var tickLocs: seq[Coord1D]

  # TODO: check if we should use w/hImg here, distinguish the axes
  let discrMarginOpt = p.theme.discreteScaleMargin
  var discrMargin = 0.0
  if discrMarginOpt.isSome:
    case axKind
    of akX: discrMargin = discrMarginOpt.unsafeGet.toRelative(length = some(pointWidth(view))).val
    of akY: discrMargin = discrMarginOpt.unsafeGet.toRelative(length = some(pointHeight(view))).val
  # NOTE: the following only holds if def. of `wview` changed in ginger
  # doAssert view.wview != view.wimg
  let barViewWidth = (1.0 - 2 * discrMargin) / numTicks.float
  var centerPos = barViewWidth / 2.0
  if not centerTicks:
    case axKind
    of akX: centerPos = 0.0
    of akY: centerPos = barViewWidth
  for i in 0 ..< numTicks:
    if not hideTickLabels: tickLabels.add format(labelSeq[i])
    else: tickLabels.add ""
    # in case of a discrete scale we have categories, which are evenly spaced.
    # taking into account the margin of the plot, calculate center of all categories
    let pos = discrMargin + i.float * barViewWidth + centerPos
    let scale = (low: 0.0, high: 1.0)
    tickLocs.add Coord1D(pos: pos,
                         kind: ukRelative)
  var rotate: Option[float]
  var alignTo: Option[TextAlignKind]
  case axKind
  of akX:
    rotate = theme.xTicksRotate
    alignTo = theme.xTicksTextAlign
  of akY:
    rotate = theme.yTicksRotate
    alignTo = theme.yTicksTextAlign
  let (tickObjs, labObjs) = view.tickLabels(tickLocs, tickLabels, axKind, rotate = rotate,
                                            alignToOverride = alignTo,
                                            font = theme.tickLabelFont,
                                            margin = margin)
  if not hideTickLabels:
    view.addObj concat(tickObjs, labObjs)
  result = tickObjs

proc handleTicks(view: Viewport, filledScales: FilledScales, p: GgPlot,
                 axKind: AxisKind, theme: Theme,
                 hideTickLabels = false,
                 numTicksOpt = none[int](),
                 boundScaleOpt = none[ginger.Scale]()): seq[GraphObject] =
  ## This handles the creation of the tick positions and tick labels.
  ## It automatically updates the x and y scales of both the viewport and the `filledScales`!
  ## `margin` is the tick label margin in centimeter!
  var marginOpt: Option[Coord1D]
  var scale: Scale
  var numTicks: int
  case axKind
  of akX:
    scale = filledScales.getXScale()
    numTicks = if numTicksOpt.isSome: numTicksOpt.unsafeGet else: p.numXTicks
    if theme.xTickLabelMargin.isSome:
      marginOpt = some(view.c1(theme.xTickLabelMargin.unsafeGet, axKind, ukCentimeter))
  of akY:
    scale = filledScales.getYScale()
    numTicks = if numTicksOpt.isSome: numTicksOpt.unsafeGet else: p.numYTicks
    if theme.yTickLabelMargin.isSome:
      marginOpt = some(view.c1(theme.yTickLabelMargin.unsafeGet, axKind, ukCentimeter))
  when defined(defaultBackend):
    let hasScale = not scale.col.isNil
  else:
    let hasScale = scale.col.name.len > 0
  if hasScale:
    case scale.dcKind
    of dcDiscrete:
      let format =
        if scale.formatDiscreteLabel != nil: scale.formatDiscreteLabel
        else: (proc(x: Value): string = $x)
      result = view.handleDiscreteTicks(p, axKind, scale.labelSeq, theme = theme,
                                        hideTickLabels = hideTickLabels,
                                        margin = marginOpt,
                                        format = format)
      if hasSecondary(filledScales, axKind):
        let secAxis = filledScales.getSecondaryAxis(axKind)
        result.add view.handleDiscreteTicks(p, axKind, scale.labelSeq, theme = theme,
                                            isSecondary = true,
                                            hideTickLabels = hideTickLabels,
                                            margin = marginOpt,
                                            format = format)
    of dcContinuous:
      result = view.handleContinuousTicks(p, axKind, scale, numTicks, theme = theme,
                                          hideTickLabels = hideTickLabels,
                                          margin = marginOpt)
      if hasSecondary(filledScales, axKind):
        let secAxis = filledScales.getSecondaryAxis(axKind)
        result.add view.handleContinuousTicks(p, axKind, scale, numTicks, theme = theme,
                                              isSecondary = true,
                                              hideTickLabels = hideTickLabels,
                                              margin = marginOpt)
  else:
    # this should mean the main geom is histogram like?
    doAssert axKind == akY, "we can have akX without scale now?"
    # in this case don't read into anything and just call ticks / labels
    var boundScale: ginger.Scale
    if boundScaleOpt.isSome:
      boundScale = boundScaleOpt.unsafeGet
    else:
      boundScale = if axKind == akX: theme.xMarginRange else: theme.yMarginRange
    let ticks = view.initTicks(axKind, numTicks, boundScale = some(boundScale))
    var tickLabs: seq[GraphObject]
    tickLabs = view.tickLabels(ticks, font = theme.tickLabelFont,
                               margin = marginOpt)
    if not hideTickLabels:
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

proc handleLabels(view: Viewport, theme: Theme) =
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
      # TODO: use custom label font for margin calc?
      let font = if theme.labelFont.isSome: theme.labelFont.get else: font(8.0)
      case axKind
      of akX:
        marginVar = Coord1D(pos: 1.1, kind: ukStrHeight,
                            text: labNames[labLens], font: font)
      of akY:
        marginVar = Coord1D(pos: 1.0, kind: ukStrWidth,
                            text: labNames[labLens], font: font) +
                    Coord1D(pos: 0.3, kind: ukCentimeter)

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

proc getPlotBackground(theme: Theme): Style =
  ## returns a suitable style (or applies default) for the background of
  ## the plot area
  result = Style(color: color(0.0, 0.0, 0.0, 0.0))
  if theme.plotBackgroundColor.isSome:
    result.fillColor = theme.plotBackgroundColor.unsafeGet
  else:
    # default color: `grey92`
    result.fillColor = grey92

proc getCanvasBackground(theme: Theme): Style =
  ## returns a suitable style (or applies default) for the background color of
  ## the whole plot canvas. By default it is transparent
  result = Style(color: transparent)
  if theme.canvasColor.isSome:
    result.fillColor = theme.canvasColor.unsafeGet
  else:
    # default background: transparent
    result.fillColor = transparent

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
  var indHeights = toSeq(0 ..< numLabels).mapIt(quant(0.0, ukRelative))
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
      ## TODO: fix the hack using `1e-5`!
      let ytickView = viewLabel.handleTicks(filledScales, p, akY, theme = theme,
                                            numTicksOpt = some(5),
                                            boundScaleOpt = some(
                                              (low: yScale.low + 1e-5,
                                               high: yScale.high - 1e-5)))

  if not hideTicks:
    var xticks = view.handleTicks(filledScales, p, akX, theme = theme)
    let format =
      if yRidgeScale.formatDiscreteLabel != nil: yRidgeScale.formatDiscreteLabel
      else: (proc(x: Value): string = $x)
    # we create the ticks manually with `discreteTickLabels` to set the labels
    var yticks = view.handleDiscreteTicks(p, akY, yLabelSeq,
                                          theme = theme, centerTicks = false,
                                          format = format)
    let grdLines = view.initGridLines(some(xticks), some(yticks))
    view.addObj @[grdLines]
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
                  hideTicks = false) =
  # first write all plots into dummy viewport
  view.background(style = some(getPlotBackground(theme)))

  # change scales to user defined if desired
  view.xScale = if theme.xRange.isSome: theme.xRange.unsafeGet else: filledScales.xScale

  if p.ridges.isSome:
    let ridge = p.ridges.unsafeGet
    view.generateRidge(ridge, p, filledScales, theme, hideLabels, hideTicks)
  else:
    view.yScale = if theme.yRange.isSome: theme.yRange.unsafeGet else: filledScales.yScale
    for fg in filledScales.geoms:
      # for each geom, we create a child viewport of `view` covering
      # the whole viewport, which will house the data we just created.
      # Due to being a child, if will be drawn *after* its parent. This way things like
      # ticks will be below the data.
      # On the other hand this allows us to draw several geoms in on a plot and have the
      # order of the function calls `geom_*` be preserved
      var pChild = view.addViewport(name = "data")
      # DF here not needed anymore!
      pChild.createGobjFromGeom(fg, theme)
      # add the data viewport to the view
      view.children.add pChild

    var
      xticks: seq[GraphObject]
      yticks: seq[GraphObject]
    if not hideTicks:
      xticks = view.handleTicks(filledScales, p, akX, theme = theme)
      yticks = view.handleTicks(filledScales, p, akY, theme = theme)

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

    let grdLines = view.initGridLines(some(xticks), some(yticks))

    # given the just created plot and tick labels, have to check
    # whether we should enlarge the column / row for the y / x label and
    # move the label
    if not hideLabels:
      # TODO: why do we add labels to child 4 and not directly into the viewport we
      # use to provide space for it, i.e. 3?
      view.handleLabels(theme)
    view.addObj @[grdLines]

proc determineExistingCombinations(fs: FilledScales,
                                   facet: Facet): OrderedSet[Value] =
  # all possible combinations of the keys we group by
  let facets = fs.facets
  doAssert facets.len > 0
  var combinations: seq[seq[Value]]
  if facet.columns.len > 1:
    combinations = product(facets.mapIt(it.labelSeq))
  else:
    combinations = facets[0].labelSeq.mapIt(@[it])
  # create key / value pairs
  var combLabels: seq[Value]
  for c in combinations:
    var comb: seq[(string, Value)]
    for i, fc in facets:
      comb.add ($fc.col, c[i])
    combLabels.add toObject(comb)
  # combinations possibly contains non existing combinations too!
  result = initOrderedSet[Value]()
  # now check each geom for each `yieldData` element and see which
  # combination exists in them
  for fg in fs.geoms:
    for xk in keys(fg.yieldData):
      for cb in combLabels:
        if cb in xk:
          result.incl cb
  doAssert result.card <= combinations.len

proc calcFacetViewMap(combLabels: OrderedSet[Value]): Table[Value, int] =
  var idx = 0
  for cb in combLabels:
    result[cb] = idx
    inc idx

proc find(fg: FilledGeom, label: Value): DataFrame =
  ## returns the `DataFrame` of the given `fg` of that label, which
  ## contains `label` in `yieldData`
  for key, val in fg.yieldData:
    if label in key:
      # multiple keys may match, add DFs!
      result.add val[2]
  doAssert result.len > 0, "Invalid call to find, `label` not found in `yieldData`!"

proc calculateMarginRange(theme: Theme, scale: ginger.Scale, axKind: AxisKind): ginger.Scale
proc calcScalesForLabel(theme: var Theme, facet: Facet,
                        fg: FilledGeom, label: Value) =
  ## Given the `ScaleFreeKind` of the `facet` possibly calculate the
  ## real data range of the current label
  proc calcScale(df: DataFrame, col: string): ginger.Scale =
    let data = df[col].toTensor(Value)
    result = (low: min(data).toFloat(allowNull = true),
              high: max(data).toFloat(allowNull = true))
  if facet.sfKind in {sfFreeX, sfFreeY, sfFree}:
    # find the correct DF in the `yieldData` table for this label
    let labDf = fg.find(label)
    if facet.sfKind in {sfFreeX, sfFree}:
      let xScale = calcScale(labDf, fg.xcol)
      # only change the scale, if it's not high == low
      if xScale.low != xScale.high:
        theme.xMarginRange = calculateMarginRange(theme, xScale, akX)
      else:
        # base on filled geom's scale instead
        theme.xMarginRange = calculateMarginRange(theme, fg.xScale, akX)
    if facet.sfKind in {sfFreeY, sfFree}:
      let yScale = calcScale(labDf, fg.ycol)
      # only change the scale, if it's not high == low
      if yScale.low != yScale.high:
        theme.yMarginRange = calculateMarginRange(theme, yScale, akY)
      else:
        # base on filled geom's scale instead
        theme.yMarginRange = calculateMarginRange(theme, fg.yScale, akY)

proc buildTheme*(filledScales: FilledScales, p: GgPlot): Theme
proc generateFacetPlots(view: Viewport, p: GgPlot,
                        filledScales: FilledScales,
                        hideLabels = false,
                        hideTicks = false) =
  var p = p
  doAssert p.facet.isSome
  let facet = p.facet.unsafeGet
  # combine scales / plot theme for final theme

  let existComb = determineExistingCombinations(filledScales, facet)
  let numExist = existComb.card
  # set margin of plot to avoid tick labels getting too close
  # TODO: only set if user did not set xlim, ylim (theme x/yRange)?
  p.theme.xMargin = some(0.05)
  p.theme.yMargin = some(0.05)
  var theme = buildTheme(filledScales, p)
  # create a theme, which ignores points outside the scale (which happens
  # due to overlap!)
  theme.xTickLabelMargin = some(0.4)
  theme.yTickLabelMargin = some(-0.2)
  theme.xTicksRotate = p.theme.xTicksRotate
  theme.yTicksRotate = p.theme.yTicksRotate
  theme.xTicksTextAlign = p.theme.xTicksTextAlign
  theme.yTicksTextAlign = p.theme.yTicksTextAlign


  var pltSeq = newSeq[Viewport](numExist)
  # calculate number of rows and columns based on numGroups
  let (rows, cols) = calcRowsColumns(0, 0, numExist)
  let viewMap = calcFacetViewMap(existComb)
  if facet.sfKind in {sfFreeX, sfFreeY, sfFree}:
    view.layout(cols, rows, margin = quant(0.025, ukRelative))
  else:
    view.layout(cols, rows, margin = quant(0.005, ukRelative))

  var
    xticks: seq[GraphObject]
    yticks: seq[GraphObject]
  let lastCol = numExist mod cols
  for label, idx in pairs(viewMap):
    var viewLabel = view[idx]
    for fg in filledScales.geoms:
      # determine data scale of the current labels data if a scale is free.
      # This changes `x/yMarginRange` of the theme
      theme.calcScalesForLabel(facet, fg, label)
      # assign theme ranges to this views scale
      viewLabel.xScale = theme.xMarginRange
      viewLabel.yScale = theme.yMarginRange
      # create the layout for a facet + header
      viewLabel.layout(1, 2, rowHeights = @[quant(0.1, ukRelative), quant(0.9, ukRelative)],
                       margin = quant(0.01, ukRelative))
      var headerView = viewLabel[0]
      # set the background of the header
      headerView.background()
      # put in the text
      let text = $label #pair.mapIt($it[0] & ": " & $it[1]).join(", ")
      let headerText = headerView.initText(c(0.5, 0.5),
                                           text,
                                           textKind = goText,
                                           alignKind = taCenter,
                                           font = some(font(8.0)),
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
      # change number of ticks from default 10 if numbers too large (i.e. we
      # print between 100 and 9000); get's too crowded along x axis
      let xTickNum = if theme.xMarginRange.high > 100.0 and
                      theme.xMarginRange.high < 1e5:
                     5
                   else:
                     p.numXTicks

      xticks = plotView.handleTicks(filledScales, p, akX, theme = theme,
                                    numTicksOpt = some(xTickNum),
                                    hideTickLabels = hideXLabels)
      yticks = plotView.handleTicks(filledScales, p, akY, theme = theme,
                                    hideTickLabels = hideYLabels)

      let grdLines = plotView.initGridLines(some(xticks), some(yticks))
      plotView.addObj grdLines

      plotView.createGobjFromGeom(fg, theme, labelVal = some(label))
      viewLabel.xScale = plotView.xScale
      viewLabel.yScale = plotView.yScale

      if not filledScales.discreteX and filledScales.reversedX:
        viewLabel.xScale = (low: view.xScale.high, high: view.xScale.low)
      if not filledScales.discreteY and filledScales.reversedY:
        viewLabel.yScale = (low: view.yScale.high, high: view.yScale.low)

      # finally assign names
      plotView.name = "facetPlot"
      viewLabel.name = "facet_" & text

  if not hidelabels:
    # set the theme margins to defaults since `view` does not have any tick label texts
    # which can be used to determine the margin
    theme.xLabelMargin = some(1.0)
    theme.yLabelMargin = some(1.5)
    view.handleLabels(theme)

proc customPosition(t: Theme): bool =
  ## returns true if `legendPosition` is set and thus legend sits at custom pos
  result = t.legendPosition.isSome

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
      result = $xScale.col
  of akY:
    let yScale = getYScale(filledScales)
    when defined(defaultBackend):
      if yScale.name.len > 0:
        result = yScale.name
      elif not yScale.col.isNil:
        result = $yScale.col
      else:
        result = if filledScales.geoms.anyIt(it.geom.statKind == stBin and
                                             it.geom.density):
                   "density"
                 else:
                   "count"
    else:
      if yScale.name.len > 0:
        result = yScale.name
      elif yScale.col.name.len > 0:
        result = $yScale.col
      else:
        ## TODO: make this nicer by having a better approach to propagate
        ## the density information from geoms to here!
        result = if filledScales.geoms.anyIt(it.geom.statKind == stBin and
                                             it.geom.density):
                   "density"
                 else:
                   "count"

proc calculateMarginRange(theme: Theme, scale: ginger.Scale, axKind: AxisKind): ginger.Scale =
  var margin: float
  case axKind
  of akX: margin = if theme.xMargin.isSome: theme.xMargin.unsafeGet else: 0.0
  of akY: margin = if theme.yMargin.isSome: theme.yMargin.unsafeGet else: 0.0
  let diff = scale.high - scale.low
  result = (low: scale.low - diff * margin,
            high: scale.high + diff * margin)

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

  # calculate `xMarginRange`, `yMarginRange` if any
  let xScale = if result.xRange.isSome: result.xRange.unsafeGet else: filledScales.xScale
  result.xMarginRange = result.calculateMarginRange(xScale, akX)
  let yScale = if result.yRange.isSome: result.yRange.unsafeGet else: filledScales.yScale
  result.yMarginRange = result.calculateMarginRange(yScale, akY)

proc getLeftBottom(view: Viewport, annot: Annotation): tuple[left: float, bottom: float] =
  ## Given an annotation this proc returns the relative `(left, bottom)`
  ## coordinates of either the `(x, y)` values in data space converted
  ## using the `x, y: ginger.Scale` of the viewport or directly using
  ## the annotations `(left, bottom)` pair if available
  if annot.left.isSome:
    result.left = annot.left.unsafeGet
  else:
    # NOTE: we make sure in during `annotate` that either `left` or
    # `x` is defined!
    result.left = toRelative(Coord1D(pos: annot.x.unsafeGet,
                                     kind: ukData,
                                     axis: akX,
                                     scale: view.xScale)).pos
  if annot.bottom.isSome:
    result.bottom = annot.bottom.unsafeGet
  else:
    # NOTE: we make sure in during `annotate` that either `bottom` or
    # `y` is defined!
    result.bottom = toRelative(Coord1D(pos: annot.y.unsafeGet,
                                       kind: ukData,
                                       axis: akY,
                                       scale: view.yScale)).pos


proc drawAnnotations*(view: var Viewport, p: GgPlot) =
  ## draws all annotations from `p` onto the mutable view `view`.
  # this is 0.5 times the string height. Margin between text and
  # the background rectangle
  const AnnotRectMargin = 0.5
  for annot in p.annotations:
    # style to use for this annotation
    let rectStyle = Style(fillColor: annot.backgroundColor,
                          color: annot.backgroundColor)
    let (left, bottom) = view.getLeftBottom(annot)
    ## TODO: Fix ginger calculations / figure out if / why cairo text extents
    # are bad in width direction
    let marginH = toRelative(strHeight(AnnotRectMargin, annot.font),
                            length = some(pointHeight(view)))
    let marginW = toRelative(strHeight(AnnotRectMargin, annot.font),
                            length = some(pointWidth(view)))
    let totalHeight = quant(
      toRelative(getStrHeight(annot.text, annot.font),
                 length = some(view.hView)).val +
      marginH.pos * 2.0,
      unit = ukRelative)
    # find longest line of annotation to base background on
    let font = annot.font # refs https://github.com/nim-lang/Nim/pull/14447
    let maxLine = annot.text.strip.splitLines.sortedByIt(
      getStrWidth(it, font).val
    )[^1]
    let maxWidth = getStrWidth(maxLine, annot.font)
    # calculate required width for background rectangle. string width +
    # 2 * margin
    let rectWidth = quant(
      toRelative(maxWidth, length = some(pointWidth(view))).val +
      marginW.pos * 2.0,
      unit = ukRelative
    )
    # left and bottom positions, shifted each by one margin
    let rectX = left - marginW.pos
    let rectY = bottom - totalHeight.toRelative(
      length = some(view.hView)
    ).val + marginH.pos
    # create background rectangle
    let annotRect = view.initRect(
      Coord(x: Coord1D(pos: rectX, kind: ukRelative),
            y: Coord1D(pos: rectY, kind: ukRelative)),
      rectWidth,
      totalHeight,
      style = some(rectStyle),
      name = "annotationBackground")
    # create actual annotation
    let annotText = view.initMultiLineText(
      origin = c(left, bottom),
      text = annot.text,
      textKind = goText,
      alignKind = taLeft,
      fontOpt = some(annot.font))
    view.addObj concat(@[annotRect], annotText)

proc drawTitle(view: Viewport, title: string, theme: Theme, width: Quantity) =
  ## Draws a title onto the `view` (which should be the header of the plot).
  ## If the length of the title exceeds the `width` (should be the width of
  ## the header viewport + right side margin), we automatically wrap the
  ## title to multiple lines.
  var title = title
  let font = if theme.titleFont.isSome: theme.titleFont.get else: font(16.0)
  if "\n" notin title:
    # user does not do manual wrapping. Check if needs to be wrapped.
    let strWidth = getStrWidth(title, font)
    if strWidth > width:
      # rebuild and wrap
      var line: string
      var mTitle: string
      for word in title.split(' '):
        let lineWidth = getStrWidth(line & word, font)
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

  let titleObj = view.initMultiLineText(c(0.0, 0.9),
                                     title,
                                     textKind = goText,
                                     alignKind = taLeft,
                                     fontOpt = some(font))
  view.addObj titleObj

func updateAesRidges(p: GgPlot): GgPlot =
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

proc ggcreate*(p: GgPlot, width = 640.0, height = 480.0): PlotView =
  ## applies all calculations to the `GgPlot` object required to draw
  ## the plot with cairo and returns a `PlotView`. The `PlotView` contains
  ## the final `Scales` built from the `GgPlot` object and all its geoms
  ## plus the ginal ginger.Viewport which only has to be drawn to produce the
  ## plot.
  ## This proc is useful to investigate the final Scales or the Viewport
  ## that will actually be drawn.
  var filledScales: FilledScales
  if p.ridges.isSome:
    # update all aesthetics to include the `yRidges` scale
    filledScales = collectScales(updateAesRidges(p))
  else:
    filledScales = collectScales(p)
  let theme = buildTheme(filledScales, p)
  let hideTicks = if theme.hideTicks.isSome: theme.hideTicks.unsafeGet
                   else: false
  let hideTickLabels = if theme.hideTickLabels.isSome: theme.hideTickLabels.unsafeGet
                       else: false
  let hideLabels = if theme.hideLabels.isSome: theme.hideLabels.unsafeGet
                   else: false

  # create the plot
  var img = initViewport(name = "root",
                         wImg = width,
                         hImg = height)

  # set color of canvas background
  img.background(style = some(getCanvasBackground(theme)))

  img.createLayout(filledScales, theme)
  # get viewport of plot
  var pltBase = img[4]

  if p.facet.isSome:
    pltBase.generateFacetPlots(p,
                               filledScales,
                               hideLabels = hideLabels,
                               hideTicks = hideTicks)
  else:
    pltBase.generatePlot(p, filledScales, theme,
                         hideLabels = hideLabels,
                         hideTicks = hideTicks)
  let xScale = pltBase.xScale
  let yScale = pltBase.yScale
  img.xScale = xScale
  img.yScale = yScale
  #img.updateDataScale()

  # possibly correct the yScale assigned to the root Viewport
  img.yScale = pltBase.yScale

  # draw legends
  # store each type of drawn legend. only one type for each kind
  var drawnLegends = initHashSet[(DiscreteKind, ScaleKind)]()
  ## TODO: consider if this is such a stable thing to do. Useful for now.
  var scaleNames = initHashSet[string]()
  var legends: seq[Viewport]
  for scale in enumerateScalesByIds(filledScales):
    if scale.scKind notin {scLinearData, scTransformedData} and
       (scale.dcKind, scale.scKind) notin drawnLegends:
      # create deep copy of the original legend pane
      var lg: Viewport
      when defined(gcDestructors):
        lg[] = img[5][]
      else:
        lg = deepCopy(img[5])
      lg.createLegend(scale, theme.legendOrder)
      let scaleCol = evaluate(scale.col).toStr
      if scaleCol notin scaleNames:
        legends.add lg
        drawnLegends.incl (scale.dcKind, scale.scKind)
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
  # make sure the target directory exists, create if not
  createDir(fname.splitFile().dir)
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

from json import `%`
from os import splitFile

proc `%`*(t: tuple): json.JsonNode =
  result = json.newJObject()
  for k, v in t.fieldPairs:
    json.`[]=`(result, k, %v)

proc ggjson*(fname: string, width = 640.0, height = 480.0): JsonDummyDraw =
  let (_, _, fext) = fname.splitFile
  JsonDummyDraw(fname: fname.replace(fext, ".json"),
                width: some(width),
                height: some(height))

proc `%`(fn: proc(): seq[uint32] {.closure.}): json.JsonNode =
  result = % fn()

proc `+`*(p: GgPlot, jsDraw: JsonDummyDraw) =
  ## generate a JSON file from the given filename by replacing the file
  ## extension by `.json` and converting the `Viewport` to JSON after
  ## calling `ggcreate`. Used for CI.
  doAssert jsDraw.width.isSome and jsDraw.height.isSome
  let plt = p.ggcreate(width = jsDraw.width.get,
                       height = jsDraw.height.get)
  writeFile(jsDraw.fname, json.`$`(% plt.view))

proc `+`*(p: GgPlot, d: VegaDraw): json.JsonNode =
  p.toVegaLite()

proc countLines(s: var FileStream): int =
  ## quickly counts the number of lines and then resets stream to beginning
  ## of file
  var buf = newString(500)
  while s.readLine(buf):
    inc result
  s.setPosition(0)

proc checkHeader(s: Stream, fname, header: string, colNames: seq[string]): bool =
  ## checks whether the given file contains the header `header`
  result = true
  if header.len > 0:
    var headerBuf: string
    if s.peekLine(headerBuf):
      result = headerBuf.startsWith(header)
    else:
      raise newException(IOError, "The input file " & $fname & " seems to be empty.")
  elif colNames.len > 0:
    # given some column names and a "header" without a symbol means we assume
    # there is no real header. If there is a real header in addition, user has
    # to use `skipLines = N` to skip it.
    result = false

proc readCsv*(s: Stream,
              sep = ',',
              header = "",
              skipLines = 0,
              colNames: seq[string] = @[],
              fname = "<unknown>"): OrderedTable[string, seq[string]] =
  ## returns a `Stream` with CSV like data as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  ## The `header` field can be used to designate the symbol used to
  ## differentiate the `header`. By default `#`.
  ## `colNames` can be used to provide custom names for the columns.
  ## If any are given and a header is present with a character indiciating
  ## the header, it is automatically skipped. ``However``, if custom names are
  ## desired and there is a real header without any starting symbol (i.e.
  ## `header.len == 0`), please use `skipLines = N` to skip it manually!
  # first check if the file even has a header of type `header`
  let hasHeader = checkHeader(s, fname, header, colNames)

  var parser: CsvParser
  open(parser, s, fname, separator = sep, skipInitialSpace = true)

  if colNames.len > 0:
    # if `colNames` available, use as header
    parser.headers = colNames
    if hasHeader:
      # and skip the real header
      discard parser.readRow()
  elif hasHeader:
    # read the header and use it
    parser.readHeaderRow()
  else:
    # file has no header nor user gave column names, raise
    raise newException(IOError, "Input neither has header starting with " &
      $header & " nor were column names provided!")

  result = initOrderedTable[string, seq[string]]()
  # filter out the header, delimiter, if any
  parser.headers.keepItIf(it != header)

  # possibly strip the headers and create the result table of columns
  var colHeaders: seq[string]
  for colUnstripped in items(parser.headers):
    let col = colUnstripped.strip
    colHeaders.add col
    result[col] = newSeqOfCap[string](5000) # start with a reasonable default cap

  # parse the actual file using the headers
  var lnCount = 0
  while readRow(parser):
    if lnCount < skipLines:
      inc lnCount
      continue
    for i, col in parser.headers:
      parser.rowEntry(col).removePrefix({' '})
      parser.rowEntry(col).removeSuffix({' '})
      result[colHeaders[i]].add parser.rowEntry(col)
  parser.close()

proc readCsv*(fname: string,
              sep = ',',
              header = "",
              skipLines = 0,
              colNames: seq[string] = @[]): OrderedTable[string, seq[string]] =
  ## returns a CSV file as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  ## The `header` field can be used to designate the symbol used to
  ## differentiate the `header`. By default `#`.
  ## `colNames` can be used to provide custom names for the columns.
  ## If any are given and a header is present with a character indiciating
  ## the header, it is automatically skipped. ``However``, if custom names are
  ## desired and there is a real header without any starting symbol (i.e.
  ## `header.len == 0`), please use `skipLines = N` to skip it manually!
  var s = newFileStream(fname, fmRead)
  if s == nil:
    raise newException(IOError, "Input file " & $fname & " does not exist! " &
     "`readCsv` failed.")
  result = s.readCsv(sep, header, skipLines, colNames, fname = fname)
  s.close()

proc writeCsv*(df: DataFrame, filename: string, sep = ',', header = "",
               precision = 4) =
  ## writes a DataFrame to a "CSV" (separator can be changed) file.
  ## `sep` is the actual separator to be used. `header` indicates a potential
  ## symbol marking the header line, e.g. `#`
  var data = newStringOfCap(df.len * 8) # for some reserved space
  # add header symbol to first line
  data.add header
  let keys = getKeys(df)
  data.add join(keys, $sep) & "\n"
  var idx = 0
  for row in df:
    idx = 0
    for x in row:
      if idx > 0:
        data.add $sep
      data.add pretty(x, precision = precision)
      inc idx
    data.add "\n"
  writeFile(filename, data)
