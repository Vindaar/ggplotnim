import options, tables, hashes, macros, strformat
import chroma
import formula
import ginger

type
  Aesthetics* = object
    # In principle `x`, `y` are `Scale(scKind: scLinearData)`!
    # possibly `scTranformedData`.
    x*: Option[Scale]
    y*: Option[Scale]
    # Replace these by e.g. `color: Option[Scale]` and have `Scale` be variant
    # type that stores its kind `kind: scColor` and `key: string`.
    fill*: Option[Scale] # classify by fill color
    color*: Option[Scale] # classify by color
    size*: Option[Scale] # classify by size
    shape*: Option[Scale] # classify by shape

  ScaleKind* = enum
    scLinearData, scTransformedData, scColor, scFillColor, scShape, scSize

  PositionKind* = enum
    pkIdentity = "identity"
    pkStack = "stack"
    pkDodge = "dodge"
    pkFill = "fill"

  StatKind* = enum
    stIdentity = "identity"
    stCount = "count"
    stBin = "bin"
    # and more to be added...

  DiscreteKind* = enum
    dcDiscrete, dcContinuous

  ScaleValue* = object
    case kind*: ScaleKind
    of scLinearData, scTransformedData:
      # just stores a data value
      val*: Value
    # TODO: overhaul this
    #of scTransformedData:
      # data under some transformation. E.g. log, tanh, ...
      #rawVal*: Value
    of scFillColor, scColor:
      # stores a color
      color*: Color
    of scShape:
      # a marker kind
      marker*: MarkerKind
    of scSize:
      # a size of something, e.g. a marker
      size*: float

  #BelongsToAxis = enum
  #  btTrue, # for Scales, which belong to an axis, e.g. continuous x, y
  #  btFalse # for Scales that do not directly belong to an axis, e.g. color Scale

  SecondaryAxis* = object
    trans*: Option[FormulaNode] # possible transformation of the first axis to arrive at second
    name*: string
    axKind*: AxisKind

  # TODO: should not one scale belong to only one axis?
  # But if we do that, how do we find the correct scale in the seq[Scale]?
  # Replace seq[Scale] by e.g. Table[string, Scale] where string is some
  # static identifier we can calculate to retrieve it?
  # e.g. `xaxis`, `<name of geom>.xaxis` etc.?
  ScaleTransform* = proc(v: Value): Value

  Scale* = object
    # the column which this scale corresponds to
    col*: string
    name*: string
    ids*: set[uint16]
    vKind*: ValueKind # the value kind of the data of `col`
    case scKind*: ScaleKind
    of scLinearData, scTransformedData:
      # which axis does it belong to?
      axKind*: AxisKind
      # where `trans` is our assigned transformation function, which is applied
      # to all values associated with this scale
      # For scLinearData the transformation proc is just the identity (or
      # not defined) and will never be called
      trans*: ScaleTransform
      secondaryAxis*: Option[SecondaryAxis] # a possible secondary x, y axis
    else: discard
    case dcKind*: DiscreteKind
    of dcDiscrete:
      # For discrete data this is a good solution. How about continuous data?
      valueMap*: OrderedTable[Value, ScaleValue]
      # seq of labels to access via index
      labelSeq*: seq[Value]
    of dcContinuous:
      # For continuous we might want to add a `Scale` in the ginger sense
      dataScale*: ginger.Scale
      # and the closure to read the correct data, which takes care of mapping
      # the data to the correct `ScaleKind`. This is `nil` for `scLinearData` and
      # `scTransformedData`, but contains the correct style calculations for the
      # other `ScaleKinds`
      mapData*: proc(idxs: seq[int] = @[]): seq[ScaleValue]

  Facet* = object
    columns*: seq[string]

  # helper object to compose `ggsave` via `+` with `ggplot`
  # Uses the default ``cairo`` backend
  Draw* = object
    fname*: string
    width*: Option[float]
    height*: Option[float]

  # helper object to compose `ggvega` via `+` with `ggplot`
  # Used to show a plot using the Vega-Lite backend
  VegaDraw* = object
    discard

  # bin position kind stores the different ways bins can be represented
  # Either as left bin edges, center positions or right edges
  BinPositionKind* = enum
    bpNone = "none" # <- means "leave data untouched", default for geom_point etc.
    bpCenter = "center"
    bpLeft = "left"
    bpRight = "right"

  GeomKind* = enum
    gkPoint, gkBar, gkHistogram, gkFreqPoly, gkTile, gkLine
  Geom* = object
    gid*: uint16 # unique id of the geom
    data*: Option[DataFrame] # optionally a geom may have its own data frame
    style*: Option[Style] # if set, apply this style instead of parent's
    position*: PositionKind
    aes*: Aesthetics # a geom can have its own aesthetics. Needs to be part of
                    # the `Geom`, because if we add it to `GgPlot` we lose track
                    # of which geom it corresponds to
    binPosition*: BinPositionKind
    kind*: GeomKind
    case statKind*: StatKind
    of stBin:
      numBins*: int # number of bins
      binWidth*: Option[float] # width of bins in terms of the data, overrides `numBins`
      # bin edges given in data values. Overrides `numBins` and `binWidth`
      binEdges*: Option[seq[float]]
    else:
      discard

  # what might become a theming object
  Theme* = object
    baseFontSize*: Option[float] # base size for fonts NOT IMPL'd
    fontSizeScale*: Option[float] # scales all fonts by given value NOT IMPL'd
    labelFont*: Option[Font]
    titleFont*: Option[Font]
    subTitleFont*: Option[Font]
    tickLabelFont*: Option[Font]
    title*: Option[string]
    subTitle*: Option[string]
    xlabel*: Option[string]
    xlabelMargin*: Option[float]
    xTicksRotate*: Option[float]
    xTicksTextAlign*: Option[TextAlignKind]
    ylabel*: Option[string]
    ylabelMargin*: Option[float]
    yTicksRotate*: Option[float]
    yTicksTextAlign*: Option[TextAlignKind]
    xLabelSecondary*: Option[string]
    yLabelSecondary*: Option[string]
    legendPosition*: Option[Coord]
    discreteScaleMargin*: Option[Quantity] # margin applied to scale of discrete kindn default 0.2 `cm`

  GgPlot*[T] = object
    data*: T
    title*: string
    subtitle*: string
    # GgPlot can only contain a single `aes` by itself. Geoms may contain
    # seperate ones
    aes*: Aesthetics # original `aes` given in `ggplot()` call. Won't be modified
    numXticks*: int
    numYticks*: int
    facet*: Option[Facet]
    geoms*: seq[Geom]
    theme*: Theme

  # A filled geom is a geom plus a closure to yield the
  FilledGeom* = object
    geom*: Geom
    xcol*: string
    ycol*: string
    # each geom will fill its own data scales. The filledScale will then
    # select the correct scale for the whole plot
    xScale*: ginger.Scale
    yScale*: ginger.Scale
    # `Style` stores base style for each value of the discrete (!) scales
    yieldData*: OrderedTable[Style, (seq[Style], DataFrame)]
    # whether X or Y is discrete or continuous. Has direct implication for drawing
    dcKindX*: DiscreteKind
    dcKindY*: DiscreteKind
    # the number of max elements for each dimensions found for this geom
    # if x or y is discrete the following will store the number of classes
    # if continuous just the max number of elements the largest style
    numX*: int
    numY*: int

  MainAddScales* = tuple[main: Option[Scale], more: seq[Scale]]
  FilledScales* = object
    xScale*: ginger.Scale
    yScale*: ginger.Scale
    geoms*: seq[FilledGeom]
    x*: MainAddScales
    y*: MainAddScales
    color*: MainAddScales
    fill*: MainAddScales
    size*: MainAddScales
    shape*: MainAddScales

  # `PlotView` describes the object the final representation of a `GgPlot` before
  # being drawn.
  PlotView* = object
    # `filledScales` is essentially the final aesthetics used in the drawing of
    # the plot, based on `GgPlot.aes` and `geoms.aes` combined and includes additional
    # fields to avoid dependency on GgPlot object to draw
    filledScales*: FilledScales
    view*: Viewport # the ginger representation of the plot


proc `==`*(s1, s2: Scale): bool =
  if s1.dcKind == s2.dcKind and
     s1.col == s2.col:
    # the other fields ``will`` be computed to the same!
    result = true
  else:
    result = false

# Workaround. For some reason `hash` for `Style` isn't found if defined in
# ginger..
proc hash*(s: Style): Hash =
  let c = s.color
  result = hash(c.r)
  result = result !& hash(c.g)
  result = result !& hash(c.b)
  result = result !& hash(s.size)
  result = result !& hash(s.lineType)
  result = result !& hash(s.lineWidth)
  let fc = s.fillColor
  result = result !& hash(fc.r)
  result = result !& hash(fc.g)
  result = result !& hash(fc.b)
  result = result !& hash(s.marker)
  result = !$result

proc hash*(x: ScaleValue): Hash =
  result = hash(x.kind.int)
  case x.kind:
  of scLinearData, scTransformedData:
    result = result !& hash(x.val)
  #of scTransformedData:
    #result = result !& hash(x.rawVal)
    # TODO: Hash proc?
  of scColor:
    let c = x.color
    result = result !& hash(c.r)
    result = result !& hash(c.g)
    result = result !& hash(c.b)
  of scFillColor:
    let fc = x.color
    # for fill we take negative values
    result = result !& hash(-fc.r)
    result = result !& hash(-fc.g)
    result = result !& hash(-fc.b)
  of scShape:
    result = result !& hash(x.marker)
  of scSize:
    result = result !& hash(x.size)
  result = !$result

proc hash*(x: Scale): Hash =
  result = hash(x.scKind.int)
  result = result !& hash(x.col)
  result = result !& hash(x.ids)
  case x.dcKind:
  of dcDiscrete:
    for k, v in x.valueMap:
      result = result !& hash(k)
      result = result !& hash(v)
    result = result !& hash(x.labelSeq)
  of dcContinuous:
    result = result !& hash(x.dataScale)
  result = !$result

proc `$`*(f: Facet): string =
  result = "(columns: "
  for i, x in f.columns:
    if i == f.columns.len - 1:
      result.add x & ")"
    else:
      result.add x & ", "

proc `$`*(aes: Aesthetics): string =
  result = "("
  if aes.x.isSome:
    result.add "x: " & $aes.x.unsafeGet
  if aes.y.isSome:
    result.add "y: " & $aes.y.unsafeGet
  if aes.size.isSome:
    result.add "size: " & $aes.size.unsafeGet
  if aes.shape.isSome:
    result.add "shape: " & $aes.shape.unsafeGet
  if aes.color.isSome:
    result.add "color: " & $aes.color.unsafeGet
  if aes.fill.isSome:
    result.add "fill: " & $aes.fill.unsafeGet
  result.add ")"

proc `$`*(g: Geom): string =
  result = "Geom("
  result.add &"kind: {g.kind}, "
  result.add &"gid: {g.gid}, "
  result.add &"data.isSome?: {g.data.isSome}, "
  if g.style.isSome:
    result.add &"style: {g.style.get}, "
  result.add &"aes: {g.aes}, "
  result.add &"position: {g.position}, "
  result.add &"binPosition: {g.binPosition}, "
  result.add &"statKind: {g.statKind}, "
  case g.statKind
  of stBin:
    result.add "("
    result.add &"numBins: {g.numBins}, "
    result.add &"binWidth: {g.binWidth}, "
    result.add &"binEdges: {g.binEdges}, "
    result.add ")"
  else: discard
  result.add ")"

macro typeName(x: typed): untyped =
  let str = x.getTypeInst.repr
  result = quote do:
    `str`

proc `$`*[T](p: GgPlot[T]): string =
  result = "(data: " & typeName(p.data)
  result.add ", title: " & $p.title
  result.add ", subtitle: " & $p.subtitle
  result.add ", aes: " & $p.aes
  result.add ", numXTicks " & $p.numXTicks
  result.add ", numYTicks " & $p.numXTicks
  result.add ", facet: " & $p.facet
  result.add ", geoms: "
  for g in p.geoms:
    result.add $g
  result.add ")"

proc `$`*(s: Scale): string =
  result = &"Scale(col: {s.col}"
  result.add &", name: {s.name}"
  if s.ids == {0'u16 .. high(uint16)}:
    result.add ", ids: {0..65535'u16}"
  else:
    result.add &", ids: {s.ids}"
  result.add &", vKind: {s.vKind}"
  result.add &", scKind: {s.scKind}"
  case s.scKind
  of scLinearData, scTransformedData:
    result.add &", axKind: {s.axKind}"
    result.add &", trans.isNil?: {s.trans.isNil}"
    result.add &", secondaryAxis: {s.secondaryAxis}"
  else: discard
  result.add &", dcKind: {s.dcKind}"
  case s.dcKind
  of dcDiscrete:
    result.add &", valueMap: {s.valueMap}"
    result.add &", labelSeq: {s.labelSeq}"
  of dcContinuous:
    result.add &", dataScale: {s.dataScale}"
    result.add &", mapData.isNil?: {s.mapData.isNil}"
  result.add ")"
