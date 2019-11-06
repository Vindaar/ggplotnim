import options, tables, hashes, macros
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
  Scale* = object
    # the column which this scale corresponds to
    col*: string
    name*: string
    vKind*: ValueKind # the value kind of the data of `col`
    case scKind*: ScaleKind
    of scLinearData, scTransformedData:
      # which axis does it belong to?
      axKind*: AxisKind
      # where `trans` is our assigned transformation function, which is applied
      # to all values associated with this scale
      # For scLinearData the transformation proc is just the identity (or
      # not defined) and will never be called
      trans*: proc(v: Value): Value
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
      # the data to the correct `ScaleKind`. For linear data it's just the data
      # itself. For `transformedData` it applies the transformation. For color etc.
      # the
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

  GeomKind* = enum
    gkPoint, gkBar, gkHistogram, gkFreqPoly, gkTile, gkLine
  Geom* = object
    data*: Option[DataFrame] # optionally a geom may have its own data frame
    style*: Option[Style] # if set, apply this style instead of parent's
    position*: PositionKind
    aes*: Aesthetics # a geom can have its own aesthetics. Needs to be part of
                    # the `Geom`, because if we add it to `GgPlot` we lose track
                    # of which geom it corresponds to
    statKind*: StatKind
    case kind*: GeomKind
    of gkHistogram, gkFreqPoly:
      numBins*: int # number of bins
      binWidth*: float # width of bins in terms of the data
    else:
      discard

  # what might become a theming object
  Theme* = object
    xlabel*: Option[string]
    xlabelMargin*: Option[float]
    ylabel*: Option[string]
    ylabelMargin*: Option[float]
    legendPosition*: Option[Coord]
    discreteScaleMargin*: Option[Quantity] # margin applied to scale of discrete kindn default 0.2 `cm`

  GgPlot*[T] = object
    data*: T
    title*: string
    subtitle*: string
    # GgPlot can only contain a single `aes` by itself. Geoms may contain
    # seperate ones
    aes*: Aesthetics
    numXticks*: int
    numYticks*: int
    facet*: Option[Facet]
    geoms*: seq[Geom]
    theme*: Theme

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
  result = hash($s.color)
  result = result !& hash(s.size)
  result = result !& hash(s.lineType)
  result = result !& hash(s.lineWidth)
  result = result !& hash($s.fillColor)
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
    result = result !& hash($x.color)
  of scFillColor:
    result = result !& hash($x.color & "FILL")
  of scShape:
    result = result !& hash(x.marker)
  of scSize:
    result = result !& hash(x.size)
  result = !$result

proc hash*(x: Scale): Hash =
  result = hash(x.scKind.int)
  result = result !& hash(x.col)
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
  result = "(kind: " & $g.kind & ","
  result.add "aes: " & $g.aes
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
