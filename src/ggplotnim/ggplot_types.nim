import options, tables, hashes, macros, strformat, times
import chroma
import datamancer
import ginger except Scale

type
  ScaleTransform* = proc(v: float): float

## two constants for count columns and previous values
const CountCol* = "counts_GGPLOTNIM_INTERNAL"
const PrevValsCol* = "prevVals_GGPLOTNIM_INTERNAL"
const SmoothValsCol* = "smoothVals_GGPLOTNIM_INTERNAL"

# something like
# aes: array[AesKind, Option[Scale]]
type
  # raised if a mix of aesthetics is invalid (typically some information missing)
  AestheticError* = object of CatchableError

  Aesthetics* = object
    # In principle `x`, `y` are `Scale(scKind: scLinearData)`!
    # possibly `scTranformedData`.
    x*: Option[Scale]
    xMin*: Option[Scale] # min value for `gkErrorBar`
    xMax*: Option[Scale] # max value for `gkErrorBar`
    y*: Option[Scale]
    yMin*: Option[Scale] # min value for `gkErrorBar`
    yMax*: Option[Scale] # max value for `gkErrorBar`
    # Replace these by e.g. `color: Option[Scale]` and have `Scale` be variant
    # type that stores its kind `kind: scColor` and `key: string`.
    fill*: Option[Scale] # classify by fill color
    color*: Option[Scale] # classify by color
    size*: Option[Scale] # classify by size
    shape*: Option[Scale] # classify by shape
    width*: Option[Scale] # width of tile / rect / raster
    height*: Option[Scale] # height of tile / rect / raster
    text*: Option[Scale] # text to display
    yRidges*: Option[Scale]
    weight*: Option[Scale]

  ScaleKind* = enum
    scLinearData, scTransformedData, scColor, scFillColor, scShape, scSize, scText

  PositionKind* = enum
    pkIdentity = "identity"
    pkStack = "stack"
    pkDodge = "dodge"
    pkFill = "fill"

  StatKind* = enum
    stIdentity = "identity"
    stCount = "count"
    stBin = "bin"
    stSmooth = "smooth"
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
      lineType*: LineType
    of scSize:
      # a size of something, e.g. a marker
      size*: float
    of scText: discard

  #BelongsToAxis = enum
  #  btTrue, # for Scales, which belong to an axis, e.g. continuous x, y
  #  btFalse # for Scales that do not directly belong to an axis, e.g. color Scale

  SecondaryAxis* = object
    name*: string
    axKind*: AxisKind
    case scKind*: ScaleKind
    of scLinearData:
      trans*: Option[FormulaNode] # possible transformation of the first axis to arrive at second
    of scTransformedData:
      transFn*: ScaleTransform
      invTransFn*: ScaleTransform
    else: discard

  DiscreteFormat* = proc(x: Value): string
  ContinuousFormat* = proc(x: float): string

  DateScale* = object
    name*: string
    axKind*: AxisKind  ## which axis does it belong to?
    isTimestamp*: bool ## is it a timestamp?
    parseDate*: proc(s: string): DateTime ## possible parser for string columns
    #formatDate*: proc(dt: DateTime): string # formatter for labels, required
    formatString*: string ## the string to format dates with
    dateSpacing*: Duration ## required duration between two ticks

  # TODO: should not one scale belong to only one axis?
  # But if we do that, how do we find the correct scale in the seq[Scale]?
  # Replace seq[Scale] by e.g. Table[string, Scale] where string is some
  # static identifier we can calculate to retrieve it?
  # e.g. `xaxis`, `<name of geom>.xaxis` etc.?
  Scale* = ref object
    # the column which this scale corresponds to
    col*: FormulaNode
    name*: string
    ids*: set[uint16]
    vKind*: ValueKind # the value kind of the data of `col`
    hasDiscreteness*: bool # TODO: set default dcKind, make option whatever instead of this
    case scKind*: ScaleKind
    of scLinearData, scTransformedData:
      # which axis does it belong to?
      axKind*: AxisKind
      reversed*: bool # decides whether the scale will be reversed
      # where `trans` is our assigned transformation function, which is applied
      # to all values associated with this scale
      # For scLinearData the transformation proc is just the identity (or
      # not defined) and will never be called
      trans*: ScaleTransform
      invTrans*: ScaleTransform
      secondaryAxis*: Option[SecondaryAxis] # a possible secondary x, y axis
      dateScale*: Option[DateScale] # an optional date scale associated to this axis
    else: discard
    case dcKind*: DiscreteKind
    of dcDiscrete:
      # For discrete data this is a good solution. How about continuous data?
      valueMap*: OrderedTable[Value, ScaleValue]
      # seq of labels to access via index
      labelSeq*: seq[Value]
      formatDiscreteLabel*: DiscreteFormat
    of dcContinuous:
      # For continuous we might want to add a `Scale` in the ginger sense
      dataScale*: ginger.Scale
      # and the closure to read the correct data, which takes care of mapping
      # the data to the correct `ScaleKind`. This is `nil` for `scLinearData` and
      # `scTransformedData`, but contains the correct style calculations for the
      # other `ScaleKinds`
      mapData*: proc(df: DataFrame): seq[ScaleValue]
      formatContinuousLabel*: ContinuousFormat

  ## enum to determine which scales in a facet plot are free
  ScaleFreeKind* = enum
    sfFixed = "fixed" # all fixed to same scale
    sfFreeX = "free_x" # x is free
    sfFreeY = "free_y" # y is free
    sfFree = "free" # each subplot uses its own scale

  Facet* = object
    columns*: seq[string]
    sfKind*: ScaleFreeKind

  Ridges* = object
    col*: FormulaNode
    overlap*: float # overlap between ridges
    showTicks*: bool
    # allows to define a custom order for the labels of the
    # ridge scale
    labelOrder*: Table[Value, int]

  # helper object to compose `ggsave` via `+` with `ggplot`
  # Uses the default ``cairo`` backend
  Draw* = object
    fname*: string
    width*: Option[float]
    height*: Option[float]
    texOptions*: TeXOptions

  ## helper to generate a json file from a ggplot call by creating a `JsonNode`
  ## of the final plot `Viewport`. This is mainly used for the CI.
  JsonDummyDraw* = object
    fname*: string
    width*: Option[float]
    height*: Option[float]

  # helper object to compose `ggvega` via `+` with `ggplot`
  # Used to show a plot using the Vega-Lite backend
  VegaDraw* = object
    fname*: string
    width*: Option[float]
    height*: Option[float]
    asPrettyJson*: bool

  # bin position kind stores the different ways bins can be represented
  # Either as left bin edges, center positions or right edges
  BinPositionKind* = enum
    bpNone = "none" # <- means "leave data untouched", default for geom_point etc.
    bpCenter = "center"
    bpLeft = "left"
    bpRight = "right"

  ## `GgStyle` object is an `Option` wrapper around `ginger.Style`.
  ## This allows us to differentiate between user settings of styles
  ## and mappings. Settings take higher priority!
  GgStyle* = object
    color*: Option[Color]
    size*: Option[float]
    lineType*: Option[LineType]
    lineWidth*: Option[float]
    fillColor*: Option[Color]
    marker*: Option[MarkerKind]
    errorBarKind*: Option[ErrorBarKind]
    # the alpha to use for `fillColor` (line colors and point colors
    # are unaffected by this and have to be set manually via the `color: Color`)
    alpha*: Option[float]
    font*: Option[Font]

  BinByKind* = enum
    bbFull = "full"
    bbSubset = "subset"

  GeomKind* = enum
    gkPoint, gkBar, gkHistogram, gkFreqPoly, gkTile, gkLine, gkErrorBar, gkText,
    gkRaster

  HistogramDrawingStyle* = enum
    hdBars = "bars" ## draws historams by drawing individual bars right next to
                    ## one another
    hdOutline = "line" ## draws histograms by drawing the outline of all bars

  SmoothMethodKind* = enum
    smSVG = "svg",   ## Savitzky-Golay filter smoothing ("LOESS")
    smLM = "lm",     ## Perform a Levenberg-Marquardt fit
    smPoly = "poly"  ## Perform a polynomial fit of given order

  Geom* = object
    gid*: uint16 # unique id of the geom
    data*: Option[DataFrame] # optionally a geom may have its own data frame
    userStyle*: GgStyle # if set, apply this style instead of parent's
    position*: PositionKind
    aes*: Aesthetics # a geom can have its own aesthetics. Needs to be part of
                    # the `Geom`, because if we add it to `GgPlot` we lose track
                    # of which geom it corresponds to
    binPosition*: BinPositionKind
    case kind*: GeomKind
    of gkHistogram:
      hdKind*: HistogramDrawingStyle
    else: discard
    case statKind*: StatKind
    of stBin:
      numBins*: int # number of bins
      binWidth*: Option[float] # width of bins in terms of the data, overrides `numBins`
      # bin edges given in data values. Overrides `numBins` and `binWidth`
      binEdges*: Option[seq[float]]
      binBy*: BinByKind # determine whether full data or only current subset is considered
                        # as the range to call `histogram` with
      density*: bool ## if true will compute the density instead of counts in
                     ## each bin
    of stSmooth:
      span*: float                  ## The window width we use to compute the smoothed output
      polyOrder*: int               ## Polynomial order to use for SVG filter
      methodKind*: SmoothMethodKind ## The method to use for smoothing (Savitzky-Golay, LM)
    else: discard

  ## `OutsideRangeKind` determines what is done to values, which lay outside of the
  ## plot's data range (its associated `ginger.Scale`).
  OutsideRangeKind* = enum
    orkNone = "none" ## leave points where they are. Will be drawn somewhere outside the plot area,
                     ## unless so much larger than plot range that they don't fit canvas at all
    orkDrop = "drop" ## remove points outside of plot range
    orkClip = "clip" ## clip outside points either to max value along the range or, if any is set
                     ## to the `*MarginRange`. That is the data range + the `*Margin`. See the
                     ## `*Margin` procs for more information.

  # what might become a theming object
  Theme* = object
    # font
    baseFontSize*: Option[float] # base size for fonts NOT IMPL'd
    fontSizeScale*: Option[float] # scales all fonts by given value NOT IMPL'd
    labelFont*: Option[Font]
    titleFont*: Option[Font]
    subTitleFont*: Option[Font]
    tickLabelFont*: Option[Font]
    # hide parts of plot
    hideTicks*: Option[bool]
    hideTickLabels*: Option[bool]
    hideLabels*: Option[bool]
    # titles
    title*: Option[string]
    subTitle*: Option[string]
    # labels
    xlabel*: Option[string]
    xlabelMargin*: Option[float]
    xLabelSecondary*: Option[string]
    ylabel*: Option[string]
    ylabelMargin*: Option[float]
    yLabelSecondary*: Option[string]
    # ticks
    xTicksRotate*: Option[float]
    xTicksTextAlign*: Option[TextAlignKind]
    xTickLabelMargin*: Option[float] # tick label margin in multiples of font height
    yTicksRotate*: Option[float]
    yTicksTextAlign*: Option[TextAlignKind]
    yTickLabelMargin*: Option[float] # tick label margin in multiples of font height
    # legend
    legendPosition*: Option[Coord]
    legendOrder*: Option[seq[int]]
    hideLegend*: Option[bool]
    # canvas
    canvasColor*: Option[Color] # background color of the whole canvas
    plotBackgroundColor*: Option[Color] # background color of a plot. By default grey92
    # grid lines
    gridLineColor*: Option[Color] # color of the grid lines. By default white.
    # data range
    discreteScaleMargin*: Option[Quantity] # margin applied to scale of discrete kindn default 0.2 `cm`
    xRange*: Option[ginger.Scale]
    yRange*: Option[ginger.Scale]
    xMargin*: Option[float] # additional margin to show
    xMarginRange*: ginger.Scale # final range taking into account margin, always calculated
    yMargin*: Option[float]
    yMarginRange*: ginger.Scale # final range taking into account margin
    xOutsideRange*: Option[OutsideRangeKind]
    yOutsideRange*: Option[OutsideRangeKind]
    # plot window
    plotMarginLeft*: Option[Quantity]
    plotMarginRight*: Option[Quantity]
    plotMarginTop*: Option[Quantity]
    plotMarginBottom*: Option[Quantity]
    # Facet related options
    facetMargin*: Option[Quantity]

  ThemeMarginLayout* = object
    left*: Quantity
    right*: Quantity
    top*: Quantity
    bottom*: Quantity
    requiresLegend*: bool

  # dummy annotation, which puts `text` at bottom left corner
  # of `(left, height)`
  Annotation* = object
    left*: Option[float]
    bottom*: Option[float]
    x*: Option[float]
    y*: Option[float]
    text*: string
    font*: Font
    rotate*: Option[float]
    backgroundColor*: Color

  GgPlot* = object
    data*: DataFrame
    title*: string
    subtitle*: string
    # GgPlot can only contain a single `aes` by itself. Geoms may contain
    # seperate ones
    aes*: Aesthetics # original `aes` given in `ggplot()` call. Won't be modified
    numXticks*: int
    numYticks*: int
    facet*: Option[Facet]
    ridges*: Option[Ridges] # creates a ridgeline plot where `y` scale becomes ridge
    geoms*: seq[Geom]
    annotations*: seq[Annotation]
    theme*: Theme
    backend*: BackendKind ## the backend to use. Determined automatically from filename and
                          ## possible options given to `ggsave`

  StyleLabel* = object
    style*: GgStyle
    label*: Value

  # A filled geom is a geom plus a closure to yield the
  FilledGeom* = object
    geom*: Geom
    # NOTE: these are strings, because they are the stringification of the
    # `FormulaNode` given to `aes`! Potential calculations have already been
    # done to the data
    xcol*: string
    ycol*: string
    # each geom will fill its own data scales. The filledScale will then
    # select the correct scale for the whole plot
    xScale*: ginger.Scale
    yScale*: ginger.Scale
    reversedX*: bool
    reversedY*: bool
    # `GgStyle` stores base style for each value of the discrete (!) scales
    # key is a VObject Value of all mapped columns, first value arg is base style
    # second arg individual styles if cont scale involved, last arg is data
    yieldData*: OrderedTable[Value, (GgStyle, seq[GgStyle], DataFrame)]
    # whether X or Y is discrete or continuous. Has direct implication for drawing
    case dcKindX*: DiscreteKind
    of dcDiscrete:
      xLabelSeq*: seq[Value]
    else: discard
    case dcKindY*: DiscreteKind
    of dcDiscrete:
      yLabelSeq*: seq[Value]
    else: discard
    # the number of max elements for each dimensions found for this geom
    # if x or y is discrete the following will store the number of classes
    # if continuous just the max number of elements the largest style
    numX*: int
    numY*: int
    # alternative Table[AesField, stringification of FormulaNode]?
    case geomKind*: GeomKind
    of gkErrorBar:
      xMin*: Option[string]
      yMin*: Option[string]
      xMax*: Option[string]
      yMax*: Option[string]
    of gkTile, gkRaster:
      # this refers to the actual data column containing the data to use to fill!
      fillCol*: string
      # `fillDataScale` is used for continuous data
      fillDataScale*: ginger.Scale
      width*: Option[string]
      height*: Option[string]
    of gkText:
      # required if text is used
      text*: string
    of gkHistogram:
      hdKind*: HistogramDrawingStyle
    else: discard

  MainAddScales* = tuple[main: Option[Scale], more: seq[Scale]]
  FilledScales* = object
    xScale*: ginger.Scale
    yScale*: ginger.Scale
    reversedX*: bool
    reversedY*: bool
    discreteX*: bool
    discreteY*: bool
    geoms*: seq[FilledGeom]
    x*: MainAddScales
    y*: MainAddScales
    color*: MainAddScales
    fill*: MainAddScales
    size*: MainAddScales
    shape*: MainAddScales
    xMin*: MainAddScales
    xMax*: MainAddScales
    yMin*: MainAddScales
    yMax*: MainAddScales
    width*: MainAddScales
    height*: MainAddScales
    text*: MainAddScales # not needed, since we don't collect text
    yRidges*: MainAddScales
    weight*: MainAddScales
    facets*: seq[Scale] # no difference between main / more

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
     s1.col.name == s2.col.name:
    # the other fields ``will`` be computed to the same!
    result = true
  else:
    result = false

proc hash*(s: GgStyle): Hash =
  if s.color.isSome:
    let c = s.color.unsafeGet
    result = hash(c.r)
    result = result !& hash(c.g)
    result = result !& hash(c.b)
  if s.size.isSome:
    result = result !& hash(s.size.unsafeGet)
  if s.lineType.isSome:
    result = result !& hash(s.lineType.unsafeGet)
  if s.lineWidth.isSome:
    result = result !& hash(s.lineWidth.unsafeGet)
  if s.fillColor.isSome:
    let fc = s.fillColor.unsafeGet
    result = result !& hash(fc.r)
    result = result !& hash(fc.g)
    result = result !& hash(fc.b)
  if s.marker.isSome:
    result = result !& hash(s.marker.unsafeGet)
  result = !$result

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
  of scText: discard
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

proc `$`*(s: Scale): string
proc `$`*(f: Facet): string =
  result = "(columns: "
  for i, x in f.columns:
    if i == f.columns.len - 1:
      result.add x & ")"
    else:
      result.add x & ", "

proc hash*(x: StyleLabel): Hash =
  result = hash(x.style)
  result = result !& hash(x.label)

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
  result.add &"style: {g.userStyle}, "
  result.add &"aes: {g.aes}, "
  result.add &"position: {g.position}, "
  result.add &"binPosition: {g.binPosition}, "
  result.add &"statKind: {g.statKind}, "
  case g.statKind
  of stBin:
    result.add "("
    result.add &"numBins: {g.numBins}, "
    result.add &"binWidth: {g.binWidth}, "
    result.add &"binEdges: {g.binEdges}"
    result.add ")"
  of stSmooth:
    result.add "("
    result.add &"span: {g.span}, "
    result.add &"polyOrder: {g.polyOrder}, "
    result.add &"methodKind: {g.methodKind}"
    result.add ")"
  else: discard
  result.add ")"

macro typeName(x: typed): untyped =
  let str = x.getTypeInst.repr
  result = quote do:
    `str`

proc `$`*(p: GgPlot): string =
  result = "(data:\n" & $p.data & "\n"
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
    result.add &", reversed: {s.reversed}"
    result.add &", trans.isNil?: {s.trans.isNil}"
    result.add &", secondaryAxis: {s.secondaryAxis}"
    result.add &", dateScale: {s.dateScale}"
  else: discard
  result.add &", hasDiscreteness: {s.hasDiscreteness}"
  result.add &", dcKind: {s.dcKind}"
  case s.dcKind
  of dcDiscrete:
    result.add &", valueMap: {s.valueMap}"
    result.add &", labelSeq: {s.labelSeq}"
  of dcContinuous:
    result.add &", dataScale: {s.dataScale}"
    result.add &", mapData.isNil?: {s.mapData.isNil}"
  result.add ")"
