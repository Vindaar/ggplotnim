import std / [options, tables, hashes, macros, strformat, times, sets]
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
    alpha*: Option[Scale] # classify by alpha
    size*: Option[Scale] # classify by size
    shape*: Option[Scale] # classify by shape
    width*: Option[Scale] # width of tile / rect / raster
    height*: Option[Scale] # height of tile / rect / raster
    text*: Option[Scale] # text to display
    yRidges*: Option[Scale]
    weight*: Option[Scale]

  ScaleKind* = enum
    scLinearData, scTransformedData, scColor, scFillColor, scAlpha, scShape, scSize, scText

  PositionKind* = enum
    pkIdentity = "identity"
    pkStack = "stack"
    pkDodge = "dodge"
    pkFill = "fill"

  StatKind* = enum
    stIdentity = "identity" # use data as is
    stCount = "count"       # simple counting of discrete cases
    stBin = "bin"           # binning into a histogram / freqPoly
    stSmooth = "smooth"     # smoothing via Savitzky-Golay filter
    stDensity = "density"   # density estimation via Kernel Density Estimation (TODO: selection of kernel)
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
    of scAlpha:
      alpha*: float # a value between 0 and 1
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

  ## The `DateTickAlgorithmKind` is an experimental enum that allows a choice between two
  ## different algorithms for the determination of date based tick labels using the
  ## given `dateSpacing`.
  ##
  ## In the case of `dtaFilter` (default) we compute the parsed dates for all elements in the
  ## date time column first and then attempt to filter out all values to leave those that
  ## match the `dateSpacing`. This works well for densely packed timestamps in a column and
  ## deals better with rounding of e.g. 52 weeks =~= 1 year like tick labels.
  ##
  ## For sparser time data, use the `dtaAddDuration` algoritm, which simply determines the
  ## first suitable date based on the format string and adds the `dateSpacing` to each of
  ## these. The next matching date based on the `formatString` is used. This does not handle
  ## rounding of dates well (4 weeks =~= 1 month will produce mismatches at certain points
  ## for example), but should be more robust.
  DateTickAlgorithmKind* = enum
    dtaFilter,      ## compute the date ticks by filtering to closest matches
    dtaAddDuration  ## compute the date ticks by adding given duration to start time
    dtaCustomBreaks ## use user given custom breaks (as unix timestamp)

  DateScale* = object
    name*: string
    axKind*: AxisKind   ## which axis does it belong to?
    isTimestamp*: bool  ## is it a timestamp?
    breaks*: seq[float] ## timestamps to use as tick labels. Overrides `dateSpacing`. Forces
                        ## `dateAlgo` to `dtaCustomBreaks`.
    parseDate*: proc(s: string): DateTime ## possible parser for string columns
    #formatDate*: proc(dt: DateTime): string # formatter for labels, required
    formatString*: string ## the string to format dates with
    dateSpacing*: Duration ## required duration between two ticks
    dateAlgo*: DateTickAlgorithmKind ## See enum description above
    timeZone*: Timezone ## relevant if input data is a timestamp.

  ## Missing is a helper object to identify "no value was given" to arguments to
  ## `geom_*`. This helps us avoid the need of `some(...)` (using `Option[T]` arguments).
  ## By default each optional argument is given a `Missing()`. If it's not that, we
  ## know the user supplied it.
  Missing* = object

  ## Different ways a color may be represented. As a `chroma.Color` object,
  ## a `uint32` or a string name of a CSS-like color description
  ##
  ## Note: If `string` does not parse correctly as a color, we will attempt
  ## to interpret it as a DF column. If it is one, the DF column may contain
  ## itself `int` values (converted to `uint32` as colors; you're on your own
  ## if you store out of range values here. Conversion is done via `val.uint32`)
  ## or `string` values storing color names.
  PossibleColor* = Missing | Color | uint32 | string | Option[Color]
  PossibleFloat* = Missing | SomeNumber | string | Option[float]
  PossibleBool* = Missing | bool
  PossibleMarker* = Missing | MarkerKind | Option[MarkerKind]
  PossibleLineType* = Missing  | LineType | Option[LineType]
  PossibleErrorBar* = Missing | ErrorBarKind | Option[ErrorBarKind]
  PossibleFont* = Missing | Font | Option[Font]
  PossibleSecondaryAxis* = Missing | SecondaryAxis

  ColorScale* = object
    name*: string # name of the used color scale
    colors*: seq[uint32] # the used color scale as colors encoded in uint32

  DataKind* = enum
    dkMapping = "mapping" ## default for size / color: compute mapping based on
                          ## data *values* stored in the column
    dkSetting = "setting" ## default for scLinear / scTransformed. Treat data
                          ## in referenced column literally. x/y: data position,
                          ## size/color: literal size/color using column value

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
    numTicks*: Option[int] # the desired number of ticks for this scale (may be ignored)
    breaks*: seq[float]    # optional position for all ticks in data units. Overrides `numTicks` if any
    dataKind*: DataKind
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
    of scColor, scFillColor:
      # color scale to use if continuous (overrides `viridis` default)
      colorScale*: ColorScale
      transC*: ScaleTransform # potential transformation of the color scale via log10, ...
      invTransC*: ScaleTransform # XXX: these should be merged with `scTransformedData`
    of scSize:
      sizeRange*: tuple[low, high: float] # range of sizes to use (default: 2.0 - 7.0)
    of scAlpha:
      alphaRange*: tuple[low, high: float] # range of sizes to use (default: 0.1 - 1.0)
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

  SingleFacet* = object
    idx*: int
    xScale*: ginger.Scale
    yScale*: ginger.Scale

  Facet* = object
    columns*: seq[Scale] # the Scales associated based on their columns
    sfKind*: ScaleFreeKind
    order*: seq[Value] ## Facets will be arranged in this order top left to bottom right if any given
    # The below fields are filled in `postproccess_scales`
    combinations*: OrderedSet[Value] # all labels that produce a facet
    facets*: OrderedTable[Value, SingleFacet] # Maps each label to a SingleFacet, which stores all data of interest

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
    backend*: BackendKind # allows to overwrite backend selection
    dataAsBitmap*: bool ## Will draw the actual data of the plot as bitmap and insert (similar to raster)

  ## helper to generate a json file from a ggplot call by creating a `JsonNode`
  ## of the final plot `Viewport`. This is mainly used for the CI.
  JsonDummyDraw* = object
    fname*: string
    width*: Option[float]
    height*: Option[float]
    backend*: BackendKind # required to generate actual numbers
    fType*: FileTypeKind

  # helper object to compose `ggvega` via `+` with `ggplot`
  # Used to show a plot using the Vega-Lite backend
  VegaBackend* = enum
    vbWebview = "webview"
    vbBrowser = "browser"
  VegaDraw* = object
    fname*: string
    width*: Option[float]
    height*: Option[float]
    asPrettyJson*: Option[bool]
    show*: bool # decides whether to show the vega plot directly
    backend*: VegaBackend # if we show it, use webview or browser?
    removeFile*: bool # if set to true, remove the generated file after
                      # showing it. Only relevant for HTML files
    divName*: string # the name of the <div> tag
    vegaLibsPath*: string # by default: https://cdn.jsdelivr.net/npm/
    vegaVersion*: string # by default 5
    vegaLiteVersion*: string # by default 4
    vegaEmbedVersion*: string # by default 6

  # helper object that refers to 2 plots. A TeX version and a Vega version
  VegaTeX* = object
    fname*: string
    width*: Option[float]
    height*: Option[float]
    texOptions*: TeXOptions

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
    gkPoint     = "point"
    gkBar       = "bar"
    gkHistogram = "histogram"
    gkFreqPoly  = "freqpoly"
    gkTile      = "tile"
    gkLine      = "line"
    gkErrorBar  = "errorbar"
    gkText      = "text"
    gkRaster    = "raster"

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
    of stDensity:
      bandwidth*: float      ## bandwidth to use. By default determined via Silverman's rule of thumb
      adjust*: float         ## multiplicative adjustment of the auto determined bandwidth
      kernel*: string        ## kernel to use for the KDE
      samples*: int          ## number of samples to use (optional, default = 1000)
      sampleSeq*: seq[float] ## sequence of where to sample the data (optional)
      range*: tuple[l: float, h: float] ## data range in which to compute the KDE, by default full data range
      normalize*: bool       ## normalize the resulting KDE or not
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
    # plot size
    width*: Option[float]
    height*: Option[float]
    fixedRatio*: Option[float] # height will be computed based on this fixed data ratio
    # font
    baseFontSize*: Option[float] # base size for fonts NOT IMPL'd
    fontSizeScale*: Option[float] # scales all fonts by given value NOT IMPL'd
    labelFont*: Option[Font]
    titleFont*: Option[Font]
    subTitleFont*: Option[Font]
    tickLabelFont*: Option[Font]
    facetHeaderFont*: Option[Font]
    legendFont*: Option[Font]
    legendTitleFont*: Option[Font]
    annotationFont*: Option[Font]
    # general
    baseScale*: Option[float] ## Base scale to scale "everything" by
    texOptions*: Option[TeXOptions] ## If given overwrites any TeX related argument to `ggsave`
    # hide parts of plot
    hideTitle*: Option[bool]
    hideTicks*: Option[bool]
    hideTickLabels*: Option[bool]
    hideXTickLabels*: Option[bool] # only hide X tick labels (does not adjust margins)
    hideYTickLabels*: Option[bool] # only hide Y tick labels (does not adjust margins)
    hideLabels*: Option[bool]
    hideXLabels*: Option[bool] # only hide X labels (does not adjust margins)
    hideYLabels*: Option[bool] # only hide Y labels (does not adjust margins)
    # titles
    title*: Option[string]
    subTitle*: Option[string]
    titlePosition*: Option[Coord]
    # facet
    facetHeaderPos*: Option[Coord] # Position of the text inside the facet header
    # labels
    xLabel*: Option[string]
    xLabelMargin*: Option[float]
    xLabelSecondary*: Option[string]
    yLabel*: Option[string]
    yLabelMargin*: Option[float]
    yLabelSecondary*: Option[string]
    baseLabelMargin*: Option[float] ## The base margin added to the width / height of tick labels to label.
                                    ## Default: 0.3 cm
    # ticks
    xTicksRotate*: Option[float]
    xTicksTextAlign*: Option[TextAlignKind]
    xTickLabelMargin*: Option[float] # tick label margin in multiples of font height
    yTicksRotate*: Option[float]
    yTicksTextAlign*: Option[TextAlignKind]
    yTickLabelMargin*: Option[float] # tick label margin in multiples of font height
    # general ticks
    tickLength*: Option[float] ## Width is 1/5 of length for now!
    tickWidth*: Option[float] ## unless set explicitly!
    tickColor*: Option[Color] ## Width is 1/5 of length for now!
    tickKind*: Option[TickKind] ## Kind of tick. One sided (`tkOneSide`) or two sided (`tkBothSides`)
    # legend
    legendPosition*: Option[Coord]
    legendOrder*: Option[seq[int]]
    hideLegend*: Option[bool]
    continuousLegendHeight*: Option[float] # Height of a continuous legend in Centimeter
    continuousLegendWidth*: Option[float] # Width of a continuous legend in Centimeter
    discreteLegendHeight*: Option[float] # Height of a discrete legend block in Centimeter
    discreteLegendWidth*: Option[float] # Width of a discrete legend block in Centimeter
    # canvas
    canvasColor*: Option[Color] # background color of the whole canvas
    plotBackgroundColor*: Option[Color] # background color of a plot. By default grey92
    # grid lines
    gridLines*: Option[bool]      # whether to draw grid lines
    gridLineColor*: Option[Color] # color of the grid lines. By default white.
    gridLineWidth*: Option[float] # width of grid line, default 1pt
    minorGridLines*: Option[bool] # whether to draw minor grid lines
    minorGridLineWidth*: Option[float] # width of minor lines, default half width of major lines
    onlyAxes*: Option[bool] # whether to *only* draw axes and no grid lines
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
    preferRowsOverColumns*: Option[bool]

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
    top*: Option[float]
    right*: Option[float]
    text*: string
    font*: Font
    rotate*: Option[float]
    alignKind*: TextAlignKind
    backgroundColor*: Color

  GgPlot* = object
    data*: DataFrame
    title*: string
    subTitle*: string
    # GgPlot can only contain a single `aes` by itself. Geoms may contain
    # seperate ones
    aes*: Aesthetics # original `aes` given in `ggplot()` call. Won't be modified
    facet*: Option[Facet]
    ridges*: Option[Ridges] # creates a ridgeline plot where `y` scale becomes ridge
    geoms*: seq[Geom]
    annotations*: seq[Annotation]
    theme*: Theme
    backend*: BackendKind ## the backend to use. Determined automatically from filename and
                          ## possible options given to `ggsave`
    fType*: FileTypeKind ## The file type we render to. This is needed for accurate information
                         ## about the height / width of text.

  StyleLabel* = object
    style*: GgStyle
    label*: Value

  # A filled geom is a geom plus a closure to yield the
  FilledGeom* = object
    geom*: Geom
    # NOTE: these are strings, because they are the stringification of the
    # `FormulaNode` given to `aes`! Potential calculations have already been
    # done to the data
    xCol*: string
    yCol*: string
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
      rasterXScale*: ginger.Scale # x scale of the raster data only
      rasterYScale*: ginger.Scale # y scale of the raster data only
      colorScale*: ColorScale # the color scale used to plot this
      trans*: ScaleTransform # for log10 of color scale
      invTrans*: ScaleTransform
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
    inputData*: DataFrame # The input DF of the `GgPlot.data` field
    reversedX*: bool
    reversedY*: bool
    discreteX*: bool
    discreteY*: bool
    geoms*: seq[FilledGeom]
    x*: MainAddScales
    y*: MainAddScales
    color*: MainAddScales
    fill*: MainAddScales
    alpha*: MainAddScales
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
    facets*: Facet # Filled `Facet` object containing multiple `Scales` and additional
                   # information about the labels

  # `PlotView` describes the object the final representation of a `GgPlot` before
  # being drawn.
  PlotView* = object
    # `filledScales` is essentially the final aesthetics used in the drawing of
    # the plot, based on `GgPlot.aes` and `geoms.aes` combined and includes additional
    # fields to avoid dependency on GgPlot object to draw
    filledScales*: FilledScales
    view*: Viewport # the ginger representation of the plot

  VegaError* = object of CatchableError

proc missing*(): Missing =
  ## Helper to get a `Missing` instance.
  Missing()

proc toOptColor*[T: PossibleColor](x: T): Option[Color] =
  when T is Missing: result = none[Color]()
  elif T is Color: result = some(x)
  elif T is uint32: result = some(x.toColor())
  elif T is string:
    try:
      result = some(parseHtmlColor(x))
    except InvalidColor:
      result = none[Color]()
  elif T is Option[Color]: result = x
  else: {.error: "Invalid branch!".}

## NOTE: all the following could be generated by a macro. But amount of
## code isn't enough to make that worthwhile.
proc toOptFloat[T: PossibleFloat](x: T): Option[float] =
  when T is Missing: result = none[float]()
  elif T is SomeNumber: result = some(x.float)
  elif T is Option[float]: result = x
  else: {.error: "Invalid branch!".}

proc toOptBool*[T: PossibleBool](x: T): Option[bool] =
  when T is Missing: result = none[bool]()
  elif T is bool: result = some(x)
  else: {.error: "Invalid branch!".}

proc toOptMarker[T: PossibleMarker](x: T): Option[MarkerKind] =
  when T is Missing: result = none[MarkerKind]()
  elif T is MarkerKind: result = some(x)
  elif T is Option[MarkerKind]: result = x
  else: {.error: "Invalid branch!".}

proc toOptLineType[T: PossibleLineType](x: T): Option[LineType] =
  when T is Missing: result = none[LineType]()
  elif T is LineType: result = some(x)
  elif T is Option[LineType]: result = x
  else: {.error: "Invalid branch!".}

proc toOptErrorBar[T: PossibleErrorBar](x: T): Option[ErrorBarKind] =
  when T is Missing: result = none[ErrorBarKind]()
  elif T is ErrorBarKind: result = some(x)
  elif T is Option[ErrorBarKind]: result = x
  else: {.error: "Invalid branch!".}

proc toOptFont*[T: PossibleFont](x: T): Option[Font] =
  when T is Missing: result = none[Font]()
  elif T is Font: result = some(x)
  elif T is Option[Font]: result = x
  else: {.error: "Invalid branch!".}

proc toOptSecAxis*[T: PossibleSecondaryAxis](x: T, axis: AxisKind): Option[SecondaryAxis] =
  when T is Missing: result = none[SecondaryAxis]()
  elif T is SecondaryAxis:
    var sa = x
    sa.axKind = axis
    result = some(sa)
  elif T is Option[SecondaryAxis]:
    var sa = x.get
    sa.axKind = axis
    result = some(sa)
  else: {.error: "Invalid branch!".}

proc colorOrStyle[T: PossibleColor](aes: var Aesthetics, x: T): Option[Color] =
  result = toOptColor(x)
  if result.isNone:
    when T is string: # is a column reference
      doAssert x.len > 0, "Don't hand an empty string as a column reference!"
      aes.color = some(scale_color_identity(x))

proc fillOrStyle[T: PossibleColor](aes: var Aesthetics, x: T): Option[Color] =
  result = toOptColor(x)
  if result.isNone:
    when T is string: # is a column reference
      doAssert x.len > 0, "Don't hand an empty string as a column reference!"
      aes.fill = some(scale_fill_identity(x))

proc sizeOrStyle[T: PossibleFloat](aes: var Aesthetics, x: T): Option[float] =
  result = toOptFloat(x)
  if result.isNone:
    when T is string: # is a column reference
      doAssert x.len > 0, "Don't hand an empty string as a column reference!"
      aes.size = some(scale_size_identity(x))

proc alphaOrStyle[T: PossibleFloat](aes: var Aesthetics, x: T): Option[float] =
  result = toOptFloat(x)
  if result.isNone:
    when T is string: # is a column reference
      doAssert x.len > 0, "Don't hand an empty string as a column reference!"
      aes.alpha = some(scale_alpha_identity(x))

func initGgStyle(
  color = none[Color](),
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

## NOTE: the explicit generics are there to avoid code gen errors.
## LINK NIM-LANG ISSUE
## Essentially if `missing()` appears for two "same" (e.g. `PossibleFloat`)
## fields, but one of them is used, the other isn't, the second field is just
## uhhh, literally, missing
proc assignIdentityScalesGetStyle*[
  C: PossibleColor;
  S: PossibleFloat;
  M: PossibleMarker;
  LT: PossibleLineType;
  LW: PossibleFloat;
  FC: PossibleColor;
  EB: PossibleErrorBar;
  A: PossibleFloat;
  F: PossibleFont](
    aes: var Aesthetics,
    pColor: C = missing(),
    pSize: S = missing(),
    pMarker: M = missing(),
    pLineType: LT = missing(),
    pLineWidth: LW = missing(),
    pFillColor: FC = missing(),
    pErrorBarKind: EB = missing(),
    pAlpha: A = missing(),
    pFont: F = missing()
                 ): GgStyle =
  ## Modifies the given `aes` for all inputs that are interpreted as
  ## being a "column 'setting'" (i.e. `DataKind = dkSetting`).
  ## For every other case, fills the corresponding field of a `GgStyle`,
  ## which will be returned.
  # the first 4 might modify `aes`
  let color = colorOrStyle(aes, pColor)
  let fill = fillOrStyle(aes, pFillColor)
  let size = sizeOrStyle(aes, pSize)
  let alpha = alphaOrStyle(aes, pAlpha)
  # the rest just turns input into `Option[T]`
  let marker = toOptMarker(pMarker)
  let lineType = toOptLineType(pLineType)
  let lineWidth = toOptFloat(pLineWidth)
  let errorBar = toOptErrorBar(pErrorBarKind)
  let font = toOptFont(pFont)
  initGgStyle(color, size, marker, lineType, lineWidth,
              fill, errorBar, alpha, font)

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
  of scAlpha:
    result = result !& hash(x.alpha)
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
      result.add $x & ")"
    else:
      result.add $x & ", "
  result.add $(f.combinations)
  result.add $(f.facets)

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

proc `$`*(p: GgPlot): string =
  result = "(data:\n" & $p.data & "\n"
  result.add ", title: " & $p.title
  result.add ", subtitle: " & $p.subTitle
  result.add ", aes: " & $p.aes
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
  result.add &", numTicks: {s.numTicks}"
  result.add &", breaks: {s.breaks}"
  result.add &", dataKind: {s.dataKind}"
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
