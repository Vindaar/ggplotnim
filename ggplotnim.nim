import sequtils, tables, sets, algorithm, strutils
import ginger
import parsecsv, streams, strutils

import seqmath

import macros

import persvector

import formula
export formula

type
  Aesthetics = object
    x: string
    y: Option[string]
    color: Option[string] # classify by color
    size: Option[string] # classify by size

  Scales = object
    discard

  Facet = object
    discard

  Draw = object
    fname: string

  GeomKind = enum
    gkPoint, gkBar, gkHistogram, gkFreqPoly
  Geom = object
    style: Option[Style] # if set, apply this style instead of parent's
    aes: Aesthetics # a geom can have its own aesthetics. Needs to be part of
                    # the `Geom`, because if we add it to `GgPlot` we lose track
                    # of which geom it corresponds to
    case kind: GeomKind
    of gkHistogram, gkFreqPoly:
      bins: int # number of bins
      binWidth: float # width of bins in terms of the data
    else:
      discard

  GgPlot[T] = object
    data: T
    title: string
    subtitle: string
    aes: seq[Aesthetics]
    scales: seq[Scales]
    facets: seq[Facet]
    geoms: seq[Geom]

proc ggplot*[T](data: T, aes: Aesthetics): GgPlot[T] =
  result = GgPlot[T](data: data,
                     aes: @[aes])
  # TODO: fill others with defaults

func geom_point*(): Geom =
  result = Geom(kind: gkPoint)

func geom_bar(): Geom =
  result = Geom(kind: gkBar)

func geom_histogram*(binWidth = 0.0, bins = 0,
                     color: Color = grey20, # color of the bars
                    ): Geom =
  let style = Style(lineType: ltSolid,
                    lineWidth: 1.0, # draw 1 pt wide black line to avoid white pixels
                                    # between bins at size of exactly 1.0 bin width
                    color: color, # default color
                    fillColor: color)
  result = Geom(kind: gkHistogram,
                style: some(style))

func geom_freqpoly*(color: Color = grey20, # color of the line
                    size: float = 1.0, # line width of the line
                    lineType: LineType = ltSolid,
                   ): Geom =
  let style = Style(lineType: lineType,
                    lineWidth: size,
                    color: color,
                    fillColor: transparent)
  result = Geom(kind: gkFreqPoly,
                style: some(style))

proc ggtitle*(title: string, subtitle = ""): (string, string) = (title, subtitle)

proc createLegend(view: var Viewport,
                  title: string,
                  markers: seq[GraphObject],
                  cat: OrderedTable[string, Color]) =
  ## creates a full legend within the given viewport based on the categories
  ## in `cat` with a headline `title` showing data points of `markers`
  let startIdx = view.len
  view.layout(1, rows = cat.len + 1)
  # iterate only over added children, skip first, because we actual legend first
  var j = 0
  var catSeq: seq[string]
  for k, v in cat:
    catSeq.add k
  for i in startIdx + 1 ..< view.len:
    # create rectangle showing style of data points
    var ch = view[i]
    let viewRatio = ch.hView.val / ch.wView.val
    let sizeY = ch.height.toPoints(some(ch.hView))
    let style = Style(lineType: ltSolid,
                      lineWidth: 1.0,
                      color: color(1.0, 1.0, 1.0),
                      fillColor: grey92)
    var rect = ch.initRect(c(0.0, 0.0),
                           quant(ch.height.val * viewRatio, ukRelative),
                           quant(1.0, ukRelative),
                           style = some(style))
    rect.name = "markerRectangle"
    # add marker ontop of rect
    let point = ch.initPoint(Coord(x: c1(ch.height.val / 2.0 * viewRatio),
                                   y: c1(0.5)),
                             marker = markers[j].ptMarker,
                             color = cat[catSeq[j]])

    let label = ch.initText(
      Coord(
        x: c1(ch.height.val * viewRatio) +
           c1(quant(0.3, ukCentimeter).toRelative(some(ch.wImg)).val),
        y: c1(0.5)),
      catSeq[j],
      alignKind = taLeft
    )
    ch.addObj [rect, point, label]
    view[i] = ch
    inc j
  # get the first viewport for the header
  var header = view[startIdx]
  var label = header.initText(
    Coord(x: header.origin.x,
          y: header.origin.y + c1(header.height.val * 3.0 * header.hView.val / header.wView.val)),
    title,
    alignKind = taLeft)
  # set to bold
  label.txtFont.bold = true
  header.addObj label
  view[startIdx] = header

proc aes*(x: string, y = "", color = ""): Aesthetics =
  var yOpt: Option[string]
  var colorOpt: Option[string]
  if y.len > 0:
    yOpt = some(y)
  if color.len > 0:
    colorOpt = some(color)
  result = Aesthetics(x: x, y: yOpt, color: colorOpt)

proc aes*(x: FormulaNode, color = ""): Aesthetics =
  result = Aesthetics(x: "", y: some(""), color: some(color))

proc `+`*(p: GgPlot, geom: Geom): GgPlot =
  ## adds the given geometry to the GgPlot object
  result = p
  result.geoms.add geom

proc `+`*(p: GgPlot, facet: Facet): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.facets.add facet

proc `+`*(p: GgPlot, scale: Scales): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.scales.add scale

proc `+`*(p: GgPlot, aes: Aesthetics): GgPlot =
  ## adds the given aesthetics to the GgPlot object
  result = p
  result.aes.add aes

proc `+`*(p: GgPlot, titleTup: (string, string)): GgPlot =
  ## adds the given title / subtitle to the GgPlot object
  result = p
  result.title = titleTup[0]
  result.subtitle = titleTup[1]

proc requiresLegend(p: GgPlot): bool =
  ## returns true if the plot requires a legend to be drawn
  if p.aes.anyIt(it.color.isSome) or
     p.aes.anyIt(it.size.isSome) or
     p.geoms.anyIt(it.aes.color.isSome) or
     p.geoms.anyIt(it.aes.size.isSome):
    result = true
  else:
    result = false

proc plotLayoutWithLegend(view: var Viewport) =
  ## creates a layout for a plot in the current viewport that leaves space
  ## for a legend. Important indices of the created viewports:
  ## - main plot: idx = 4
  ## - legend: idx = 5
  view.layout(3, 3, colwidths = @[quant(2.0, ukCentimeter),
                                  quant(0.0, ukRelative),
                                  quant(5.0, ukCentimeter)],
              rowheights = @[quant(1.25, ukCentimeter),
                             quant(0.0, ukRelative),
                             quant(2.0, ukCentimeter)])

proc plotLayoutWithoutLegend(view: var Viewport) =
  ## creates a layout for a plot in the current viewport without a legend
  ## Main plot viewport will be:
  ## idx = 4
  view.layout(3, 3, colwidths = @[quant(2.0, ukCentimeter),
                                  quant(0.0, ukRelative),
                                  quant(1.0, ukCentimeter)],
              rowheights = @[quant(1.0, ukCentimeter),
                             quant(0.0, ukRelative),
                             quant(2.0, ukCentimeter)])

proc dataTo[T: Table | OrderedTable | DataFrame; U](
  df: T,
  col: string,
  outType: typedesc[U]): seq[U] =
  ## reads the column `col` from the Table / DataFrame and converts
  ## it to `outType`, returns it as a `seq[outType]`
  ## NOTE: For now we just assume that a Table will be of kind
  ## `Table[string, seq[string]]`!
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
    let dkind = df[col][0].kind
    when outType is SomeNumber:
      case dkind
      of VInt:
        result = df[col].toSeq.mapIt(it.num.outType)
      of VFloat:
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
    else:
      case dkind
      of VObject:
        doAssert false, "there cannot be a column with object type!"
      of VNull:
        raise newException(Exception, "Column " & $col & " has no data!")
      else: discard

proc ggsave*(p: GgPlot, fname: string) =
  # check if any aes
  doAssert p.aes.len > 0, "Needs at least one aesthetics!"
  const
    numXTicks = 10
    numYTicks = 10

  var
    xScale: Scale
    yScale: Scale
    colorsCat: OrderedTable[string, Color]
    colors: seq[string]
  for aes in p.aes:
    # determine min and max scales of aes
    let xdata = p.data.dataTo(aes.x, float)
    let
      minX = xdata.min
      maxX = xdata.max
    xScale = (low: minX, high: maxX)
    var
      minY: float
      maxY: float
    if aes.y.isSome:
      let ydata = p.data.dataTo(aes.y.get, float)
      minY = ydata.min
      maxY = ydata.max
      yScale = (low: minY, high: maxY)

    if aes.color.isSome:
      var colorCs: seq[Color]
      colors = p.data.dataTo(aes.color.get, string)
      # convert to set to filter duplicates, back to seq and sort
      let catSeq = colors.toSet.toSeq.sorted
      colorCs = ggColorHue(catSeq.len)
      for i, k in catSeq:
        colorsCat[k] = colorCs[i]

  # create the plot
  var img = initViewport(xScale = some(xScale),
                         yScale = some(yScale))

  let drawLegend = p.requiresLegend
  if drawLegend:
    img.plotLayoutWithLegend()
  else:
    img.plotLayoutWithoutLegend()

  # get viewport of plot
  var plt = img[4]
  plt.background()

  for geom in p.geoms:
    var data = newSeq[GraphObject]()
    # for each geom, we create a child viewport of `plt` covering
    # the whole viewport, which will house the data we just created.
    # Due to being a child, if will be drawn *after* its parent. This way things like
    # ticks will be below the data.
    # On the other hand this allows us to draw several geoms in on a plot and have the
    # order of the function calls `geom_*` be preserved
    var pChild = plt.addViewport()

    case geom.kind
    of gkPoint:
      for aes in p.aes:
        let xData = p.data.dataTo(aes.x, float)
        let yData = p.data.dataTo(aes.y.get, float)
        for i in 0 ..< xData.len:
          if aes.color.isSome:
            #let style = Style(fillColor: colorsCat[colors[i]])
            data.add initPoint(pChild, (x: xData[i], y: yData[i]),
                               marker = mkCircle,
                               color = colorsCat[colors[i]])
          else:
            data.add initPoint(pChild, (x: xData[i], y: yData[i]),
                               marker = mkCircle)
    of gkHistogram, gkFreqPoly:
      for aes in p.aes:

        # before performing a calculation for the histogram viewports, get the
        # new xScale, by calling calcTickLocations with it
        # Note: we don't have to assign it to the `pChild` viewport, since that will
        # happen when calculation of the actual ticks will be done later on
        let (newXScale, _, _) = calcTickLocations(xScale, numXTicks)

        # generate the histogram itself
        let rawDat = p.data.dataTo(aes.x, float)
        const nbins = 30
        let binWidth = (newXScale.high - newXScale.low).float / nbins.float

        # TODO: if aes.colos.isSome we have to group the data we histogram first
        # and calculate histograms for each

        let hist = rawDat.histogram(bins = nbins, range = (newXScale.low, newXScale.high))

        # given the histogram, we can now deduce the base yScale we need
        let yScaleBase = (low: 0.0, high: hist.max.float)
        # however, this is not the final one, since we still have to calculate
        # the tick locations, which might change it again
        let (newYScale, _, _) = calcTickLocations(yScaleBase, numYTicks)

        # create viewports showing the bars
        pChild.yScale = newYScale

        var style: Option[Style]
        if geom.style.isSome:
          style = geom.style
        else:
          # TODO: inherit from parent somehow?
          discard
        if geom.kind == gkHistogram:
          pChild.layout(nbins, 1)
          var i = 0
          for p in mitems(pChild):
            doAssert p.yScale.high >= hist.max.float
            echo "I ", i , " for len ", hist.len
            let yPos = 1.0 - quant(hist[i].float, ukData).toRelative(scale = some(p.yScale)).val
            let r = p.initRect(c(0.0, yPos), # bottom left
                               quant(1.0, ukRelative),
                               quant(hist[i].float, ukData),#.toRelative(scale = some(p.yScale)),
                               style = style)
            p.addObj r
            inc i
        else:
          doAssert geom.kind == gkFreqPoly
          # only single viewport will be used
          # calculate bin centers
          let binCenters = linspace(newXScale.low + binWidth / 2.0, newXScale.high - binWidth / 2.0, nbins)
          # build data points for polyLine
          var points = newSeq[Point](nbins)
          for i in 0 ..< nbins:
            points[i] = (x: binCenters[i], y: hist[i].float)

          # TODO: rewrite code such that user hands style, but we convert to
          # `Style` in geom?
          var style: Option[Style]
          if geom.style.isSome:
            style = geom.style
          else:
            # TODO: somehow inherit something from GgPlot, maybe via themes?
            discard
          # have to update the scale of our viewport! `initPolyLine` depends on it
          # However: we could change the `ginger` code to accept a seq[Coord] instead and
          # use the `newXScale, newYScale` as the ukData scale instead!
          pChild.xScale = newXScale
          data.add pChild.initPolyLine(points, style)

      # for these types there is no y data scale attached to the image so far,
      # thus we assign `pChild`'s data scale to it
      # TODO: solve this more elegantly!
      plt.yScale = pChild.yScale
      img.yScale = pChild.yScale
    else:
      discard

    # add the data to the child
    pChild.addObj data
    # add the data viewport to the plt
    plt.children.add pChild

  let
    xticks = plt.xticks(numXTicks)
    yticks = plt.yticks(numYTicks)
    xtickLabels = plt.tickLabels(xticks)
    ytickLabels = plt.tickLabels(yticks)
    xlabel = plt.xlabel(p.aes[0].x)
    grdlines = plt.initGridLines(some(xticks), some(yticks))

  var ylabel: GraphObject
  case p.geoms[0].kind
  of gkPoint:
    ylabel = plt.ylabel(p.aes[0].y.get)
  of gkHistogram:
    ylabel = plt.ylabel("count")
  else: discard

  plt.addObj concat(xticks, yticks, xtickLabels, ytickLabels, @[xlabel, ylabel, grdLines])
  img[4] = plt

  if drawLegend:
    var lg = img[5]
    lg.height = quant(img[4].height.val / 2.0, ukRelative) #quant(0.5, ukRelative)
    lg.origin.y = lg.origin.y + c1(img[4].height.val / 4.0)
    lg.origin.x = lg.origin.x + img.c1(0.5, akX, ukCentimeter)
    var markers: seq[GraphObject]
    for k, v in colorsCat:
      markers.add initPoint(plt,
                            (0.0, 0.0), # dummy coordinates
                            marker = mkCircle) # assign same marker as above
        #proc createLegend(view: var Viewport,
    #              title: string,
    #              markers: seq[GraphObject],
    #              cat: Table[string, Color]) =
    lg.createLegend(p.aes[0].color.get,
                    markers,
                    colorsCat)
    img[5] = lg
  if p.title.len > 0:
    var titleView = img[1]
    let font = Font(family: "sans-serif",
                    size: 16.0,
                    color: black)
    let title = titleView.initText(c(0.0, 0.5),
                                   p.title,
                                   taLeft,
                                   font = some(font))
    titleView.addObj title
    img[1] = titleView
  img.draw(fname)

proc ggsave(fname: string): Draw = Draw(fname: fname)

proc `+`(p: GgPlot, d: Draw) =
  p.ggsave(d.fname)

proc readCsv*(fname: string): OrderedTable[string, seq[string]] =
  ## returns a CSV file as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  var s = newFileStream(fname, fmRead)
  if s == nil:
    quit("cannot open the file" & fname)

  var x: CsvParser
  open(x, s, fname)
  var isHeader = true
  x.readHeaderRow()
  result = initOrderedTable[string, seq[string]]()
  for col in items(x.headers):
    result[col] = @[]
  while readRow(x):
    for col in items(x.headers):
      result[col].add x.rowEntry(col)

let mpg = readCsv("data/mpg.csv")

let plt = ggplot(mpg, aes(x = "displ", y = "hwy")) +
  geom_point()
plt.ggsave("scatter.pdf")

ggplot(mpg, aes(x = "displ", y = "cty", color = "class")) +
  geom_point() +
  ggtitle("ggplotnim - or I Suck At Naming Things™") +
  ggsave("scatterColor.pdf")

ggplot(mpg, aes("hwy")) +
  geom_histogram() +
  ggsave("simpleHisto.pdf")

ggplot(mpg, aes("hwy")) +
  geom_freqpoly() +
  ggsave("freqpoly.pdf")

ggplot(mpg, aes("hwy")) +
  geom_histogram() + # the order of the geom calls decides the order in which
                     # they are drawn! FreqPoly will be drawn on top of histo
  geom_freqpoly(color = parseHex("FD971F"),
                size = 3.0) +
  ggsave("histoPlusFreqpoly.pdf")


#let pltCalc = ggplot(mpg, aes(year ~ (displ * hwy + cty), color = "class")) +
#  geom_point() +
#  ggtitle("ggplotnim - or I Suck At Naming Things™") +
#  ggsave("scatterColor.pdf")
