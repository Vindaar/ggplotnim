import sequtils, tables, sets, algorithm, strutils
import ginger
import parsecsv, streams, strutils

import seqmath

import macros

import formula
export formula

type
  Aesthetics = object
    x: string
    y: Option[string]
    color: Option[string]

  Scales = object
    discard

  Facet = object
    discard

  Draw = object
    fname: string

  GeomKind = enum
    gkPoint, gkBar, gkHistogram, gkFreqPoly
  Geom = object
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

func geom_histogram*(binWidth = 0.0, bins = 0): Geom =
  result = Geom(kind: gkHistogram)

func geom_freqpoly*(): Geom =
  result = Geom(kind: gkHistogram)

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
    let
      minX = p.data[aes.x].mapIt(it.parseFloat).min
      maxX = p.data[aes.x].mapIt(it.parseFloat).max
    xScale = (low: minX, high: maxX)
    var
      minY: float
      maxY: float
    if aes.y.isSome:
      minY = p.data[aes.y.get].mapIt(it.parseFloat).min
      maxY = p.data[aes.y.get].mapIt(it.parseFloat).max
      yScale = (low: minY, high: maxY)

    if aes.color.isSome:
      var colorCs: seq[Color]
      colors = p.data[aes.color.get]
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

  var data = newSeq[GraphObject]()
  for geom in p.geoms:
    case geom.kind
    of gkPoint:
      for aes in p.aes:
        let xData = p.data[aes.x].mapIt(it.parseFloat)
        let yData = p.data[aes.y.get].mapIt(it.parseFloat)
        for i in 0 ..< xData.len:
          if aes.color.isSome:
            #let style = Style(fillColor: colorsCat[colors[i]])
            data.add initPoint(plt, (x: xData[i], y: yData[i]),
                               marker = mkCircle,
                               color = colorsCat[colors[i]])
          else:
            data.add initPoint(plt, (x: xData[i], y: yData[i]),
                               marker = mkCircle)
    of gkHistogram:
      for aes in p.aes:

        # before performing a calculation for the histogram viewports, get the
        # new xScale, by calling calcTickLocations with it
        # Note: we don't have to assign it to the `plt` viewport, since that will
        # happen when calculation of the actual ticks will be done later on
        let (newXScale, _, _) = calcTickLocations(xScale, numXTicks)

        # generate the histogram itself
        let rawDat = p.data[aes.x].mapIt(it.parseFloat)
        const nbins = 30
        let binWidth = (newXScale.high - newXScale.low).float / nbins.float
        #let bin_edges = linspace(newXScale.low - binWidth / 2.0, newXScale.high - binWidth / 2.0, nbins)
        let hist = rawDat.histogram(bins = nbins, range = (newXScale.low, newXScale.high))

        # given the histogram, we can now deduce the base yScale we need
        let yScaleBase = (low: 0.0, high: hist.max.float)
        # however, this is not the final one, since we still have to calculate
        # the tick locations, which might change it again
        let (newYScale, _, _) = calcTickLocations(yScaleBase, numYTicks)

        # create viewports showing the bars
        plt.yScale = newYScale
        img.yScale = newYScale
        plt.layout(nbins, 1)
        var i = 0
        for p in mitems(plt):
          doAssert p.yScale.high >= hist.max.float
          let yPos = 1.0 - quant(hist[i].float, ukData).toRelative(scale = some(p.yScale)).val
          let style = Style(lineType: ltSolid,
                            lineWidth: 1.0, # draw 1 pt wide black line to avoid white pixels
                                            # between bins at size of exactly 1.0 bin width
                            color: grey20,
                            fillColor: grey20)
          let r = p.initRect(c(0.0, yPos), # bottom left
                             quant(1.0, ukRelative),
                             quant(hist[i].float, ukData),#.toRelative(scale = some(p.yScale)),
                             style = some(style))
          p.addObj r
          inc i
    else:
      discard

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


  plt.addObj concat(xticks, yticks, xtickLabels, ytickLabels, @[xlabel, ylabel, grdLines], data)
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

proc readCsv*(fname: string): Table[string, seq[string]] =
  ## returns a CSV file as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  var s = newFileStream(fname, fmRead)
  if s == nil:
    quit("cannot open the file" & fname)

  var x: CsvParser
  open(x, s, fname)
  var isHeader = true
  x.readHeaderRow()
  result = initTable[string, seq[string]]()
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


#let pltCalc = ggplot(mpg, aes(year ~ (displ * hwy + cty), color = "class")) +
#  geom_point() +
#  ggtitle("ggplotnim - or I Suck At Naming Things™") +
#  ggsave("scatterColor.pdf")
