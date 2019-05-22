import sequtils, tables, sets, algorithm
import ginger
import parsecsv, streams, strutils

type
  Aesthetics = object
    x: string
    y: string
    color: string

  Scales = object
    discard

  Facet = object
    discard

  Draw = object
    fname: string

  GeomKind = enum
    gkPoint, gkBar
  Geom = object
    kind: GeomKind

  GgPlot[T] = object
    data: T
    aes: seq[Aesthetics]
    scales: seq[Scales]
    facets: seq[Facet]
    geoms: seq[Geom]

proc ggplot[T](data: T, aes: Aesthetics): GgPlot[T] =
  result = GgPlot[T](data: data,
                     aes: @[aes])
  # TODO: fill others with defaults

proc geom_point(): Geom =
  result = Geom(kind: gkPoint)
proc geom_bar(): Geom =
  result = Geom(kind: gkBar)

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
  for i in startIdx + 1 ..< view.len:#startIdx + 2:#view.len:
    # create rectangle showing style of data points
    var ch = view[i]
    let viewRatio = ch.hView.val / ch.wView.val
    let sizeY = ch.height.toPoints(some(ch.hView))
    var rect = ch.initRect(c(0.0, 0.0),
                           quant(ch.height.val * viewRatio, ukRelative),
                           quant(1.0, ukRelative),
                           color = grey92)
    rect.name = "markerRectangle"
    # add marker ontop of rect
    let point = ch.initPoint(Coord(x: c1(ch.height.val / 2.0 * viewRatio),
                                   y: c1(0.5)),
                             marker = markers[j].ptMarker,
                             color = cat[catSeq[j]])
    echo point
    #let rectBla = ch.initRect(Coord(x: c1(cX),#centerX,
    #                                y: c1(2 * cY)),#centerY),
    #                           quant(0.25, ukCentimeter),
    #                           quant(0.25, ukCentimeter),
    #                           #marker = markers[j].ptMarker,
    #                           color = cat[catSeq[j]])

    #'let point2 = ch.initPoint(Coord(x: c1(cX),#centerX,
    #                                y: c1(0.0)),#centerY),
    #                         marker = markers[j].ptMarker,
    #                         color = cat[catSeq[j]])

    let label = ch.initText(
      Coord(
        x: c1(ch.height.val * viewRatio) + c1(quant(0.3, ukCentimeter).toRelative(ch.wImg).val),#ch.origin.x + c1(ch.height.val + quant(0.3, ukCentimeter).toRelative(ch.wImg).val),
        y: c1(0.5)),
      catSeq[j],
      alignKind = taLeft
    )
    #rect.addObj point
    #ch.children.add rect
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

proc aes(x, y: string, color = ""): Aesthetics =
  result = Aesthetics(x: x, y: y, color: color)

proc `+`(p: GgPlot, geom: Geom): GgPlot =
  ## adds the given geometry to the GgPlot object
  result = p
  result.geoms.add geom

proc `+`(p: GgPlot, facet: Facet): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.facets.add facet

proc `+`(p: GgPlot, scale: Scales): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.scales.add scale

proc `+`(p: GgPlot, aes: Aesthetics): GgPlot =
  ## adds the given aesthetics to the GgPlot object
  result = p
  result.aes.add aes

proc draw(p: GgPlot, fname: string) =
  # check if any aes
  doAssert p.aes.len > 0, "Needs at least one aesthetics!"
  # determine min and max scales of aes
  let
    minX = p.data[p.aes[0].x].mapIt(it.parseFloat).min
    maxX = p.data[p.aes[0].x].mapIt(it.parseFloat).max
    minY = p.data[p.aes[0].y].mapIt(it.parseFloat).min
    maxY = p.data[p.aes[0].y].mapIt(it.parseFloat).max
    xScale = (low: minX, high: maxX)
    yScale = (low: minY, high: maxY)

  var colorsCat: OrderedTable[string, Color]
  var colors: seq[string]
  var colorCs: seq[Color]
  if p.aes[0].color.len > 0:
    colors = p.data[p.aes[0].color]
    # convert to set to filter duplicates, back to seq and sort
    let catSeq = colors.toSet.toSeq.sorted
    colorCs = ggColorHue(catSeq.len)
    for i, k in catSet:
      colorsCat[k] = colorCs[i]

  # create the plot
  var img = initViewport(xScale = some(xScale),
                         yScale = some(yScale))

  var drawLegend = false
  if p.aes[0].color.len > 0:
    # create legend
    drawLegend = true
    img.layout(3, 3, colwidths = @[quant(2.0, ukCentimeter),
                                   quant(0.0, ukRelative),
                                   quant(5.0, ukCentimeter)],
               rowheights = @[quant(1.0, ukCentimeter),
                              quant(0.0, ukRelative),
                              quant(2.0, ukCentimeter)])
  else:
    img.layout(3, 3, colwidths = @[quant(2.0, ukCentimeter),
                                   quant(0.0, ukRelative),
                                   quant(1.0, ukCentimeter)],
               rowheights = @[quant(1.0, ukCentimeter),
                              quant(0.0, ukRelative),
                              quant(2.0, ukCentimeter)])
  # get viewport of plot
  var plt = img[4]
  plt.background()
  let
    xticks = plt.xticks(8)
    yticks = plt.yticks(5)
    xtickLabels = plt.tickLabels(xticks)
    ytickLabels = plt.tickLabels(yticks)
    xlabel = plt.xlabel(p.aes[0].x)
    ylabel = plt.ylabel(p.aes[0].y)
    grdlines = plt.initGridLines(some(xticks), some(yticks))
  let xData = p.data[p.aes[0].x].mapIt(it.parseFloat)
  let yData = p.data[p.aes[0].y].mapIt(it.parseFloat)
  var data = newSeq[GraphObject](xData.len)
  for i in 0 ..< xData.len:
    if colors.len > 0:
      #let style = Style(fillColor: colorsCat[colors[i]])
      data.add initPoint(plt, (x: xData[i], y: yData[i]),
                         marker = mkCircle,
                         color = colorsCat[colors[i]])
    else:
      data.add initPoint(plt, (x: xData[i], y: yData[i]),
                         marker = mkCircle)

  plt.addObj concat(xticks, yticks, xtickLabels, ytickLabels, @[xlabel, ylabel, grdLines], data)
  img[4] = plt

  if drawLegend:
    var lg = img[5]
    echo lg.origin
    echo lg.width
    echo lg.height
    lg.height = quant(0.5, ukRelative)
    echo "lg view height ", lg.hView
    lg.origin.y = lg.origin.y + c1(0.2)
    lg.origin.x = lg.origin.x + img.c1(0.1, akX, ukCentimeter)
    echo lg.width
    var markers: seq[GraphObject]
    for k, v in colorsCat:
      markers.add initPoint(plt,
                            (0.0, 0.0), # dummy coordinates
                            marker = mkCircle) # assign same marker as above
        #proc createLegend(view: var Viewport,
    #              title: string,
    #              markers: seq[GraphObject],
    #              cat: Table[string, Color]) =
    lg.createLegend(p.aes[0].color,
                    markers,
                    colorsCat)
    img[5] = lg
  img.draw(fname)

proc draw(fname: string): Draw = Draw(fname: fname)

proc `+`(p: GgPlot, d: Draw): GgPlot =
  p.draw(d.fname)

proc readCsv(fname: string): Table[string, seq[string]] =
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
  echo result
  while readRow(x):
    for col in items(x.headers):
      result[col].add x.rowEntry(col)

let mpg = readCsv("data/mpg.csv")

let plt = ggplot(mpg, aes(x = "displ", y = "hwy")) +
  geom_point() + draw("scatter.pdf")
let pltColor = ggplot(mpg, aes(x = "displ", y = "cty", color = "class")) +
  geom_point() + draw("scatterColor.pdf")
