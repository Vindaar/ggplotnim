import sequtils, tables, sets, algorithm, strutils
import ginger
import parsecsv, streams, strutils, hashes

import random

import math
from seqmath import histogram, shape, linspace

import macros

import persvector
export persvector

import ggplotnim / formula
export formula

import ggplotnim / ggplot_utils

export sets

import chroma
export chroma

type
  Aesthetics = object
    # In principle `x`, `y` are `Scale(scKind: scLinearData)`!
    # possibly `scTranformedData`.
    x: Option[string]
    y: Option[string]
    # Replace these by e.g. `color: Option[Scale]` and have `Scale` be variant
    # type that stores its kind `kind: scColor` and `key: string`.
    fill: Option[Scale] # classify by fill color
    color: Option[Scale] # classify by color
    size: Option[Scale] # classify by size
    shape: Option[Scale] # classify by shape

  ScaleKind = enum
    scLinearData, scTransformedData, scColor, scFillColor, scShape, scSize

  PositionKind = enum
    pkIdentity = "identity"
    pkStack = "stack"
    pkDodge = "dodge"
    pkFill = "fill"

  DiscreteKind = enum
    dcDiscrete, dcContinuous

  ScaleValue = object
    case kind: ScaleKind
    of scLinearData:
      # just stores a data value
      val: Value
    of scTransformedData:
      # data under some transformation. E.g. log, tanh, ...
      rawVal: Value
      # where `trans` is our assigned transformation function
      trans: proc(v: Value): Value
    of scFillColor, scColor:
      # stores a color
      color: Color
    of scShape:
      # a marker kind
      marker: MarkerKind
    of scSize:
      # a size of something, e.g. a marker
      size: float

  # TODO: should not one scale belong to only one axis?
  # But if we do that, how do we find the correct scale in the seq[Scale]?
  # Replace seq[Scale] by e.g. Table[string, Scale] where string is some
  # static identifier we can calculate to retrieve it?
  # e.g. `xaxis`, `<name of geom>.xaxis` etc.?
  Scale = object
    # the column which this scale corresponds to
    col: string
    scKind: ScaleKind
    case kind: DiscreteKind
    of dcDiscrete:
      # For discrete data this is a good solution. How about continuous data?
      valueMap: OrderedTable[Value, ScaleValue]
      # seq of labels to access via index
      labelSeq: seq[Value]
    of dcContinuous:
      # For continuous we might want to add a `Scale` in the ginger sense
      dataScale: ginger.Scale
      # with this we can calculate on the fly the required values given the
      # data

  Facet = object
    columns: seq[string]

  # helper object to compose `ggsave` via `+` with `ggplot`
  Draw = object
    fname: string

  GeomKind = enum
    gkPoint, gkBar, gkHistogram, gkFreqPoly, gkTile, gkLine
  Geom = object
    style: Option[Style] # if set, apply this style instead of parent's
    position: PositionKind
    aes: Aesthetics # a geom can have its own aesthetics. Needs to be part of
                    # the `Geom`, because if we add it to `GgPlot` we lose track
                    # of which geom it corresponds to
    case kind: GeomKind
    of gkHistogram, gkFreqPoly:
      numBins: int # number of bins
      binWidth: float # width of bins in terms of the data
    else:
      discard

  GgPlot[T] = object
    data: T
    title: string
    subtitle: string
    # GgPlot can only contain a single `aes` by itself. Geoms may contain
    # seperate ones
    aes: Aesthetics
    numXticks: int
    numYticks: int
    facet: Facet
    geoms: seq[Geom]

proc `==`*(s1, s2: Scale): bool =
  if s1.kind == s2.kind and
     s1.col == s2.col:
    # the other fields ``will`` be computed to the same!
    result = true
  else:
    result = false

proc hash*(x: ScaleValue): Hash =
  result = hash(x.kind.int)
  case x.kind:
  of scLinearData:
    result = result !& hash(x.val)
  of scTransformedData:
    result = result !& hash(x.rawVal)
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
  case x.kind:
  of dcDiscrete:
    for k, v in x.valueMap:
      result = result !& hash(k)
      result = result !& hash(v)
    result = result !& hash(x.labelSeq)
  of dcContinuous:
    result = result !& hash(x.dataScale)
  result = !$result

proc getValue(s: Scale, label: Value): ScaleValue =
  ## returns the `ScaleValue` of the given Scale `s` for `label`
  result = s.valueMap[label]

proc getLabelKey(s: Scale, at: int): Value =
  ## returns the key at index `at` for the Scale `s`
  result = s.labelSeq[at]

iterator enumerateLabels*(s: Scale): Value =
  for k in s.labelSeq:
    yield k

iterator enumerateLabelPairs*(s: Scale): (int, Value) =
  for i, k in s.labelSeq:
    yield (i, k)

iterator pairs*(s: Scale): (Value, ScaleValue) =
  for k, v in s.valueMap:
    yield (k, v)

iterator values*(s: Scale): ScaleValue =
  for v in values(s.valueMap):
    yield v

iterator keys*(s: Scale): Value =
  ## NOTE: for clarity use `enumerateLabels` instead!
  for k in keys(s.valueMap):
    yield k

iterator enumerateScales(p: GgPlot, geom: Geom): Scale =
  ## yields each scale that is not `none` found in the `GgPlot` or
  ## `geom`. If the same scale is found in both `GgPlot` and the `geom`
  ## the `geom` Scale overrides the `GgPlot` scale!
  let
    paes = p.aes
    gaes = geom.aes
  template genYield(field: untyped): untyped =
    if gaes.field.isSome:
      yield gaes.field.unsafeGet
    elif paes.field.isSome:
      yield paes.field.unsafeGet
  # color Scale
  genYield(color)
  # fill Scale
  genYield(fill)
  # shape Scale
  genYield(shape)
  # size Scale
  # calling the template does not work ?! Fails with `undelcared identifier: scale`?!
  # genYield(size)
  if gaes.size.isSome:
    yield gaes.size.unsafeGet
  elif paes.size.isSome:
    yield paes.size.unsafeGet

iterator enumerateScales(p: GgPlot, geom: seq[Geom]): Scale =
  ## Overload for above iterator, which allows handing `seq[Geom]`
  # TODO: avoid yielding `p` scales multiple times!
  var yieldedSet = initHashSet[Scale]()
  for g in geom:
    for scale in enumerateScales(p, g):
      if scale notin yieldedSet:
        yield scale
      else:
        yieldedSet.incl scale

proc guessType(s: seq[Value]): ValueKind =
  ## returns a ``guess`` (!) of the data type stored in `s`.
  ## We check a subset of 100 elements of the seq (or the whole if
  ## < 100 elements) and see if they match a single ValueKind.
  ## If they do match, return it, else return `VNull`
  var r = initRand(299792458) # for now just set a local state
  let idxNum = min(99, s.high)
  let randIdx = toSeq(0 .. idxNum).mapIt(r.rand(s.high))
  result = VNull
  var resultSet = false
  for i in randIdx:
    if not resultSet:
      result = s[i].kind
      resultSet = true
    else:
      if result != s[i].kind:
        return VNull

proc isDiscreteData(s: seq[Value]): bool =
  ## returns an ``estimate`` (!) of whether the given sequence of
  ## data is most likely discrete or continuous. First determine
  ## most probable type, then check for discreteness
  ## - if Values are strings: discrete
  ## - if float / int: generate set of first 100 elements, check
  ##   if cardinality of set > 50: continuous, else discrete
  ## - if bool: discrete
  let guessedT = s.guessType
  case guessedT
  of VString:
    result = true
  of VFloat, VInt:
    # same approach as in `guessType`
    var r = initRand(42) # for now just set a local state
    let idxNum = min(99, s.high)
    let randIdx = toSeq(0 .. idxNum).mapIt(r.rand(s.high))
    let elements = randIdx.mapIt(s[it]).toHashSet
    if elements.card > (idxNum.float / 2.0).round.int:
      result = false
    else:
      result = true
  of VBool:
    result = true
  of VNull:
    result = false
  of VObject:
     raise newException(Exception, "A VObject can neither be discrete nor continuous!")

proc mapDataToScale(refVals: seq[Value], val: Value, scale: Scale): ScaleValue =
  let isDiscrete = refVals.isDiscreteData
  if isDiscrete:
    case scale.scKind
    of scColor, scFillColor, scSize, scShape:
      result = scale.getValue(val)
    of scLinearData:
      # that's just the value itself
      result = ScaleValue(kind: scLinearData, val: val)
    else:
      raise newException(Exception, "`mapDataToScale` not yet implemented for " &
        "discrete data of kind " & $scale.scKind)
  else:
    case scale.scKind
    of scLinearData:
      # again, that's still just the value itself
      result = ScaleValue(kind: scLinearData, val: val)
    of scSize:
      # TODO: regardless of continuous or discrete data, bin the
      # data and determine size by binning!
      const numSizes = 5
      const minSize = 2.0
      const maxSize = 7.0
      const stepSize = (maxSize - minSize) / numSizes.float
      let dataRange = (low: min(refVals), high: max(refVals))
      # calc the actual size for contiuous data
      result = ScaleValue(kind: scSize, size: minSize + (maxSize - minSize) *
        (val.toFloat - dataRange.low.toFloat) / (dataRange.high.toFloat - dataRange.low.toFloat))
    else:
      raise newException(Exception, "`mapDataToScale` not yet implemented for " &
        "continuous data of kind " & $scale.scKind)

proc dataTo[T: Table | OrderedTable | DataFrame; U](
  df: T,
  col: string,
  outType: typedesc[U]): seq[U]

proc fillScale(scaleOpt: Option[Scale], p: GgPlot,
               scKind: static ScaleKind): Option[Scale] =
  ## fills the `Scale` of `scKind` kind of the `aes`
  if not scaleOpt.isSome:
    return none[Scale]()
  let scale = scaleOpt.unsafeGet

  var res = Scale(scKind: scKind, col: scale.col)
  # get the data column we scale by
  var data: seq[Value]
  if scale.col in p.data:
    data = p.data.dataTo(scale.col, Value)
  else:
    data = @[Value(kind: VString, str: scale.col)]

  let isDiscrete = data.isDiscreteData
  if isDiscrete:
    # generate a discrete `Scale`
    res = Scale(scKind: scKind, col: scale.col, kind: dcDiscrete)
    # convert to set to filter duplicates, back to seq and sort
    # TODO: we could also use `sequtils.deduplicate` here
    res.labelSeq = data.toHashSet.toSeq.sorted
    var valueMap = initOrderedTable[Value, ScaleValue]()
    case scKind
    of scColor, scFillColor:
      let colorCs = ggColorHue(res.labelSeq.len)
      for i, k in res.labelSeq:
        # NOTE: workaround, since we cannot do `kind: sckind` atm
        valueMap[k] = if scKind == scColor:
                        ScaleValue(kind: scColor, color: colorCs[i])
                      else:
                        ScaleValue(kind: scFillColor, color: colorCs[i])
    of scSize:
      let numSizes = min(res.labelSeq.len, 5)
      const minSize = 2.0
      const maxSize = 7.0
      let stepSize = (maxSize - minSize) / numSizes.float
      for i, k in res.labelSeq:
        valueMap[k] = ScaleValue(kind: scSize, size: minSize + i.float * stepSize)
    else:
      raise newException(Exception, "`fillScale` not implemented for " & $scKind)
    res.valueMap = valueMap
  result = some(res)

proc fillAes(p: GgPlot, aes: Aesthetics): Aesthetics =
  # TODO: we must estimate whether the given column is "continuous like" or
  # "discrete like"
  # If continuous like in case of
  # - color: create colorbar
  # - shape: raise exception not possible
  # - size: bin the data and use bins as fixed sizes
  result = aes
  result.color = aes.color.fillScale(p, scColor)
  result.fill = aes.fill.fillScale(p, scFillColor)
  # not implemented yet:
  #result.shape = aes.shape.fillScale(p, scShape)
  result.size = aes.size.fillScale(p, scSize)

proc addAes(p: var GgPlot, aes: Aesthetics) =
  ## adds the aesthetics to the plot. This is non trivial, because
  ## an aestetics encodes information that may have to be calculated
  p.aes = fillAes(p, aes)

proc orNone(s: string): Option[string] =
  ## returns either a `some(s)` if s.len > 0 or none[string]()
  if s.len == 0: none[string]()
  else: some(s)

proc orNoneScale(s: string, scKind: static ScaleKind): Option[Scale] =
  ## returns either a `some(Scale)` of kind `ScaleKind` or `none[Scale]` if
  ## `s` is empty
  if s.len > 0:
    result = some(Scale(scKind: scKind, col: s))
  else:
    result = none[Scale]()

proc aes*(x = "", y = "", color = "", fill = "", shape = "", size = ""): Aesthetics =
  result = Aesthetics(x: x.orNone, y: y.orNone,
                      color: color.orNoneScale(scColor),
                      fill: fill.orNoneScale(scFillColor),
                      shape: shape.orNoneScale(scShape),
                      size: size.orNoneScale(scSize))

proc aes*(x: FormulaNode, color = "", fill = "", shape = "", size = ""): Aesthetics =
  result = Aesthetics(x: none[string](), y: none[string](),
                      color: color.orNoneScale(scColor),
                      fill: fill.orNoneScale(scFillColor),
                      shape: shape.orNoneScale(scShape),
                      size: size.orNoneScale(scSize))

proc ggplot*[T](data: T, aes: Aesthetics = aes()): GgPlot[T] =
  result = GgPlot[T](data: data,
                     numXticks: 10,
                     numYticks: 10)
  result.addAes aes
  # TODO: fill others with defaults

func geom_point*(aes: Aesthetics = aes(),
                 color: Color = black,
                 size: float = 3.0): Geom =
  result = Geom(kind: gkPoint,
                style: some(Style(color: color,
                                  size: size)),
                aes: aes)

func geom_bar(): Geom =
  result = Geom(kind: gkBar)

func geom_line*(aes: Aesthetics = aes(),
                color: Color = grey20,
                size: float = 1.0,
                lineType: LineType = ltSolid): Geom =
  result = Geom(kind: gkLine,
                style: some(Style(color: color,
                                  lineWidth: size,
                                  lineType: lineType,
                                  fillColor: transparent)),
                aes: aes)


func geom_histogram*(aes: Aesthetics = aes(),
                     binWidth = 0.0, bins = 30,
                     color: Color = grey20, # color of the bars
                     position = "stack",
                    ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let style = Style(lineType: ltSolid,
                    lineWidth: 1.0, # draw 1 pt wide black line to avoid white pixels
                                    # between bins at size of exactly 1.0 bin width
                    color: color, # default color
                    fillColor: color)
  result = Geom(kind: gkHistogram,
                aes: aes,
                numBins: bins,

                style: some(style),
                position: pkKind)

func geom_freqpoly*(color: Color = grey20, # color of the line
                    size: float = 1.0, # line width of the line
                    lineType: LineType = ltSolid,
                    bins = 30,
                    position = "identity",
                   ): Geom =
  let pkKind = parseEnum[PositionKind](position)
  let style = Style(lineType: lineType,
                    lineWidth: size,
                    color: color,
                    fillColor: transparent,)
  result = Geom(kind: gkFreqPoly,
                style: some(style),
                numBins: bins,
                position: pkKind)

proc geom_tile*(): Geom =
  result = Geom(kind: gkTile)

proc facet_wrap*(fns: varargs[ FormulaNode]): Facet =
  result = Facet()
  for f in fns:
    doAssert f.kind == fkTerm
    doAssert f.rhs.val.kind == VString
    result.columns.add f.rhs.val.str

proc ggtitle*(title: string, subtitle = ""): (string, string) = (title, subtitle)

proc createLegend(view: var Viewport,
                  cat: Scale,
                  markers: seq[GraphObject]) =
  ## creates a full legend within the given viewport based on the categories
  ## in `cat` with a headline `title` showing data points of `markers`
  let startIdx = view.len
  case cat.kind
  of dcDiscrete:
    view.layout(1, rows = cat.valueMap.len + 1)
  of dcContinuous:
    # for now 5 sizes...
    view.layout(1, rows = 5 + 1)

  # iterate only over added children, skip first, because we actual legend first
  var j = 0
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
    # TODO: the markers given must contain all information already, that is:
    # - marker kind
    # - marker size
    # - marker color
    # then here we should just pop those in. Also need to differentiate between legends
    # made of markers and color bars!
    let point = ch.initPoint(Coord(x: c1(ch.height.val / 2.0 * viewRatio),
                                   y: c1(0.5)),
                             marker = markers[j].ptMarker,
                             size = markers[j].ptSize,
                             color = markers[j].ptColor)
    var labelText = ""
    case cat.scKind
    of scColor, scFillColor, scShape, scSize:
      labelText = $cat.getLabelKey(j)
    else:
      raise newException(Exception, "`createLegend` unsupported for " & $cat.scKind)

    let label = ch.initText(
      Coord(
        x: c1(ch.height.val * viewRatio) +
           c1(quant(0.3, ukCentimeter).toRelative(some(ch.wImg)).val),
        y: c1(0.5)),
      labelText,
      alignKind = taLeft
    )
    ch.addObj [rect, point, label]
    view[i] = ch
    inc j
  # get the first viewport for the header
  var header = view[startIdx]
  var label = header.initText(
    Coord(x: header.origin.x,
          y: c1(0.5)),
    cat.col,
    alignKind = taLeft)
  # set to bold
  label.txtFont.bold = true
  header.addObj label
  view[startIdx] = header

proc `+`*(p: GgPlot, geom: Geom): GgPlot =
  ## adds the given geometry to the GgPlot object
  result = p
  # fill the aesthetics of the geom
  var mgeom = geom
  mgeom.aes = fillAes(p, geom.aes)
  result.geoms.add mgeom

proc `+`*(p: GgPlot, facet: Facet): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.facet = facet

proc `+`*(p: GgPlot, scale: Scale): GgPlot =
  ## adds the given facet to the GgPlot object
  result = p
  result.scales.add scale

proc `+`*(p: GgPlot, aes: Aesthetics): GgPlot =
  ## adds the given aesthetics to the GgPlot object
  result = p
  result.addAes aes

proc `+`*(p: GgPlot, titleTup: (string, string)): GgPlot =
  ## adds the given title / subtitle to the GgPlot object
  result = p
  result.title = titleTup[0]
  result.subtitle = titleTup[1]

proc requiresLegend(p: GgPlot): bool =
  ## returns true if the plot requires a legend to be drawn
  if p.aes.color.isSome or
     p.aes.fill.isSome or
     p.aes.size.isSome or
     p.geoms.anyIt(it.aes.color.isSome) or
     p.geoms.anyIt(it.aes.fill.isSome) or
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
  ## NOTE: This proc may also be used as a means to extract a column from
  ## a `DataFrame` as a `seq[Value]`, although that is identical to just
  ## calling `toSeq(df[column])`.
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
    # TODO: replace by call to `guessType`
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
    elif outType is Value:
      result = toSeq(df[col])
    else:
      case dkind
      of VObject:
        doAssert false, "there cannot be a column with object type!"
      of VNull:
        raise newException(Exception, "Column " & $col & " has no data!")
      else: discard

proc changeStyle(s: Style, scVal: ScaleValue): Style =
  ## returns a modified style with the appropriate field replaced
  result = s
  case scVal.kind
  of scColor:
    result.color = scVal.color
  of scFillColor:
    # for FillColor we set both the stroke and fill color to the
    # same value
    result.color = scVal.color
    result.fillColor = scVal.color
  of scSize:
    result.size = scVal.size
  else:
    raise newException(Exception, "Setting style of " & $scVal.kind & " not " &
      "supported at the moment!")

proc createPointGobj(view: var Viewport, p: GgPlot, geom: Geom): seq[GraphObject] =
  ## creates the GraphObjects for a `gkPoint` geom
  ## TODO: we could unify the code in the `create*Gobj` procs, by
  ## - making all procs in ginger take a `Style`
  ## - just build the style in the same way we do here (in a separate proc)
  ##   and create the `GraphObject`
  doAssert geom.kind == gkPoint
  doAssert geom.style.isSome
  var style = geom.style.unsafeGet
  var marker = mkCircle
  var any = false
  for scale in enumerateScales(p, geom):
    any = true
    var data: seq[Value]
    # TODO: we do not actually make use of `data`!
    if scale.col in p.data:
      data = p.data.dataTo(scale.col, Value)
    else:
      data = toSeq(0 ..< p.data.len).mapIt(Value(kind: VString, str: scale.col))
    for label, val in scale:
      when type(p.data) is DataFrame:
        let df = p.data.filter(f{scale.col == label})
      else:
        let df = toDf(p.data).filter(f{scale.col == label})
      # now get the labeled data
      let xData = df.dataTo(p.aes.x.get, float)
      let yData = df.dataTo(p.aes.y.get, float)
      # create all data points
      for i in 0 ..< xData.len:
        case scale.scKind
        of scShape:
          # Marker is not encoded in `ginger.Style`, hence get retrieve manually
          marker = scale.getValue(label).marker
        else:
          style = changeStyle(style, val)
        result.add initPoint(view, (x: xData[i], y: yData[i]),
                             marker = marker,
                             color = style.color,
                             size = style.size)
  if not any:
    let xData = p.data.dataTo(p.aes.x.get, float)
    let yData = p.data.dataTo(p.aes.y.get, float)
    # create points needed for polyLine
    for i in 0 ..< xData.len:
      result.add initPoint(view, (x: xData[i], y: yData[i]),
                           marker = marker,
                           color = style.color,
                           size = style.size)

proc createLineGobj(view: var Viewport,
                    p: GgPlot,
                    geom: Geom): seq[GraphObject] =
  ## creates the `goPolyLine` objects for the given geom
  doAssert geom.kind == gkLine
  doAssert geom.style.isSome
  var style = geom.style.unsafeGet
  var any = false
  for scale in enumerateScales(p, geom):
    any = true
    var data: seq[Value]
    # TODO: we do not actually make use of `data`!
    if scale.col in p.data:
      data = p.data.dataTo(scale.col, Value)
    else:
      data = toSeq(0 ..< p.data.len).mapIt(Value(kind: VString, str: scale.col))
    for label, val in scale:
      when type(p.data) is DataFrame:
        let df = p.data.filter(f{scale.col == label})
      else:
        let df = toDf(p.data).filter(f{scale.col == label})
      # now get the labeled data
      let xData = df.dataTo(p.aes.x.get, float)
      let yData = df.dataTo(p.aes.y.get, float)
      # create points needed for polyLine
      var points = newSeq[Point](xData.len)
      for i in 0 ..< xData.len:
        points[i] = (x: xData[i], y: yData[i])
      style = changeStyle(style, val)
      points = points.sortedByIt(it.x)
      result.add view.initPolyLine(points, some(style))
  if not any:
    let xData = p.data.dataTo(p.aes.x.get, float)
    let yData = p.data.dataTo(p.aes.y.get, float)
    # create points needed for polyLine
    var points = newSeq[Point](xData.len)
    for i in 0 ..< xData.len:
      points[i] = (x: xData[i], y: yData[i])
    # sort the points to that the lines are not connected arbitrarily
    points = points.sortedByIt(it.x)
    result.add view.initPolyLine(points, geom.style)

proc addHistoRect[T](view: var Viewport, val: T, style: Style,
                     yPos: Coord1D = c1(1.0)) =
  ## creates a rectangle for a histogram and adds it to the viewports objects
  if val.float > 0.0:
    let r = view.initRect(Coord(x: c1(0.0),
                                y: yPos), # bottom left
                          quant(1.0, ukRelative),
                          quant(-val.float, ukData),
                          style = some(style))
    view.addObj r

proc addHistoRects(view: var Viewport,
                   data: OrderedTable[string, (seq[int], Style)],
                   yScale: ginger.Scale,
                   position: PositionKind) =
  ## Adds all rectangles for a histogram
  ## The `data` table contains both the `seq[float]` data and the `Style`
  ## that corresponds to it
  # now get the labeled data
  #let rawData = df.dataTo(p.aes.x.get, float)
  ## generate the histogram
  #var (hist, bins) = histogram(rawData, bins = nbins, range = (newXScale.low, newXScale.high))
  # make the rectangles
  var i = 0
  for p in mitems(view):
    #doAssert p.yScale.high >= hist.max.float
    case position
    of pkIdentity:
      for label, (val, style) in data:
        p.addHistoRect(val[i], style)
    of pkStack:
      # create one rectangle for each label, each successive starting at the
      # top of the previous
      var prevTop = c1(1.0)
      for label, (val, style) in data:
        p.addHistoRect(val[i], style, prevTop)
        prevTop = prevTop - Coord1D(pos: yScale.high - val[i].float, kind: ukData,
                                    scale: yScale, axis: akY)
    of pkDodge:
      discard
    of pkFill:
      discard
    inc i

proc addHistoRects(view: var Viewport,
                   hist: seq[int],
                   yScale: ginger.Scale,
                   style: Style,
                   position: PositionKind) =
  ## overload of the above working on a whole data frame. This just extracts the
  ## (label / data) pairs and hands it to `addHistoRects`
  var data = initOrderedTable[string, (seq[int], Style)]()
  data["x"] = (hist, style)
  view.addHistoRects(data, yScale, position)

proc addFreqPoly(view: var Viewport,
                 data: OrderedTable[string, (seq[int], Style)],
                 binWidth: float,
                 nbins: int,
                 position: PositionKind) =
  # only single viewport will be used
  # calculate bin centers
  let binCenters = linspace(view.xScale.low + binWidth / 2.0, view.xScale.high - binWidth / 2.0, nbins)
  # build data points for polyLine
  case position
  of pkIdentity:
    for label, (val, style) in data:
      var points = newSeq[Point](val.len)
      for i in 0 ..< nbins:
        points[i] = (x: binCenters[i], y: val[i].toFloat)
      view.addObj view.initPolyLine(points, some(style))
  of pkStack:
    var polyTab = initOrderedTable[string, seq[seq[Point]]]()
    for label in keys(data):
      polyTab[label] = newSeqWith(1, newSeq[Point]())

    # It tries to take care of drawing separate poly lines for each "unconnected" line, i.e.
    # each line disconnected by more than 1 empty bin
    # This is somewhat complicated.
    for i in 0 ..< nbins:
      var binVal = 0
      for label, (val, style) in data:
        # add the current value to the current bin value
        binVal = binVal + val[i]
        if val[i] > 0 or # has data int it, add
           binVal == 0 or # nothing in the bin yet, add
           polyTab[label][^1].len == 0 or # current polyLine is empty, add
          (polyTab[label][^1].len > 0 and i > 0 and # sanity checks
            (val[i - 1] > 0 and val[i] == 0) # this element is empty, but last
                                             # was not, so add to draw back to 0
          ):
          polyTab[label][^1].add (x: binCenters[i], y: binVal.toFloat)
        elif polyTab[label][^1].len > 0 and i != nbins - 1 and val[i + 1] == 0:
          # only create new seq, if has content and next element is 0
          polyTab[label].add newSeq[Point]()
    # now create the poly lines from the data
    for label, (val, style) in data:
      for line in polyTab[label]:
        if line.len > 0:
          view.addObj view.initPolyLine(line, some(style))
  else:
    doAssert false
  echo "View has objects ", view.objects

proc addFreqPoly(view: var Viewport,
                 hist: seq[int],
                 binWidth: float,
                 nbins: int,
                 style: Style,
                 position: PositionKind) =
  var data = initOrderedTable[string, (seq[int], Style)]()
  data["x"] = (hist, style)
  view.addFreqPoly(data, binWidth, nbins, position)

## TODO: write a helper proc to perform the data mangling somehow that
## takes care of `enumerateScales`, reading the data etc. Maybe also as a template which
## we hand some fields and types we want to read and then give a block that does
## the stuff we have to do with this? I don't know :) This way is ugly though

proc createHistFreqPolyGobj(view: var Viewport, p: GgPlot, geom: Geom): seq[GraphObject] =
  # before performing a calculation for the histogram viewports, get the
  # new xScale, by calling calcTickLocations with it
  # Note: we don't have to assign it to the `view` viewport, since that will
  # happen when calculation of the actual ticks will be done later on
  let (newXScale, _, _) = calcTickLocations(view.xScale, p.numXTicks)
  # TODO: here?
  # assign the new XScale to the view
  view.xScale = newXScale

  # generate the histogram itself
  let nbins = geom.numBins
  let binWidth = (newXScale.high - newXScale.low).float / nbins.float

  var style: Style
  if geom.style.isSome:
    style = geom.style.unsafeGet
  else:
    # TODO: inherit from parent somehow?
    discard

  # create the layout needed for the different geoms
  case geom.kind
  of gkHistogram:
    view.layout(nbins, 1)
  else:
    doAssert geom.kind == gkFreqPoly
    # we juse use the given `Viewport`

  var any = false
  var yScaleBase = (low: 0.0, high: 0.0)
  for scale in enumerateScales(p, geom):
    any = true
    var data: seq[Value]
    # TODO: we do not actually make use of `data`!
    if scale.col in p.data:
      data = p.data.dataTo(scale.col, Value)
    else:
      data = toSeq(0 ..< p.data.len).mapIt(Value(kind: VString, str: scale.col))

    # accumulate data before we do anything with our viewports, since depending on
    # the `position` of the `geom`, we might need the data for each label at the
    # same time
    var labData = initOrderedTable[string, (seq[int], Style)]()
    var numLabel = 0
    for label, val in scale:
      when type(p.data) is DataFrame:
        let df = p.data.filter(f{scale.col == label})
      else:
        let df = toDf(p.data).filter(f{scale.col == label})
      # just the regular rectangles, one behind another
      style = changeStyle(style, val)
      const epsilon = 1e-2
      # for every label, we increase the `lineWidth` by `epsilon` so that the last
      # layer completely covers the ones before it
      # TODO: this is still not a nice solution, especially regarding top and bottom
      # of the rectangles!
      if geom.kind == gkHistogram:
        style.lineWidth = style.lineWidth + numLabel.float * epsilon
      inc numLabel
      let rawData = df.dataTo(p.aes.x.get, float)
      # generate the histogram
      var (hist, bins) = histogram(rawData, bins = nbins, range = (newXScale.low, newXScale.high))
      # increase the scale if necessary
      yScaleBase = (low: 0.0, high: max(yScaleBase.high, hist.max.float))
      labData[$label] = (hist, style)

    # reverse the order of `labData`, so that the element class with highest string
    # value is located at the bottom of the histogram (to match `ggplot2`)
    labData.sort(
      cmp = (
        proc(a, b: (string, (seq[int], Style))): int =
          result = system.cmp(a[0], b[0])
      ),
      order = SortOrder.Descending)

    # calculate the new Y scale maximum of the plot
    var newYMax = yScaleBase.high
    case geom.position
    of pkStack:
      # for `pkStack` we must find the bin with the largest sum of
      # label elements
      var sums = newSeq[int](nbins)
      for i in 0 ..< nbins:
        var s = 0
        for label in keys(labData):
          s += labData[label][0][i]
        sums[i] = s
      newYMax = max(sums).float
    of pkFill:
      # TODO: `pkFill` is basically the same as `pkStack`, only that the sum of
      # any non empty bin must add up to 1.0
      raise newException(Exception, "Not yet implemented!")
    else: discard
    # with the data available, create the histogram rectangles
    # fix the data scales on the children viewports
    yScaleBase = (low: 0.0, high: newYMax)
    let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)
    case geom.kind
    of gkHistogram:
      view.addHistoRects(labData, newYScale, geom.position)
    of gkFreqPoly:
      view.yScale = newYScale
      view.addFreqPoly(labData, binWidth, nbins, geom.position)
    else:
      doAssert false
  if not any:
    let rawData = p.data.dataTo(p.aes.x.get, float)
    # generate the histogram
    var (hist, _) = histogram(rawData, bins = nbins, range = (newXScale.low, newXScale.high))
    # set the y scale
    yScaleBase = (low: 0.0, high: hist.max.float)
    # fix the data scales on the children viewports
    let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)
    case geom.kind
    of gkHistogram:
      view.addHistoRects(hist, newYScale, style, geom.position)
    of gkFreqPoly:
      view.yScale = newYScale
      view.addFreqPoly(hist, binWidth, nbins, style, geom.position)
    else:
      doAssert false

  # fix the data scales on the children viewports
  let (newYScale, _, _) = calcTickLocations(yScaleBase, p.numYTicks)
  view.yScale = newYScale
  for ch in mitems(view):
    ch.yScale = newYScale

proc createGobjFromGeom(view: var Viewport, p: GgPlot, geom: Geom): seq[GraphObject] =
  ## performs the required conversion of the data from the data
  ## frame according to the given `geom`
  case geom.kind
  of gkPoint:
    result = view.createPointGobj(p, geom)
  of gkHistogram, gkFreqPoly:
    result = view.createHistFreqPolyGobj(p, geom)
  of gkLine:
    result = view.createLineGobj(p, geom)
  else:
    discard

proc generateLegendMarkers(plt: Viewport, scale: Scale): seq[GraphObject] =
  ## generate the required Legend Markers for the given `aes`
  ## TODO: add different objects to be shown depending on the scale and geom.
  ## E.g. in case of `fill` fill the whole rectangle with the color. In case
  ## of geom_line only draw a line etc.
  ## Thus also put the rectangle drawing here.
  # TODO: rewrite this either via a template, proc or macro!
  case scale.sckind
  of scColor:
    for i in 0 ..< scale.valueMap.len:
      let color = scale.getValue(scale.getLabelKey(i)).color
      result.add initPoint(plt,
                           (0.0, 0.0), # dummy coordinates
                           marker = mkCircle,
                           color = color) # assign same marker as above
  of scFillColor:
    for i in 0 ..< scale.valueMap.len:
      let color = scale.getValue(scale.getLabelKey(i)).color
      result.add initPoint(plt,
                           (0.0, 0.0), # dummy coordinates
                           marker = mkCircle,
                           color = color) # assign same marker as above
  of scShape:
    for i in 0 ..< scale.valueMap.len:
      result.add initPoint(plt,
                           (0.0, 0.0), # dummy coordinates
                           marker = scale.getValue(scale.getLabelKey(i)).marker)
  of scSize:
   for i in 0 ..< scale.valueMap.len:
     let size = scale.getValue(scale.getLabelKey(i)).size
     result.add initPoint(plt,
                          (0.0, 0.0), # dummy coordinates
                          marker = mkCircle,
                          size = size)
  else:
    raise newException(Exception, "`createLegend` unsupported for " & $scale.scKind)


proc ggsave*(p: GgPlot, fname: string) =
  var
    xScale: ginger.Scale
    yScale: ginger.Scale
    colorsCat: OrderedTable[string, Color]
    colors: seq[string]

  # TODO: this probably doesn't have to happen here!
  let xdata = p.data.dataTo(p.aes.x.get, float)
  # TODO: Check if `xdata.isDiscreteData` and handle discrete cases (possibly
  # also `string` data, after reading `xdata` not into `float`, but into `Value`.
  # TODO2: For latter we must make sure that reading `xdata` as `Value` actually
  # gives us `VFloat` values, instead of `VString` with floats as string
  # For a `DataFrame` this should work, but for a `Table` it won't (as it's
  # `seq[string]` internally).
  let
    minX = xdata.min
    maxX = xdata.max
  xScale = (low: minX, high: maxX)
  var
    minY: float
    maxY: float
  if p.aes.y.isSome:
    let ydata = p.data.dataTo(p.aes.y.get, float)
    minY = ydata.min
    maxY = ydata.max
    yScale = (low: minY, high: maxY)

  # create the plot
  var img = initViewport(xScale = some(xScale),
                         yScale = some(yScale))

  # NOTE: the question if ``not`` whether a `PLOT` requires a legend
  # but rather whether an `AESTHETIC` with an attached `SCALE` requires
  # legend! So check for each `aes` whether we have to draw a legend!
  # Assembly of the plot layout thus has to happen at the very end
  let drawLegend = p.requiresLegend
  if drawLegend:
    img.plotLayoutWithLegend()
  else:
    img.plotLayoutWithoutLegend()

  # get viewport of plot
  var pltBase = img[4]

  # first perform faceting by creating subgroups
  var mplt = p
  mplt.data = p.data.group_by(p.facet.columns)
  echo mplt
  var pltSeq: seq[Viewport]
  for (pair, df) in groups(mplt.data):
    mplt = p
    mplt.data = df
    # first write all plots into dummy viewport
    var plt = pltBase

    plt.background()
    for geom in mplt.geoms:
      # for each geom, we create a child viewport of `plt` covering
      # the whole viewport, which will house the data we just created.
      # Due to being a child, if will be drawn *after* its parent. This way things like
      # ticks will be below the data.
      # On the other hand this allows us to draw several geoms in on a plot and have the
      # order of the function calls `geom_*` be preserved
      var pChild = plt.addViewport()

      let data = pChild.createGobjFromGeom(mplt, geom)

      # add the data to the child
      pChild.addObj data
      # add the data viewport to the plt
      plt.children.add pChild

      # for these types there is no y data scale attached to the image so far,
      # thus we assign `pChild`'s data scale to it
      # TODO: solve this more elegantly!

      # potentially the creation of the graph objects have altered the scales
      # of the viewport. We have to make sure that the parents receive an updated
      # scale too
      plt.yScale = pChild.yScale
      img.yScale = pChild.yScale

    let
      xticks = plt.xticks(mplt.numXTicks)
      yticks = plt.yticks(mplt.numYTicks)
      xtickLabels = plt.tickLabels(xticks)
      ytickLabels = plt.tickLabels(yticks)
      xlabel = plt.xlabel(mplt.aes.x.get)
      grdlines = plt.initGridLines(some(xticks), some(yticks))
    var ylabel: GraphObject
    case mplt.geoms[0].kind
    of gkPoint:
      ylabel = plt.ylabel(mplt.aes.y.get)
    of gkHistogram:
      ylabel = plt.ylabel("count")
    else: discard
    plt.addObj concat(xticks, yticks, xtickLabels, ytickLabels, @[xlabel, ylabel, grdLines])

    # now add dummy plt to pltSeq
    pltSeq.add plt

  # now create layout in `pltBase`, the actual canvas for all plots
  let (rows, cols) = calcRowsColumns(0, 0, pltSeq.len)
  pltBase.layout(cols, rows)
  for i, plt in pltSeq:
    pltBase.children[i].objects = plt.objects
    pltBase.children[i].children = plt.children
  img[4] = pltBase


  # draw legends
  for scale in enumerateScales(p, mplt.geoms):
    # handle color legend
    var lg = img[5]
    let markers = lg.generateLegendMarkers(scale)
    # TODO: The following currently creates stacked legends for each Scale that
    # requires one. Need to create a `seq[Viewport]` or something to first build
    # all legends and then calculate the sizes required.
    # set height to number of markers + 1 centimeter
    lg.height = quant((markers.len + 1).float, ukCentimeter)
    lg.origin.y = lg.origin.y + c1(img[4].height.val / 8.0)
    lg.origin.x = lg.origin.x + img.c1(0.5, akX, ukCentimeter)
    lg.createLegend(scale, markers)
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

proc ggsave*(fname: string): Draw = Draw(fname: fname)

proc `+`*(p: GgPlot, d: Draw) =
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
    result[col.strip] = @[]
  while readRow(x):
    for col in items(x.headers):
      result[col.strip].add x.rowEntry(col).strip


when isMainModule:
  let mpg = toDf(readCsv("data/mpg.csv"))
  let plt = ggplot(mpg, aes(x = "displ", y = "hwy")) +
    geom_point()
  plt.ggsave("scatter.pdf")

  mpg.filter(f{"class" == "suv"}) # comparison via `f{}` macro
    .mutate(ratioHwyToCity ~ hwy / cty # raw untyped template function definition
    ) # <- note that we have to use normal UFCS to hand to `ggplot`!
    .ggplot(aes(x = "ratioHwyToCity", y = "displ", color = "class")) +
    geom_point() +
    ggsave("scatterFromDf.pdf")

  #let re = df.mutate(f{"cty_norm" ~ "cty" / abs("hwy")})
            # f{"displ_ccm" ~ "displ" * "1000.0"}, # displacement in ccm
            # unfortunately this is not yet possible. The `FormulaNode.fkVariable`
            # needs to be converted to type Value before we can do that

  let f = f{"cty_norm" ~ "cty" / mean("cty") * 2.0}
  echo "Func ", f

  let val = 1000
  let key = "cty"
  mpg.mutate(f{"cty_norm" ~ "cty" / mean(key) * val})
            # f{"displ_ccm" ~ "displ" * "1000.0"}, # displacement in ccm
            # unfortunately this is not yet possible. The `FormulaNode.fkVariable`
            # needs to be converted to type Value before we can do that
    .ggplot(aes(x = "displ", y = "cty_norm", color = "class")) +
    geom_point() +
    ggsave("classVsNormCty.pdf")

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
