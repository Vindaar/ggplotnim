import ggplot_types, ggplot_scales, ggplot_utils
import ginger except Scale
when defined(defaultBackend):
  import dataframe/fallback/formula
else:
  import dataframe/arraymancer_backend
#[
Contains procs dealing with styles and defines defaults for
different geoms.
]#

## The default styles we use for each geom in case neither the user provides
## a setting nor a mapping
const PointDefaultStyle = Style(size: 3.0,
                                marker: mkCircle,
                                color: black,
                                fillColor: black)
const LineDefaultStyle = Style(lineWidth: 1.0,
                               lineType: ltSolid,
                               size: 5.0, # used to draw error bar 'T' horizontal
                               color: grey20,
                               fillColor: transparent)
const BarDefaultStyle = Style(lineWidth: 1.0,
                              lineType: ltSolid,
                              color: grey20,
                              fillColor: grey20)
const HistoDefaultStyle = Style(lineWidth: 0.2,
                                lineType: ltSolid,
                                color: grey20,
                                fillColor: grey20)
const TileDefaultStyle = Style(lineWidth: 0.05,
                               lineType: ltSolid,
                               color: grey20,
                               fillColor: grey20)
const TextDefaultStyle = Style(font: font(12.0),
                               size: 12.0,
                               color: black)

func defaultStyle(geomKind: GeomKind): Style =
  case geomKind
  of gkPoint:
    result = PointDefaultStyle
  of gkLine, gkFreqPoly, gkErrorBar:
    result = LineDefaultStyle
  of gkBar:
    result = BarDefaultStyle
  of gkHistogram:
    result = HistoDefaultStyle
  of gkTile:
    result = TileDefaultStyle
  of gkText:
    result = TextDefaultStyle
  of gkRaster:
    # raster doesn't have default style atm (what would that imply?)
    discard

func mergeUserStyle*(s: GgStyle, uStyle: GgStyle, geomKind: GeomKind): Style =
  ## merges the given `Style` with the desired `userStyle`.
  # Have to differentiate between 3 cases of priority:
  # 1. `uStyle`
  # 2. `s`
  # 3. default style for a given geom
  template fillField(field: untyped): untyped =
    if uStyle.field.isSome:
      result.field = uStyle.field.unsafeGet
    elif s.field.isSome:
      result.field = s.field.unsafeGet
    else:
      result.field = defaultStyle(geomKind).field
  fillField(color)
  fillField(size)
  fillField(lineType)
  fillField(lineWidth)
  fillField(fillColor)
  fillField(marker)
  fillField(errorBarKind)
  fillField(font)
  # now apply `alpha` to `fillColor`
  if uStyle.alpha.isSome:
    result.fillColor.a = uStyle.alpha.unsafeGet
    ## TODO: should we apply this to other geoms as well? This would lead to also
    ## making the outlines of fills change alpha. Might be wanted, might not be wanted.
    ## Or add a `fillAlpha` option? How does ggplot2 handle this?
    if geomKind in {gkPoint, gkLine, gkErrorBar, gkText}:
      result.color.a = uStyle.alpha.unsafeGet
  elif s.alpha.isSome:
    result.fillColor.a = s.alpha.unsafeGet
    if geomKind in {gkPoint, gkLine, gkErrorBar, gkText}:
      result.color.a = uStyle.alpha.unsafeGet

  # apply `color`, `size` to font
  ## TODO: This will overwrite a user style!!
  if result.color != result.fillColor:
    result.font.color = result.color
  let defSize = defaultStyle(geomKind).size
  if result.size != defSize:
    result.font.size = result.size * 2.5

proc changeStyle*(s: GgStyle, scVal: ScaleValue): GgStyle =
  ## returns a modified style with the appropriate field replaced
  result = s
  case scVal.kind
  of scColor:
    result.color = some(scVal.color)
  of scFillColor:
    # for FillColor we set both the stroke and fill color to the
    # same value
    result.color = some(scVal.color)
    result.fillColor = some(scVal.color)
  of scSize:
    result.size = some(scVal.size)
  of scShape:
    result.marker = some(scVal.marker)
    result.lineType = some(scVal.lineType)
  else:
    raise newException(Exception, "Setting style of " & $scVal.kind & " not " &
      "supported at the moment!")

proc applyStyle*[T: string | FormulaNode](style: var GgStyle, df: DataFrame, scales: seq[Scale],
                                          keys: seq[(T, Value)]) =
  var styleVal: ScaleValue
  for (col, val) in keys:
    for s in scales:
      # walk all scales and build the correct style
      if s.scKind in {scLinearData, scTransformedData, scText}: continue
      case s.dcKind
      of dcDiscrete:
        when T is string:
          let isCol = col in df
        else:
          let isCol = col.isColumn(df)
        if not isCol:
          # constant value
          styleVal = s.getValue(evaluate(s.col))
        elif $col == $s.col:
          # else only get value if this `col` is the scales column!
          styleVal = if val.kind == VNull: s.getValue(%~ $col) else: s.getValue(val)
        else: continue
        style = changeStyle(style, styleVal)
      else:

        discard
