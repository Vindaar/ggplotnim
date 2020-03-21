import ggplot_types, formula, ggplot_scales
import ginger except Scale

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
  # now apply `alpha` to `fillColor`
  if uStyle.alpha.isSome:
    result.fillColor.a = uStyle.alpha.unsafeGet
  elif s.alpha.isSome:
    result.fillColor.a = s.alpha.unsafeGet

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
  else:
    raise newException(Exception, "Setting style of " & $scVal.kind & " not " &
      "supported at the moment!")

proc applyStyle*[T: string | FormulaNode](style: var GgStyle, df: DataFrame, scales: seq[Scale],
                                          keys: seq[(T, Value)]) =
  var styleVal: ScaleValue
  for (col, val) in keys:
    for s in scales:
      # walk all scales and build the correct style
      if s.scKind in {scLinearData, scTransformedData}: continue
      case s.dcKind
      of dcDiscrete:
        when T is string:
          let isCol = col in df
        else:
          let isCol = col.isColumn(df)
        if not isCol:
          # constant value
          styleVal = s.getValue(evaluate(s.col))
        else:
          styleVal = s.getValue(val)
        style = changeStyle(style, styleVal)
      else:

        discard
