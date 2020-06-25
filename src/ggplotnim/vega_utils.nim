import json, tables, options, strutils
import ggplot_types
when defined(defaultBackend):
  import dataframe/fallback/formula
  import persvector
else:
  import dataframe/arraymancer_backend

const vegaLiteTmpl = """
{
  "$schema": "https://vega.github.io/schema/vega-lite/v4.json",
  "description" : "Vega-lite plot created by ggplotnim"
}
"""

let mapping = { "title" : "title" }.toTable
let geom_mapping = { "point" : "point" }.toTable

template toVegaField(f: string): string =
  # TODO: for some reason having `f` untyped and  using `astToStr`
  # on `f` does not work if `ggplot_types` is imported?!
  mapping[f]

proc mapGeomKind(gk: GeomKind): string =
  result = geom_mapping[($gk).replace("gk", "").toLowerAscii]

proc toJson(v: Value): JsonNode =
  case v.kind
  of VString:
    result = % v.str
  of VInt:
    result = % v.num
  of VFloat:
    result = % v.fnum
  of VBool:
    result = % v.bval
  of VObject:
    result = % v.fields
  of VNull:
    result = newJNull()

proc dfToJson(df: DataFrame): JsonNode =
  result = newJArray()
  for row in df:
    var rowJson = newJObject()
    for k, v in row:
      rowJson[k] = v.toJson
    result.add rowJson

proc collectCols(p: GgPlot): seq[string] =
  ## returns all required columns for the plot
  # iterate all aesthetics and extract columns that are "some"
  # TODO: replace by actual impl
  result = @[$p.aes.x.get.col, $p.aes.y.get.col, $p.aes.color.get.col]

proc dataAsJson(p: GgPlot): JsonNode =
  let cols = collectCols(p)
  let df = p.data.select(cols)
  echo df
  result = df.dfToJson

proc toVegaLite*(p: GgPlot): JsonNode =
  ## converts a `GgPlot` object to a vega-lite conform JsonNode
  # start with the template
  result = parseJson(vegaLiteTmpl)
  result[toVegaField("title")] = % p.title
  result["mark"] = % mapGeomKind(p.geoms[0].kind)
  result["width"] = % 640#p.wImg
  result["height"] = % 480#p.wHeight
  var encoding = newJObject()
  # iterate aes / scales here
  if p.aes.x.isSome:
    encoding["x"] = %* { "field" : $p.aes.x.get.col,
                         "type" : "quantitative" }
  if p.aes.y.isSome:
    encoding["y"] = %* { "field" : $p.aes.y.get.col,
                         "type" : "quantitative" }
  if p.aes.color.isSome:
    encoding["color"] = %* { "field" : $p.aes.color.get.col,
                             "type" : "nominal" }
  result["encoding"] = encoding
  result["data"] = %* {"values" : % p.dataAsJson}
