import ggplotnim
import ggplotnim/vega_utils

import unittest, json

import monocle

suite "Vega-lite backend tests":
  test "vega-scatter plot":
    let expected = parseJson("""
{
  "$schema": "https://vega.github.io/schema/vega-lite/v4.json",
  "description" : "Vega-lite plot created by ggplotnim",
  "width" : 640,
  "height" : 480,
  "title": "ggplotnim - or I Suck At Naming Things",
  "data": {"values" : []},
  "mark": "point",
  "encoding": {
    "x": {"field": "displ", "type": "quantitative"},
    "y": {"field": "cty", "type": "quantitative"},
    "color": {"field": "class", "type": "nominal"}
   }
}
""")
    let expDataEx = parseJson("""[
{"displ": 1.8, "cty": 18.0, "class": "compact"},
{"displ": 1.8, "cty": 21.0, "class": "compact"},
{"displ": 2.0, "cty": 20.0, "class": "compact"} ]
""")
    let mpg = toDf(readCsv("data/mpg.csv"))
    let vegaJson = ggplot(mpg, aes(x = "displ", y = "cty", color = "class")) +
      geom_point() +
      ggtitle("ggplotnim - or I Suck At Naming Things™") +
      ggvega()
    check vegaJson["$schema"] == expected["$schema"]
    check vegaJson["description"] == expected["description"]
    # check vegaJson["data"] == expected["data"]
    check vegaJson["mark"] == expected["mark"]
    check vegaJson["encoding"] == expected["encoding"]
    check vegaJson["data"].kind == expected["data"].kind
    for i in 0 .. 2:
      check vegaJson["data"]["values"][i] == expDataEx[i]
    check vegaJson["data"]["values"].len == 234
    check vegaJson.len == expected.len
    show(vegaJson)
