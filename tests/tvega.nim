import unittest
import ./ggplotnim
import ./ggplotnim/ggplot_vega

suite "Vega-Lite backend":
  let mpg = readCsv("data/mpg.csv")
  let plt = ggplot(mpg, aes(x = "displ", y = "cty", color = "class")) +
    geom_point() +
    ggtitle("ggplotnim in Vega-Lite!")

  test "Raises on unsupported file types":
    try:
      plt + ggvega("test.foo", show = false)
    except VegaError:
      discard
