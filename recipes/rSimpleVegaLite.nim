import ggplotnim
import ggplotnim/ggplot_vega
let mpg = readCsv("data/mpg.csv")
ggplot(mpg, aes(x = "displ", y = "cty", color = "class")) +
  geom_point() +
  ggtitle("ggplotnim in Vega-Lite!") +
  ggvega("media/recipes/rSimpleVegaLite.html") # w/o arg creates a `/tmp/vega_lite_plot.html`
