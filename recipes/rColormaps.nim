import ggplotnim, sequtils

# 1000 points to use
let xs = linspace(0.0, 1.0, 1000)
var plts: seq[GgPlot]
# generate data to show a gradient for (currently a single element in one
# axis isn't supported for `geom_raster`, as it's essentially discrete).
var df = newDataFrame()
for i in 0 ..< 5:
  df.add toDf({"x" : xs, "y" : i })
  
for scale in [viridis(), magma(), plasma(), inferno()]:
  plts.add ggplot(df, aes("x", "y", fill = "x")) +
    scale_fill_gradient(scale) + # assign the correct scale
    geom_raster() +
    ylim(0, 5) +   # due to our weird data (height deduced as 1, set correct limits)
    theme_void() + # no scales
    hideLegend() + # no legend
    margin(top = 1, bottom = 0.3, right = 0.3) + # set small margin left / bottom 
    ggtitle("Colorscale: " & $scale.name) 
# now create a multi plot of all of them
ggmulti(plts, "media/recipes/rColormaps.png", widths = @[600], heights = @[150, 150, 150, 150],
        width = 600, height = 600)
