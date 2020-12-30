import ggplotnim, sequtils
const numx = 50
const numy = 8
const lineSizes = [0.4, 0.3, 0.2, 0.8, 0.5, 0.6, 0.7, 0.9]
# alternatively using fixed colors and one geom_linerange for each color
let colorCodes = @[color(0, 0, 0),
                   color(1, 0, 0),
                   color(0, 1, 0),
                   color(0, 0, 1),
                   color(1 , 1, 0),
                   color(1, 0, 1),
                   color(0, 1, 1),
                   color(1, 0, 1)]
var df = newDataFrame()
for nr in 0 ..< numy:
  df["neuron " & $nr] = toColumn randomTensor(numx, 1.0)
var plt = ggplot(df)
for nr in 0 ..< numy:
  # could combine with above loop, but for clarity
  plt = plt + geom_linerange(aes(x = ("neuron " & $nr),
                                 y = nr,
                                 ymin = nr.float - lineSizes[nr] / 2.0,
                                 ymax = nr.float + lineSizes[nr] / 2.0),
                             color = some(colorCodes[nr]))
# finally add scales, title and plot
plt + scale_y_continuous() + # make sure y is considered cont.
  ylim(-1, 8) + # at the moment ymin, ymax are not considered for the plot range (that's a bug)
  xlab("Neurons") +
  ylab("Spikes") +
  ggtitle("Spike raster plot, manual colors") +
  theme_opaque() +
  ggsave("media/recipes/rCustomColoredNeuralSpikes.png")
