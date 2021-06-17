# first start with auto selection of colors
import ggplotnim, sequtils
const numx = 50
const numy = 8
const lineSizes = [0.4, 0.3, 0.2, 0.8, 0.5, 0.6, 0.7, 0.9]
# NOTE: The creation of the data here could surely be done in a nicer
# way...
var spikes = newSeq[float]()
var sizes = newSeq[float]()
for y in 0 ..< numy:
  for x in 0 ..< numx:
    spikes.add y.float
    sizes.add lineSizes[y]
var df = newDataFrame()
df["spikes"] = toColumn spikes
df["neurons"] = toColumn randomTensor(numx * numy, 1.0)
df["lineSize"] = toColumn sizes

ggplot(df, aes("neurons", "spikes", color = factor("lineSize"))) +
  geom_linerange(aes(ymin = f{c"spikes" - c"lineSize" / 2.0},
                     ymax = f{c"spikes" + c"lineSize" / 2.0})) +
  scale_y_continuous() + # make sure y is considered cont.
  ylim(-1, 8) + # at the moment ymin, ymax are not considered for the plot range (that's a bug)
  ggtitle("Spike raster plot") +
  theme_opaque() +
  ggsave("media/recipes/rAutoColoredNeuralSpikes.png")
