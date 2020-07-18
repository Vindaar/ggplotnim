import ggplotnim, random
var
  xs = newSeq[float]()
  ys = newSeq[float]()
  zs1 = newSeq[float]()
  zs2 = newSeq[float]()
for x in 0 ..< 256:
  for y in 0 ..< 256:
    xs.add x.float
    ys.add y.float
    zs1.add rand(1.0)
    zs2.add rand(1.0)
let df = seqsToDf(xs, ys, zs1, zs2)
  .gather(["zs1", "zs2"], key = "Map", value = "vals")
ggplot(df, aes("xs", "ys", fill = "vals")) +
  facet_wrap("Map") +
  xlim(0, 256) + ylim(0, 256) +
  geom_raster() +
  ggsave("media/recipes/rFacetRaster.png", width = 920)
