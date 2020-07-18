import ggplotnim, random
var
  xs = newSeq[float]()
  ys = newSeq[float]()
  zs = newSeq[float]()
for x in countup(-256, 254, 2):
  for y in 0 ..< 256:
    xs.add x.float
    ys.add y.float
    zs.add rand(1.0)
let df = seqsToDf(xs, ys, zs)
ggplot(df, aes("xs", "ys", fill = "zs")) +
  geom_raster() +
  ggsave("media/recipes/rSimpleRaster.png")
