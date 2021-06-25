import ggplotnim, random
var
  xs = newSeq[float]()
  ys = newSeq[float]()
  zs = newSeq[float]()
  rnd = initRand(42)
for x in countup(-256, 254, 2):
  for y in 0 ..< 256:
    xs.add x.float
    ys.add y.float
    zs.add rnd.rand(1.0)
let df = seqsToDf(xs, ys, zs)
ggplot(df, aes("xs", "ys", fill = "zs")) +
  geom_raster() +
  ggsave("media/recipes/rSimpleRaster.png")
