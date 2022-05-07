import ggplotnim, random
var
  xs = newSeq[float]()
  ys = newSeq[float]()
  zs = newSeq[float]()
  rnd = initRand(42)
for x in 0 ..< 28:
  for y in 0 ..< 28:
    xs.add x.float
    ys.add y.float
    zs.add rnd.rand(1.0)
let df = toDf(xs, ys, zs)
ggplot(df, aes("xs", "ys", fill = "zs")) +
  geom_tile() +
  #scale_x_discrete() +
  #scale_y_discrete() +
  ggsave("media/recipes/rSimpleTile.png")
