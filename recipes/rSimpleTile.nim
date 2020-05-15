import ggplotnim, random
var
  xs = newSeq[float]()
  ys = newSeq[float]()
  zs = newSeq[float]()
for x in 0 ..< 28:
  for y in 0 ..< 28:
    xs.add x.float
    ys.add y.float
    zs.add random(1.0)
let df = seqsToDf(xs, ys, zs)
ggplot(df, aes("xs", "ys", fill = "zs")) +
  geom_tile() +
  #scale_x_discrete() +
  #scale_y_discrete() +
  ggsave("media/recipes/rSimpleTile.png")
