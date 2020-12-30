import ggplotnim, sequtils, seqmath
const
  width = 720
  height = 480
let x = linspace(0.0, 30.0, 1000)
let y = x.mapIt(pow(sin(it), 2.0))
let df = seqsToDf(x, y)
ggplot(df, aes("x", "y")) +
  geom_line() +
  theme_opaque() +
  ggsave("media/recipes/rLinePlotSize.png", width = width, height = height)
