import ggplotnim, sequtils, seqmath

let x = linspace(0.0, 30.0, 1000)
let y = x.mapIt(pow(sin(it), 2.0))
let df = seqsToDf(x, y)

ggplot(df, aes("x", "y")) + # x and y are the identifiers given above as strings
  geom_line() +
  scale_x_continuous(breaks = @[0.0, 1.0, 2.0, 12.0]) + # set custom ticks along x
  scale_y_continuous(breaks = 50) + # set a custom number of ticks along y
  ggsave("media/recipes/rCustomBreaks.png")
