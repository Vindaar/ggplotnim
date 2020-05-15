import ggplotnim, strutils, sequtils, seqmath

let x = linspace(0.0, 30.0, 1000)
let y = x.mapIt(pow(sin(it), 2.0))

let df = seqsToDf(x, y)

ggplot(df, aes("x", "y")) + 
  geom_line() +
  scale_x_continuous(labels = proc(x: float): string =
                              x.formatFloat(ffDecimal, 2)) +
  xlab(label = " ") +
  ggsave("media/recipes/rFormatDecimalsPlot.png")
