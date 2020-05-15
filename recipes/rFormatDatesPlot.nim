import ggplotnim, strutils, sequtils, seqmath, times

let x = linspace(0.0, 30.0, 1000)
let y = x.mapIt(pow(sin(it), 2.0))

let df = seqsToDf(x, y)

ggplot(df, aes("x", "y")) +
  geom_line() +
  scale_y_continuous(labels = proc(x: float): string =
                              x.formatFloat(ffDecimal, 1)) +
  scale_x_continuous(labels = proc(x: float): string =
                              getDateStr(now() - int(x).months)) +
  xlab(label = " ") +
  ggsave("media/recipes/rFormatDatesPlot.png")
