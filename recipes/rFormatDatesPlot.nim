import ggplotnim, strutils, sequtils, seqmath, times

let x = linspace(0.0, 30.0, 1000)
let y = x.mapIt(pow(sin(it), 2.0))

let df = seqsToDf(x, y)

# helper template to get a reproducible `DateTime` for CI!
template nowTmpl(): untyped = initDateTime(15, mMay, 2020, 00, 00, 00, 00, utc())

ggplot(df, aes("x", "y")) +
  geom_line() +
  scale_y_continuous(labels = proc(x: float): string =
                              x.formatFloat(ffDecimal, 1)) +
  scale_x_continuous(labels = proc(x: float): string =
                              getDateStr(nowTmpl() - int(x).months)) +
  xlab(label = " ") +
  ggsave("media/recipes/rFormatDatesPlot.png")
