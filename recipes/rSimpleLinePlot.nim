import ggplotnim, sequtils, seqmath

let x = linspace(0.0, 30.0, 1000)
let y = x.mapIt(pow(sin(it), 2.0))

let df = seqsToDf(x, y)

ggplot(df, aes("x", "y")) + # x and y are the identifiers given above as strings
  geom_line() +
  ggsave("rSimpleLinePlot.pdf")
