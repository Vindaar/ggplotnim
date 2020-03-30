import ggplotnim, random, math

let df = toDf(readCsv("data/mpg.csv"))
let dfRed = df.group_by(["class", "cyl"]).summarize(f{float: "meanHwy" << mean( c"hwy" )})
# stringification of formula is default name
let meanHwyCol = "meanHwy"
ggplot(dfRed, aes("class", "cyl", fill = meanHwyCol)) +
  geom_tile() +
  geom_text(aes(text = meanHwyCol)) +
  scale_y_continuous(dcKind = dcDiscrete) +
  ggsave("media/recipes/rAnnotatedHeatmap.png")
