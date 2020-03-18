import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("hwy", "cty")) +
  geom_point() +
  xlim(10.0, 30.0) +
  ggsave("media/recipes/rLimitXRange.png")
