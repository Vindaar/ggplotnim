import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
let breaks = @[0'f64, 10, 15, 19, 23, 25, 40]
ggplot(df, aes("cty")) +
  geom_histogram(breaks = breaks) +
  geom_point(stat="bin", breaks = breaks, binPosition = "center") +
  theme_opaque() +
  ggsave("media/recipes/rMpgHistoPlusPoints.png")
