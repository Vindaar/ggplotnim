import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy", "cty")) +
  geom_point() +
  xlim(10.0, 60.0) +
  ggsave("media/recipes/rEnlargeXRange.png")
