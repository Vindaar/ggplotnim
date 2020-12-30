import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("hwy", "cty")) +
  geom_point() +
  xlim(10.0, 60.0) +
  theme_opaque() +
  ggsave("media/recipes/rEnlargeXRange.png")
