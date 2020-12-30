import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("hwy", "cyl")) +
  geom_point() +
  theme_opaque() +
  ggsave("media/recipes/rDiscreteYAxis.png")
