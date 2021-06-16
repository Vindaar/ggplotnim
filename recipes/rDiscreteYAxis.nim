import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy", "cyl")) +
  geom_point() +
  theme_opaque() +
  ggsave("media/recipes/rDiscreteYAxis.png")
