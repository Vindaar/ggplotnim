import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("class", "cyl", color = "class")) +
  geom_point() +
  theme_opaque() +
  ggsave("media/recipes/rBothDiscreteAxes.png")
