import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("class", "cyl", color = "class")) +
  geom_point() +
  ggsave("media/recipes/rBothDiscreteAxes.png")
