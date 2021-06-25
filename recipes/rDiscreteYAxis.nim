import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy", "cyl")) +
  geom_point() +
  ggsave("media/recipes/rDiscreteYAxis.png")
