import ggplotnim
let df = readCsv("data/diamonds.csv")
ggplot(df, aes("carat")) + 
  geom_histogram(density = true) + 
  ggsave("media/recipes/rHistogramDensity.png")
