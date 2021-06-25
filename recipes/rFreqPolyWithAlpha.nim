import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("cty", fill = "class")) + 
  geom_freqpoly(alpha = some(0.3)) + 
  ggsave("media/recipes/rFreqPolyWithAlpha.png")
