import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("cty", color = "class")) + 
  geom_freqpoly() + 
  ggsave("media/recipes/rStackedMpgFreqpoly.png")
