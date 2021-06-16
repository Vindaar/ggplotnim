import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("cty", color = "class")) + 
  geom_freqpoly() + 
  theme_opaque() +
  ggsave("media/recipes/rStackedMpgFreqpoly.png")
