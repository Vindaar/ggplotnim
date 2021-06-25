import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("cty", fill = "class")) + 
  geom_histogram() + 
  ggsave("media/recipes/rStackedMpgHistogram.png")
