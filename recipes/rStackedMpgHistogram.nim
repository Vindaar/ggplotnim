import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("cty", fill = "class")) + 
  geom_histogram() + 
  ggsave("media/recipes/rStackedMpgHistogram.png")
