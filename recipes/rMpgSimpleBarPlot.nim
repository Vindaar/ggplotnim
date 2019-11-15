import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("class")) + 
  geom_bar() + 
  ggsave("media/recipes/rMpgSimpleBarPlot.png")
