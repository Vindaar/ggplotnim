import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("class", fill = "drv")) + 
  geom_bar() + 
  ggsave("media/recipes/rMpgStackedBarPlot.png")
