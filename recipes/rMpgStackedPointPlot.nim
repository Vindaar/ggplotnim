import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("class", fill = "drv")) + 
  geom_point(stat = "count") + 
  ggsave("media/recipes/rMpgStackedPointPlot.png")
