import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("class", color = "drv")) + 
  geom_point(stat = "count") + 
  geom_line(stat = "count") + 
  ggsave("media/recipes/rMpgStackedPointPlot.png")
