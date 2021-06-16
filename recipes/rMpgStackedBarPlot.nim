import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("class", fill = "drv")) + 
  geom_bar() + 
  theme_opaque() +
  ggsave("media/recipes/rMpgStackedBarPlot.png")
