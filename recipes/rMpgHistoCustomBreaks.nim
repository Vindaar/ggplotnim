import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy")) + 
  geom_histogram(breaks = @[0'f64, 10, 15, 19, 23, 25, 40]) +
  theme_opaque() +
  ggsave("media/recipes/rMpgHistoCustomBreaks.png")
