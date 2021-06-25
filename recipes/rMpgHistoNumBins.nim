import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy")) + 
  geom_histogram(bins = 20) + # by default 30 bins are used
  ggsave("media/recipes/rMpgHistoNumBins.png")
