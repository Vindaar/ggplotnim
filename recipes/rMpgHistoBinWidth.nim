import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy")) + 
  geom_histogram(binWidth = 1.5) +
  theme_opaque() +
  ggsave("media/recipes/rMpgHistoBinWidth.png")
