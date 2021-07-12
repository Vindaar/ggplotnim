import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("cty", color = "class")) + 
  geom_histogram(lineWidth = some(2.0), 
                 alpha = some(0.0), # make transparent (only fill)
                 hdKind = hdOutline) + # draw as outline
  ggsave("media/recipes/rHistogramOutline.png")
