import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("cty", color = "class")) + 
  geom_histogram(lineWidth = 2.0, 
                 alpha = 0.0, # make transparent (only fill)
                 hdKind = hdOutline) + # draw as outline
  ggsave("media/recipes/rHistogramOutline.png")
