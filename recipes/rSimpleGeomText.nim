import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy", "displ")) + 
  geom_text(aes(text = "manufacturer")) + 
  theme_opaque() +
  ggsave("media/recipes/rSimpleGeomText.png")
