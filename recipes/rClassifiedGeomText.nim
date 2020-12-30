import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("hwy", "displ", color = "class", size = "cyl")) + 
  geom_text(aes(text = "manufacturer")) + 
  theme_opaque() +
  ggsave("media/recipes/rClassifiedGeomText.png")
