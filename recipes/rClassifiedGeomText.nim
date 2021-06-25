import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("hwy", "displ", color = "class", size = "cyl")) + 
  geom_text(aes(text = "manufacturer")) + 
  ggsave("media/recipes/rClassifiedGeomText.png")
