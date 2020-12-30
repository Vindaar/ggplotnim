import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
# coloring by class is of course not required to make this work :)
ggplot(df, aes("cyl", "hwy", color = "class")) + 
  geom_point() + 
  theme_opaque() +
  ggsave("media/recipes/rMpgDiscreteXScale.png")
