import ggplotnim
let df = readCsv("data/mpg.csv")
# coloring by class is of course not required to make this work :)
ggplot(df, aes("cyl", "hwy", color = "class")) + 
  geom_point() + 
  ggsave("media/recipes/rMpgDiscreteXScale.png")
