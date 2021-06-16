import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("displ", "hwy", color = "cty")) + 
  geom_point() + 
  theme_opaque() +
  ggsave("media/recipes/rMpgContinuousColorPoints.png")
