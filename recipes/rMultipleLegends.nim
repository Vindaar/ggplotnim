import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("cty", "displ", size = "cyl", color = "cty")) +
  geom_point() +
  theme_opaque() +
  ggsave("media/recipes/rMultipleLegends.png")
