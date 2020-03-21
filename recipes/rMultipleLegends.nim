import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("cty", "displ", size = "cyl", color = "cty")) + #, color = "cty", size = "cyl")) +
  geom_point() +
  ggsave("media/recipes/rMultipleLegends.png")
