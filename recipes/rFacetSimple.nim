import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("hwy", "displ", color = "class")) + 
  facet_wrap(~ cyl) +
  geom_point() + 
  ggsave("media/recipes/rFacetSimple.png")
