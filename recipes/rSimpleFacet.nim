import ggplotnim

let mpg = toDf(readCsv("data/mpg.csv"))
ggplot(mpg, aes("displ", "hwy")) +
  geom_point(aes(color = "manufacturer")) +
  facet_wrap(["drv", "cyl"]) + 
  ggsave("media/recipes/rSimpleFacet.png")
