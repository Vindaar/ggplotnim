import ggplotnim

let mpg = readCsv("data/mpg.csv")
ggplot(mpg, aes("displ", "hwy")) +
  geom_point(aes(color = "manufacturer")) +
  facet_wrap(["drv", "cyl"]) + 
  ggsave("media/recipes/rSimpleFacet.png")
