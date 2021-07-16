import ggplotnim
let df = readCsv("data/mpg.csv")
ggplot(df, aes("displ", "hwy", color = "class")) +
  geom_point() +
  geom_smooth(smoother = "poly", polyOrder = 1) +
  ggsave("media/recipes/rLinearFit.png")
