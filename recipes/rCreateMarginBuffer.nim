import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("hwy", "cty")) +
  geom_point() +
  ylim(5.0, 25.0) +
  yMargin(0.1) +
  ggsave("media/recipes/rCreateMarginBuffer.png")
