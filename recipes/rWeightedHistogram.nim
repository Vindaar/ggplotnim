import ggplotnim
let df = toDf(readCsv("data/diamonds.csv"))
ggplot(df, aes("carat", weight = "price")) + 
  geom_histogram() + 
  ylab("Binned carat weighted by price") + 
  theme_opaque() +
  ggsave("media/recipes/rWeightedHistogram.png")
