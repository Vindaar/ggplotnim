import ggplotnim
let df = readCsv("data/diamonds.csv")
ggplot(df, aes("carat", weight = "price")) + 
  geom_histogram() + 
  ylab("Binned carat weighted by price") + 
  ggsave("media/recipes/rWeightedHistogram.png")
