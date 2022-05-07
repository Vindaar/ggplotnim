import ggplotnim
let bins = @[0, 2, 5, 9, 15]
let counts = @[0.1, 0.8, 0.3, 0.05, 0.0] # <- last element is dummy
let df = toDf({"bin_edges" : bins, "counts" : counts})
ggplot(df, aes("bin_edges", "counts")) + 
  geom_histogram(stat = "identity") +
  ggsave("media/recipes/rPrebinnedHisto.png")
