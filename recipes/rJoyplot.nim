import ggplotnim
let df = readCsv("data/gaussSigma_runs.csv")
ggplot(df, aes("bins", "counts")) +
  ggridges("Run", overlap = 3.0) +
  geom_freqpoly(stat = "identity", color = "white",
                size = 2.0) +
  margin(top = 3, right = 2.5) +
  theme_void("black") + hideLegend() +
  ggsave("media/recipes/rJoyplot.png", width = 1200, height = 1200)
