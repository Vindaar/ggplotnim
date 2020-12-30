import ggplotnim
let df = toDf(readCsv("data/gaussSigma_runs.csv"))
ggplot(df, aes("bins", "counts", fill = "Run")) +
  ggridges("Run", overlap = 3.0) +
  geom_freqpoly(stat = "identity", color = some(parseHex("FFFFFF")),
                size = some(3.0)) +
  xlab("gaussSigma") + ylab("Counts") +
  margin(top = 3, right = 2.5) +
  theme_void(parseHex("000000")) + hideLegend() +
  ggsave("media/recipes/rRidgeLineGaussBlack.png", width = 1200, height = 1200)
