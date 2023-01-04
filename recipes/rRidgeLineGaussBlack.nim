import ggplotnim
let df = readCsv("data/gaussSigma_runs.csv")
ggplot(df, aes("bins", "counts", fill = "Run")) +
  ggridges("Run", overlap = 3.0) +
  geom_freqpoly(stat = "identity", color = "white",
                size = 3.0) +
  xlab("gaussSigma") + ylab("Counts") +
  margin(top = 3, right = 2.5, bottom = 2) +
  theme_void("black") + hideLegend() +
  ggsave("media/recipes/rRidgeLineGaussBlack.png", width = 1200, height = 1200)
