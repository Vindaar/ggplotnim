import ggplotnim

let df = toDf(readCsv("data/gaussSigma_runs.csv"))
ggplot(df, aes("bins", "counts", fill = "Run")) +
#ggplot(df, aes("bins", "counts")) +
  ggridges("Run", overlap = 3.0) +
  geom_freqpoly(stat = "identity", color = some(parseHex("FFFFFF")),
                size = some(1.5)) +
  geom_linerange(aes = aes(yMin = 0, yMax = ymax, x = mean),
                 color = some(black)) +
  geom_linerange(aes = aes(
    fill = "Run", yMin = 0, yMax = ymax,
    x = f{float -> float: getMean(`bins`, `counts`)}),
    color = some(parseHex("FF0000"))) +
  margin(top = 2) +
  xlab("gaussSigma") + ylab("Counts") +
  margin(top = 3, right = 2.5) +
  #theme_void(black) + hideLegend() +
  ggsave("/tmp/gaussSigma_ridge.pdf", width = 800, height = 800)#1200)

