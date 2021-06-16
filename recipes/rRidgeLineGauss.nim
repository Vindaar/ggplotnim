import ggplotnim

proc getMean(bins, counts: Tensor[float]): float =
  ## we define a helper proc to compute each mean of each ridge
  ## individually for each `Run`
  for i in 0 ..< counts.size:
    result += bins[i] * counts[i]
  result /= counts.sum

let df = readCsv("data/gaussSigma_runs.csv")
echo df
let mean = getMean(df["bins", float], df["counts", float])
let ymax = df["counts", float].max
let white = color(1.0, 1.0, 1.0)
let black = color(0.0, 0.0, 0.0)
ggplot(df, aes("bins", "counts", fill = "Run")) +
  ggridges("Run", overlap = 3.0) + # use a large overlap
  geom_freqpoly(stat = "identity", # do not perform binning
                color = some(white), # white outline
                size = some(1.5)) + # make the lines a bit thicker
  geom_linerange(aes = aes(yMin = 0, # draw black line of common mean
                           yMax = ymax, 
                           x = mean), 
                 color = some(black)) +
  geom_linerange(aes = aes(
    fill = "Run", yMin = 0, yMax = ymax, # draw red line for each mean 
    x = f{float -> float: getMean(`bins`, `counts`)}), # compute the mean of the current Run
    color = some(parseHex("FF0000"))) + # color the line red
  margin(top = 2) + # increase top margin due to large overlap
  xlab("gaussSigma") + ylab("Counts") +
  theme_opaque() +
  ggsave("media/recipes/rRidgeLineGauss.png", width = 1200, height = 1200)
