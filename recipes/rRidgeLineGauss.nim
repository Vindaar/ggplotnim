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
ggplot(df, aes("bins", "counts", fill = "Run")) +
  ggridges("Run", overlap = 3.0) + # use a large overlap
  geom_freqpoly(stat = "identity", # do not perform binning
                color = "white", # white outline
                size = 1.5) + # make the lines a bit thicker
  geom_linerange(aes = aes(yMin = 0, # draw black line of common mean
                           yMax = ymax, 
                           x = mean), 
                 color = "black") +
  geom_linerange(aes = aes(
    fill = "Run", yMin = 0, yMax = ymax, # draw red line for each mean 
    x = f{float -> float: getMean(`bins`, `counts`)}), # compute the mean of the current Run
    color = "#FF0000") + # color the line red
  margin(top = 2) + # increase top margin due to large overlap
  xlab("gaussSigma") + ylab("Counts") +
  ggsave("media/recipes/rRidgeLineGauss.png", width = 1200, height = 1200)
