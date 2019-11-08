import ggplotnim, os

proc fastRead() =
  let csvD = readCsv("../data/diamonds.csv")
  let df = toDf(csvD)
  echo df

proc fastPlot() =
  let mpg = toDf(readCsv("../data/mpg.csv"))
  ggplot(mpg, aes(x = "displ", y = "hwy")) +
    geom_point() +
    ggsave("figs/2.3_1.pdf")
  ggplot(mpg, aes("displ", "hwy")) +
    geom_point() +
    ggsave("figs/2.3_2.pdf")
  ggplot(mpg, aes("cty", "hwy")) +
    geom_point() +
    ggsave("figs/2.3_4.pdf")

proc fastTogether() =
  fastPlot()
  fastRead()

proc slowTogether() =
  block:
    let mpg = toDf(readCsv("../data/mpg.csv"))
    ggplot(mpg, aes(x = "displ", y = "hwy")) +
      geom_point() +
      ggsave("figs/2.3_1.pdf")
    ggplot(mpg, aes("displ", "hwy")) +
      geom_point() +
      ggsave("figs/2.3_2.pdf")
    ggplot(mpg, aes("cty", "hwy")) +
      geom_point() +
      ggsave("figs/2.3_4.pdf")
  block:
    let csvD = readCsv("../data/diamonds.csv")
    let df = toDf(csvD)
    echo df
when isMainModule:
  let t0 = epochTime()
  fastPlot()
  let t1 = epochTime()
  echo "Plot first: ", (t1 - t0)
  fastRead()
  let t2 = epochTime()
  echo "Read first: ", (t2 - t1)
  fastTogether()
  let t3 = epochTime()
  echo "Fast together ", (t3 - t2)
  slowTogether()
  let t4 = epochTime()
  echo "Together ", (t4 - t3)
