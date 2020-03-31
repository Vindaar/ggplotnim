import ggplotnim
import random
import tables
import math

const paths = 1000
const dates = 80

proc gaussian*(rnd: var Rand, mu = 0.0, sigma = 1.0): float =
  var
    s = 0.0
    u = 0.0
    v = 0.0
  while s >= 1.0 or s <= 0.0:
    u = 2.0 * rnd.rand(0.0..1.0) - 1.0
    v = 2.0 * rnd.rand(0.0..1.0) - 1.0
    s = (u * u) + (v * v)

  let x = u * sqrt(-2.0 * ln(s) / s)
  return (mu + (sigma * x))

proc createDataFrame(): DataFrame =
  const sigma = 0.10
  result = initDataFrame()
  var rnd = initRand(124325)
  var simPath = newSeq[float](dates)
  for j in 0..<paths:
    simPath[0] = 100.0
    for i in 1..<dates:
      simPath[i] = simPath[i-1] * exp(-0.5 * sigma^2 + sigma * gaussian(rnd))
    result["path" & $(j + 1)] = toColumn simPath

  var tenors = newSeq[int](dates)
  for i in 0..<dates:
    tenors[i] = i
  result["tenors"] = toColumn tenors

let df = createDataFrame()
var g = ggplot(df)
for i in 1..paths:
  g = g + geom_line(aes(y = "path" & $i, x = "tenors"))
g + ggsave("test1.pdf")
