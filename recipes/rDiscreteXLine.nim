import ggplotnim, seqmath
import random

const paths = 10
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
  var rnd = initRand(124325)
  var pathNames = newSeq[string](dates * paths)
  var pathVals = newSeq[float](dates * paths)
  var tenors = newSeq[int](dates * paths)
  for j in 0 ..< paths:
    pathVals[j * dates] = 100.0
    pathNames[j * dates] = "path" & $(j + 1)
    tenors[j * dates] = 0
    for i in 1 ..< dates:
      let idx = j * dates + i
      pathNames[idx] = "path" & $(j + 1)
      pathVals[idx] = (pathVals[idx - 1] * exp(-0.5 * sigma * sigma + sigma * gaussian(rnd)))
      tenors[idx] = i
  result = toDf({ "tenors" : tenors,
                      "pathNames" : pathNames,
                      "pathValues" : pathVals })

let df = createDataFrame()
ggplot(df, aes("tenors", "pathValues", color = "pathNames")) + 
  geom_line() +
  ggsave("media/recipes/rDiscreteXLine.png")
