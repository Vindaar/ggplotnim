import arraymancer_backend
#import ggplotnim

import sequtils
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

  var rnd = initRand(124325)
  var data: OrderedTable[string, seq[Value]]
  var simPath = newSeq[Value](dates)
  for j in 0..<paths:
    simPath[0] = %~  100.0
    for i in 1..<dates:
      simPath[i] = %~(simPath[i-1].toFloat() * exp(-0.5 * sigma^2 + sigma * gaussian(rnd)))
    data["path" & $(j + 1)] = simPath

  var tenors = newSeq[Value](dates)
  for i in 0..<dates:
    tenors[i] = %~ float(i)
  data["tenors"] = tenors
  toDf(data)
import times

block:
  let t0 = epochTime()
  echo "Creating DF"
  var df = createDataFrame()
  let t1 = epochTime()
  echo "Took ", t1 - t0, " to read dataframe"
  let pNames = toSeq(1 .. paths).mapIt("path" & $it)
  # convert to long table using `gather`
  let t2 = epochTime()
  df = df.gather(pNames, key = "pathNames", value = "pathValues")
  let t3 = epochTime()
  echo "Took ", t3 - t2, " converting to long format"

  echo df.unique

#[

ggplotnim default, debug build
basti at void in ~/CastData/ExternCode/ggplotnim/playground ツ time ./testArMuchGather
Creating DF
Took 0.1273388862609863 to read dataframe
Took 119.7014491558075 converting to long format
./testArMuchGather  119.31s user 0.08s system 99% cpu 1:59.86 total

ggplotnim default, danger build
basti at void in ~/CastData/ExternCode/ggplotnim/playground ツ time ./testArMuchGather
Creating DF
Took 0.01559567451477051 to read dataframe
Took 12.55435419082642 converting to long format
./testArMuchGather  12.44s user 0.05s system 99% cpu 12.592 total

arraymancer backend, debug build
basti at void in ~/CastData/ExternCode/ggplotnim/playground ツ time ./testArMuchGather
Creating DF
Took 0.2135574817657471 to read dataframe
Took 21.26101899147034 converting to long format
./testArMuchGather  21.43s user 0.03s system 99% cpu 21.502 total

arraymancer backend, danger build
basti at void in ~/CastData/ExternCode/ggplotnim/playground ツ time ./testArMuchGather
Creating DF
Took 0.02126693725585938 to read dataframe
Took 1.880753040313721 converting to long format
./testArMuchGather  1.88s user 0.03s system 99% cpu 1.919 total

UPDATE gather implementation:
arraymancer backend, ``DEBUG`` build!!
basti at void in ~/CastData/ExternCode/ggplotnim/playground ツ time ./testArMuchGather
Creating DF
Took 0.2133247852325439 to read dataframe
Took 0.1917932033538818 converting to long format
./testArMuchGather  0.40s user 0.02s system 99% cpu 0.422 total

arraymancer backend, danger build
basti at void in ~/CastData/ExternCode/ggplotnim/playground ツ time ./testArMuchGather
Creating DF
Took 0.2133247852325439 to read dataframe
Took 0.1917932033538818 converting to long format
./testArMuchGather  0.40s user 0.02s system 99% cpu 0.422 total

arraymancer backend, danger build
basti at void in ~/CastData/ExternCode/ggplotnim/playground ツ time ./testArMuchGather
Creating DF
Took 0.02375459671020508 to read dataframe
Took 0.02321767807006836 converting to long format
./testArMuchGather  0.05s user 0.01s system 98% cpu 0.064 total
THAT'S more like it!! :)
]#
