import nimdata

import seqmath, sequtils, times, sugar

proc main =
  let x = linspace(0.0, 2.0, 1000)
  let y = x.mapIt(0.12 + it * it * 0.3 + 2.2 * it * it * it)
  var df = DF.fromSeq(zip(x, y))
  df.take(5).show()
  echo df.count()

  const num = 1_000_000
  let t0 = cpuTime()
  for i in 0 ..< num:
    df = df.map(x => (x[0], x[0] * x[0])).cache()
  let t1 = cpuTime()
  echo "Took ", t1 - t0, " for ", num, " iter"



when isMainModule:
  main()
