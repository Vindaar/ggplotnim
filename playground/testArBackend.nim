import arraymancer_backend
#import ggplotnim
import seqmath, sequtils, times, sugar
#import formulaClosure
#import laser/strided_iteration/foreach
#import ggplotnim

proc main =
  let x = linspace(0.0, 2.0, 1000)
  let y = x.mapIt(0.12 + it * it * 0.3 + 2.2 * it * it * it)

  var df = seqsToDf(x, y)
  #echo df.repr
  echo df

  #echo df.head(5)
  #echo df.tail(5)
  #
  #echo df[0 .. 6]
  #
  #block:
  #  let x2 = collect(newSeq):
  #    for i in 0 ..< x.len:
  #      if i mod 2 == 0:
  #        x[i]
  #  let y2 = x2.mapIt(0.12 + it * it * 0.3 + 2.2 * it * it * it)
  #  echo x2.len
  #  let df2 = seqsToDf({"x" : x2, "y" : y2})
  #  echo setDiff(df, df2, symmetric = true)
  #  echo setDiff(df2, df)



  let mpg = toDf(readCsv2("../data/mpg.csv"))
  echo mpg.arrange(@["class", "cyl"]).pretty(-1)
  for lab, subDf in groups(mpg.group_by("class")):
    echo "DF ", subDf
    echo "Lab: ", lab
  var count = 0
  for lab, subDf in groups(mpg.group_by(["class", "cyl"])):
    echo "DF ", subDf
    echo "Lab: ", lab
    inc count
  echo count

  echo mpg.count("class")

  block:
    let dfD = toDf(readCsv2("../data/50-18004.CSV"))
    let dfnew = dfD.gather(["C1_in_V", "C2_in_V"], key = "Channel", value = "V")
    echo dfNew

  echo mpg.summarize(f{float: `mean` << mean(`hwy`)},
                     f{float: `meanCty` << mean(`cty`)})
  echo mpg.group_by("class").summarize(f{float: `mean` << mean(`hwy`) + mean(`cty`) - 20},
                                       f{float: `meanCty` << mean(`cty`)})


  echo mpg.mutate(f{`agmax` << argmax(`hwy`, 0)[0].float + 0.5})



  #df.mutateInplace(f{"x" * 2.0})
  #echo df
  #let mulVal = 2.0
  ##let aba = f{"test" + 2 - "x"}
  ##echo aba.name
  #
  #let (name, col) = df.calcNewColumn(f{"x2" ~ "x" * mulVal + "y"})
  #df[name] = col
  #echo df
  #
  #echo f{"res" <- "test" & "aba" & "ok"}
  #echo f{2 - 1}
  #echo f{2 - 1 == 1}
  #
  #let (name2, colBool) = df.calcNewColumn(f{"x2" ~ "x" > 0.5})
  #echo colBool
  #echo f{"x" > 0.5}
  #echo df.filter(f{"x" > 0.5 and "y" < 1.1})

  #let diamonds = toDf(readCsv("../data/diamonds.csv"))
  #let diamonds = toDf(readCsv2("../data/diamonds.csv"))
  #echo "Filtering now "
  #let t0 = cpuTime()
  #discard diamonds.filter(f{"cut" == "Ideal"})
  #discard diamonds.filter(f{`cut` == "Ideal"})
  #echo diamonds.filter(f{"y" < 4.3 and "z" > 2.73},
  #                     f{"cut" == "Ideal"})
  #echo diamonds.filter(f{`y` < 4.3 and `z` > 2.73},
  #                     f{`cut` == "Ideal"})
  #let t1 = cpuTime()
  #echo "filtering took ", t1 - t0
  #
  #echo "Sorting by 1 now : "
  #let t2a = cpuTime()
  #discard diamonds.arrange(@["x"])
  #let t3a = cpuTime()
  #echo "Sorting by 1 took ", t3a - t2a
  #
  #
  #echo "Sorting now : "
  #let t2 = cpuTime()
  #discard  diamonds.arrange(@["x", "z"])
  #let t3 = cpuTime()
  #echo "Sorting took ", t3 - t2

  let df1 = seqsToDf({ "names" : @["peter", "siri", "katja", "birka", "julia"],
                       "nums" : @[1, 2, 3, 4, 5] })
  let df2 = seqsToDf({ "names" : @["peter", "siri", "birka", "julia"],
                       "nums" : @[1, 2, 4, 5],
                       "id" : @[31, 32, 65, 23]})
  let t0 = cpuTime()
  const num = 1_000
  for _ in 0 ..< num:
    discard innerJoin(df1, df2, by = "names")
  echo "Took ", (cpuTime() - t0) / num.float
  #const num = 1_000_000
  #let t0 = cpuTime()
  #for i in 0 ..< num:
  #  df = df.mutate(f{"xSquared" ~ "x" * "x"})
  #let t1 = cpuTime()
  #echo "Took ", t1 - t0, " for ", num, " iter"


#proc rawTensor() =
#  const num = 1_000_000
#  let x = linspace(0.0, 2.0, 1000)
#  let y = x.mapIt(0.12 + it * it * 0.3 + 2.2 * it * it * it)
#  var df = seqsToDf(x, y)
#  var t = newTensor[float](df.len)
#  let xT = df["x"].toTensor(float)
#  let t0 = cpuTime()
#  for i in 0 ..< num:
#    for j in 0 ..< df.len:
#      t[j] = xT[j] * xT[j]
#  let t1 = cpuTime()
#  echo "Took ", t1 - t0, " for ", num, " iter"


when isMainModule:
  main()
  #rawTensor()

# 10.6 seconds for 1_000_000 iter
# with arraymancer backend
#
# 23.3 seconds for 100_000 iter
# with normal backend
# speed up of x22!

# using pandas:
#[

import numpy as np
import pandas as pd
x = np.linspace(0.0, 2.0, 1000)
y = (0.12 + x * x * 0.3 + 2.2 * x * x * x)


df = pd.DataFrame({"x" : x, "y" : y})
def call():
    t0 = time.time()
    for i in range(100000):
        df.assign(xSquared = df["x"] * df["x"])
    t1 = time.time()
    print("Took ", (t1 - t0), " for 1,000,000 iterations")


Took  60.24467134475708  for 1,000,00 iterations                                                                     ]#



#[
NimData


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

Took 16.322826325 for 1000000 iter


Pretty good!

]#


#result = toColumn res_47970014


#echo df["x"]
#df["x2"] = block:
#  var res = newTensor[float](df.len)
#  var idx = 0
#  let xCol = df["x"].fCol
#  let yCol = df["y"].fCol
#  forEach x in xCol, y in yCol:
#    res[idx] = x * mulVal + y
#    inc idx
#  res.toColumn

#let fn = f{1.0 + 2.0}
#echo fn
#echo type(evaluate(fn))

#proc x(df: DataFrame): Column =
#  let col1 = get(df, "x")
#  let col2 = get(df, "y")
#  var res = newTensor[float](df.len)
#  for i in 0 ..< df.len:
#    res[i] = col1[i] * mulVal + col2[i]
#  result = toColumn res
