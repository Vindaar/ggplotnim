import ggplotnim, unittest, sequtils, math, strutils, os
import algorithm

suite "Data frame tests":
  test "Creation of DFs from seqs":
    let a = [1, 2, 3]
    let b = [3, 4, 5]
    let c = [4, 5, 6]
    let d = [8, 9, 10]
    # creation directly from a,b,c,d
    block:
      let df = seqsToDf(a, b, c, d)
      check "a" in df
      check "b" in df
      check "c" in df
      check "d" in df
    # creation via key / value pairs
    block:
      let df = seqsToDf({ "one" : a,
                          "two" : b,
                          "three" : c,
                          "four" : d})
      check "one" in df
      check "two" in df
      check "three" in df
      check "four" in df

  test "Testing `bind_rows`":
    let a = [1, 2, 3]
    let b = [3, 4, 5]

    let c = [4, 5, 6, 7]
    let d = [8, 9, 10, 11]

    block:
      # bind_rows with automatic `ids`, both having same columns
      let df = seqsToDf({"a" : a, "b" : b})
      let df2 = seqsToDf({"a" : c, "b" : d})
      let res = bind_rows([df, df2])
      echo "Resulting df "
      echo res
      check toSeq(res["a"]) == %~ concat(@a, @c)
      check toSeq(res["b"]) == %~ concat(@b, @d)
      # without specifying `id`, no column will be added
      #check toSeq(res["id"]) == %~ concat(toSeq(0..<a.len).mapIt("0"),
      #                                      toSeq(0..<c.len).mapIt("1"))

    block:
      echo "-------"
      # bind_rows with automatic `ids`, having different columns
      let df = seqsToDf({"a" : a, "b" : b})
      let df2 = seqsToDf({"a" : c, "d" : d})
      let res = bind_rows([df, df2])
      echo "Resulting df "
      echo res
      echo res["a"]
      echo toVector(%~ concat(@a, @c))
      check toSeq(res["a"]) == %~ concat(@a, @c)
      check toSeq(res["b"]) == %~ concat(%~ b, toSeq(0 .. 3).mapIt(Value(kind: VNull)))
      check toSeq(res["d"]) == %~ concat(toSeq(0 .. 2).mapIt(Value(kind: VNull)), %~ d)
      #check toSeq(res["id"]) == %~ concat(toSeq(0..<a.len).mapIt("0"),
      #                                      toSeq(0..<c.len).mapIt("1"))

    block:
      echo "-------"
      # bind_rows with custom `id` name, both having same columns
      let df = seqsToDf({"a" : a, "b" : b})
      let df2 = seqsToDf({"a" : c, "b" : d})
      let res = bind_rows([df, df2], id = "combine")
      echo "Resulting df "
      echo res
      check toSeq(res["a"]) == %~ concat(@a, @c)
      check toSeq(res["b"]) == %~ concat(@b, @d)
      check toSeq(res["combine"]) == %~ concat(toSeq(0..<a.len).mapIt("0"),
                                                 toSeq(0..<c.len).mapIt("1"))

    block:
      echo "-------"
      # bind_rows with custom `id` name, custom `id` values, both having same columns
      let df = seqsToDf({"a" : a, "b" : b})
      let df2 = seqsToDf({"a" : c, "b" : d})
      let res = bind_rows([("one", df), ("two", df2)], id = "combine")
      echo "Resulting df "
      echo res
      check toSeq(res["a"]) == %~ concat(@a, @c)
      check toSeq(res["b"]) == %~ concat(@b, @d)
      check toSeq(res["combine"]) == %~ concat(toSeq(0..<a.len).mapIt("one"),
                                               toSeq(0..<c.len).mapIt("two"))

  test "Group by":
    proc almostEqual(x, y: float, eps = 1e-6): bool =
      result = (x - y) < eps

    var mpg = toDf(readCsv("data/mpg.csv"))

    let mpggroup = mpg.group_by("cyl")

    # TODO: make this to `doAssert`!
    let summary = mpg.summarize(f{"mean_cyl" ~ mean("cyl")},
                                f{"mean_hwy" ~ mean("hwy")})
    echo summary
    check almostEqual(summary["mean_cyl"][0].toFloat, 5.88889)
    check almostEqual(summary["mean_hwy"][0].toFloat, 23.4402)

    echo "----"
    let sum_grouped = mpggroup.summarize(f{"mean_displ" ~ mean("displ")},
                                         f{"mean_hwy" ~ mean("hwy")})
      .arrange("cyl")
    check sum_grouped.len == 4
    check sum_grouped["cyl"][0].toInt == 4
    check sum_grouped["cyl"][1].toInt == 5
    check sum_grouped["cyl"][2].toInt == 6
    check sum_grouped["cyl"][3].toInt == 8
    check almostEqual(sum_grouped["mean_displ"][0].toFloat, 2.14568)
    check almostEqual(sum_grouped["mean_displ"][1].toFloat, 2.5)
    check almostEqual(sum_grouped["mean_displ"][2].toFloat, 3.40886)
    check almostEqual(sum_grouped["mean_displ"][3].toFloat, 5.13286)
    check almostEqual(sum_grouped["mean_hwy"][0].toFloat, 28.8025)
    check almostEqual(sum_grouped["mean_hwy"][1].toFloat, 28.75)
    check almostEqual(sum_grouped["mean_hwy"][2].toFloat, 22.8228)
    check almostEqual(sum_grouped["mean_hwy"][3].toFloat, 17.6286)
    let mpg2groups = mpggroup.group_by("class", add = true)
    let classes = mpg["class"].unique
    let cyls = mpg["cyl"].unique
    let product = product([classes, cyls])
    var subgroupCount = 0
    for (by, df) in groups(mpg2groups):
      echo "--------------------Subgroup by ", by, "--------------------\n"
      echo df
      # check whether current subgroup is part of our cartesian product, i.e. combinations we
      # expect. `by` contains both the field name and value, extract values
      let curSubGroup = @[by[1][1], by[0][1]]
      check curSubGroup in product
      inc subGroupCount

    # sub group count must be smaller or equal to the cartesian product
    # it can be smaller if a specific combination has no entries
    check subGroupCount <= product.len
    # which is the case for current grouping and the mpg dataset:
    check subGroupCount == 19


    let cylFiltered = mpg.filter(f{"cyl" == 4})
    check cylFiltered.len == 81
    let cylDrvFiltered = cylFiltered.filter(f{"drv" == "4"})
    check cylDrvFiltered.len == 23

    echo cylDrvFiltered
    #echo mpg.filter(f{"class" == "suv"})

  test "Unequal":
    let mpg = toDf(readCsv("data/mpg.csv"))

    let mpgNoSuv = mpg.filter(f{"class" != "suv"})
    check (%~ "suv") notin mpgNoSuv["class"].unique

  test "Filter - two comparisons using `and`":
    let x = toSeq(0 .. 100)
    let df = seqsToDf(x)
    let dfFilter = df.filter(f{"x" >= 50 and
                               "x" <= 75})
    check dfFilter["x"].vToSeq == %~ toSeq(50 .. 75)

  test "Filter - comparisons using function":
    let x = toSeq(0 .. 100)
    let df = seqsToDf(x)
    let dfFilter = df.filter(f{"x" >= max("x") * 0.5})
    check dfFilter["x"].vToSeq == %~ toSeq(50 .. 100)

  test "Transmute - float arithmetic":
    let x = toSeq(0 ..< 100)
    let y = x.mapIt(sin(it.float))
    let y2 = x.mapIt(pow(sin(it.float), 2.0))
    let df = seqsToDf(x, y)
    check df.len == 100
    let dfTrans = df.transmute(f{"x"}, f{"y2" ~ "y" * "y"})
    check "y" notin dfTrans
    check "y2" in dfTrans
    check "x" in dfTrans
    check dfTrans["y2"].vToSeq == %~ y2

  test "Transmute - parse floats in dataframe from string column":
    let x = toSeq(0 ..< 100)
    let y = x.mapIt($sin(it.float))
    let yFloat = x.mapIt(sin(it.float))
    let df = seqsToDf(x, y)
    check df.len == 100
    liftScalarStringProc(parseFloat, toExport = false)
    let dfTrans = df.transmute(f{"x"}, f{"yFloat" ~ parseFloat("y")})
    check "y" notin dfTrans
    check "yFloat" in dfTrans
    check "x" in dfTrans
    check dfTrans["yFloat"].vToSeq == %~ yFloat

  test "Gather - 2 columns":
    let x = toSeq(0 ..< 100)
    let y1 = x.mapIt(sin(it.float))
    let y2 = x.mapIt(sin(it.float - PI / 2.0) - 0.5)
    let yComb = concat(y1, y2)
    let df = seqsToDf(x, y1, y2)
    check df.len == 100
    let dfLong = df.gather(["y1", "y2"], key = "from", value = "y")
    check dfLong.len == 200
    check dfLong["from"].unique == %~ @["y1", "y2"]
    check dfLong["y"].vToSeq == %~ yComb
    let dfY1FromLong = dfLong.filter(f{"from" == "y1"})
    let dfY2FromLong = dfLong.filter(f{"from" == "y2"})
    check dfY1FromLong["y"].vToSeq == df["y1"].vToSeq
    check dfY2FromLong["y"].vToSeq == df["y2"].vToSeq
    check dfY1FromLong["x"].vToSeq == df["x"].vToSeq
    check dfY2FromLong["x"].vToSeq == df["x"].vToSeq

  test "Gather - 3 columns":
    ## check that it works for 3 columns too
    let x = toSeq(0 ..< 100)
    let y1 = x.mapIt(sin(it.float))
    let y2 = x.mapIt(sin(it.float - PI / 2.0) - 0.5)
    let y3 = x.mapIt(cos(it.float - PI / 2.0) - 0.5)
    let yComb = concat(y1, y2, y3)
    let df = seqsToDf(x, y1, y2, y3)
    check df.len == 100
    let dfLong = df.gather(["y1", "y2", "y3"], key = "from", value = "y")
    check dfLong.len == 300
    check dfLong["from"].unique == %~ @["y1", "y2", "y3"]
    check dfLong["y"].vToSeq == %~ yComb
    let dfY1FromLong = dfLong.filter(f{"from" == "y1"})
    let dfY2FromLong = dfLong.filter(f{"from" == "y2"})
    let dfY3FromLong = dfLong.filter(f{"from" == "y3"})
    check dfY1FromLong["y"].vToSeq == df["y1"].vToSeq
    check dfY2FromLong["y"].vToSeq == df["y2"].vToSeq
    check dfY3FromLong["y"].vToSeq == df["y3"].vToSeq
    check dfY1FromLong["x"].vToSeq == df["x"].vToSeq
    check dfY2FromLong["x"].vToSeq == df["x"].vToSeq
    check dfY3FromLong["x"].vToSeq == df["x"].vToSeq

  test "Gather - string and float column":
    ## while it may be questionable to combine string and float columns in general
    ## it should still work
    let x = toSeq(0 ..< 100)
    let y1 = x.mapIt(sin(it.float))
    let yStr = x.mapIt($it)
    let yComb = concat(%~ y1, %~ yStr)
    let df = seqsToDf(x, y1, yStr)
    check df.len == 100
    let dfLong = df.gather(["y1", "yStr"], key = "from", value = "y")
    check dfLong.len == 200
    check dfLong["from"].unique == %~ @["y1", "yStr"]
    check dfLong["y"].vToSeq == yComb
    let dfY1FromLong = dfLong.filter(f{"from" == "y1"})
    let dfYSTRFromLong = dfLong.filter(f{"from" == "yStr"})
    check dfY1FromLong["y"].vToSeq == df["y1"].vToSeq
    check dfYSTRFromLong["y"].vToSeq == df["yStr"].vToSeq
    check dfY1FromLong["x"].vToSeq == df["x"].vToSeq
    check dfYSTRFromLong["x"].vToSeq == df["x"].vToSeq

  test "Pretty printing of DFs":
    var
      # need the data as two sequences (well actually as a DataFrame, but that is
      # created most easily from two or more sequences).
      x: seq[float]
      y: seq[float]
    for i in 0 ..< 1000:
      let pos = 2 * 3.1415 / 100.0 * i.float
      x.add pos
      y.add sin(pos)
    let df = seqsToDf(x, y)
    let defaultExp = """
       Idx         x         y
         0         0         0
         1   0.06283   0.06279
         2    0.1257    0.1253
         3    0.1885    0.1874
         4    0.2513    0.2487
         5    0.3141     0.309
         6     0.377    0.3681
         7    0.4398    0.4258
         8    0.5026    0.4817
         9    0.5655    0.5358
        10    0.6283    0.5878
        11    0.6911    0.6374
        12     0.754    0.6845
        13    0.8168     0.729
        14    0.8796    0.7705
        15    0.9425     0.809
        16     1.005    0.8443
        17     1.068    0.8763
        18     1.131    0.9048
        19     1.194    0.9298
"""
    let dfStr = pretty(df, header = false)
    check dfStr == defaultExp
    let expPrecision12 = """
               Idx                 x                 y
                 0                 0                 0
                 1           0.06283    0.062788670114
                 2           0.12566    0.125329556644
                 3           0.18849    0.187375853836
                 4           0.25132    0.248682707741
                 5           0.31415    0.309008182482
                 6           0.37698    0.368114215006
                 7           0.43981    0.425767554563
                 8           0.50264    0.481740683175
                 9           0.56547    0.535812713502
                10            0.6283    0.587770260526
                11           0.69113    0.637408283636
                12           0.75396    0.684530895785
                13           0.81679    0.728952136516
                14           0.87962    0.770496705823
                15           0.94245    0.809000655938
                16           1.00528    0.844312038323
                17           1.06811    0.876291503299
                18           1.13094     0.90481284997
                19           1.19377    0.929763524249
"""
    let dfPrecision12 = pretty(df, precision = 12, header = false)
    check expPrecision12 == dfPrecision12

  test "CSV parsing with spaces":
    const csvData = """
t_in_s,  C1_in_V,  C2_in_V,  type
-3.0000E-06,  -2.441E-04,  -6.836E-04,  T1
-2.9992E-06,  2.441E-04,  -6.836E-04 ,  T1
-2.9984E-06,  1.025E-03,  -8.789E-04 ,  T1
-2.9976E-06,  1.025E-03,  -2.930E-04 ,  T1
-2.9968E-06,  9.277E-04,  2.930E-04  ,  T2
-2.9960E-06,  4.395E-04,  4.883E-04  ,  T2
-2.9952E-06,  1.465E-04,  -2.930E-04 ,  T2
-2.9944E-06,  -3.418E-04,  -1.270E-03,  T2
"""
    writeFile("testCsvSpaces.csv", csvData)
    let csvRead = readCsv("testCsvSpaces.csv")
    let texp = @[-3.0000E-06, -2.9992E-06, -2.9984E-06, -2.9976E-06, -2.9968E-06,
                 -2.9960E-06, -2.9952E-06, -2.9944E-06]
    let c1Exp = @[-2.441E-04, 2.441E-04, 1.025E-03, 1.025E-03, 9.277E-04, 4.395E-04,
                   1.465E-04, -3.418E-04]
    let c2Exp = @[-6.836E-04, -6.836E-04, -8.789E-04, -2.930E-04, 2.930E-04,
                  4.883E-04, -2.930E-04, -1.270E-03]
    let typeExp = @["T1", "T1", "T1", "T1", "T2",
                    "T2", "T2", "T2"]
    let dfExp = seqsToDf({"t_in_s" : texp, "C1_in_V" : c1Exp, "C2_in_V" : c2Exp,
                           "type" : typeExp})
    let df = toDf(csvRead)
    check df["t_in_s"].vToSeq == dfExp["t_in_s"].vToSeq
    check df["C1_in_V"].vToSeq == dfExp["C1_in_V"].vToSeq
    check df["C2_in_V"].vToSeq == dfExp["C2_in_V"].vToSeq
    check df["type"].vToSeq == dfExp["type"].vToSeq
    removeFile("testCsvSpaces.csv")

  test "Summarize":
    let mpg = toDf(readCsv("data/mpg.csv"))
    block:
      # explicit LHS
      let res = mpg.summarize(f{"num" ~ sum("cyl")})
      check "num" in res
      check res.len == 1
      check res["num"][0] == %~ 1378
      # implicit LHS
      let resImplicit = mpg.summarize(f{sum("cyl")})
      check "(sum cyl)" in resImplicit
      check resImplicit.len == 1
      check resImplicit["(sum cyl)"][0] == %~ 1378
    block:
      # explicit LHS
      let res = mpg.summarize(f{"mean" ~ mean("cyl")})
      check "mean" in res
      check res.len == 1
      check almostEqual(res["mean"][0].toFloat, 5.888888889)
      # implicit LHS
      let resImplicit = mpg.summarize(f{mean("cyl")})
      check "(mean cyl)" in resImplicit
      check resImplicit.len == 1
      check almostEqual(resImplicit["(mean cyl)"][0].toFloat, 5.888888889)

  test "Count":
    # count elements by group. Useful combination of group_by and summarize(len)
    let mpg = toDf(readCsv("data/mpg.csv"))
    # in manual case the order is not preserved, due to `summarize` impl!
    let exp = toSet({6 : 79, 8 : 70, 4 : 81, 5 : 4})
    block:
      # manually
      let res = mpg.group_by("cyl").summarize(f{"num" ~ length("cyl")})
      check "num" in res
      check res.len == 4
      var resSet = initHashSet[(int, int)]()
      for i, row in res:
        resSet.incl (row["cyl"].toInt.int, row["num"].toInt.int)
      check resSet == exp
      # using `count` directly
      let resDirect = mpg.count("cyl")
      check "n" in resDirect
      check resDirect.len == 4
      var resDirectSet = initHashSet[(int, int)]()
      for i, row in resDirect:
        resDirectSet.incl (row["cyl"].toInt.int, row["n"].toInt.int)
      check resDirectSet == exp

  test "isNull":
    # tests removal of VNull elements in a column with VNull
    let x1 = toSeq(0 .. 100)
    let x2 = toSeq(0 .. 10)
    let df = seqsToDf(x1, x2)
    check df.filter(f{isNull("x2") == false})["x2"].vToSeq == %~ x2
