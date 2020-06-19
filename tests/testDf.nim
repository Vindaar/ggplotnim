import ggplotnim, unittest, sequtils, math, strutils, streams, sugar
import algorithm
import seqmath

suite "Column":
  test "Constant columns":
    let c = constantColumn(12, 100)
    check c.kind == colConstant
    check c.len == 100
    check c.cCol == %~ 12

    for i in 0 ..< c.len:
      check c[i, int] == 12

  test "Adding two equal constant columns":
    let c1 = constantColumn(12, 40)
    let c2 = constantColumn(12, 60)
    check c1.len == 40
    check c1.cCol == %~ 12
    check c2.len == 60
    check c2.cCol == %~ 12

    let res = add(c1, c2)
    check res.kind == colConstant
    check res.cCol == %~ 12
    check res.len == 100

  test "Adding two unequal constant columns of same value type":
    let c1 = constantColumn(12, 40)
    let c2 = constantColumn(14, 60)
    check c1.len == 40
    check c1.cCol == %~ 12
    check c2.len == 60
    check c2.cCol == %~ 14

    let res = add(c1, c2)
    check res.kind == colInt
    check res.len == 100
    for i in 0 ..< 100:
      if i < 40:
        check res[i, int] == 12
      else:
        check res[i, int] == 14

  test "Adding two unequal constant columns of int & float":
    let c1 = constantColumn(12, 40)
    let c2 = constantColumn(14.0, 60)
    check c1.len == 40
    check c1.cCol == %~ 12
    check c2.len == 60
    check c2.cCol == %~ 14.0

    let res = add(c1, c2)
    check res.kind == colFloat
    check res.len == 100
    for i in 0 ..< 100:
      if i < 40:
        check res[i, float] == 12.0
      else:
        check res[i, float] == 14.0

  test "Adding two unequal constant columns of different types":
    let c1 = constantColumn(12, 40)
    let c2 = constantColumn("foo", 60)
    check c1.len == 40
    check c1.cCol == %~ 12
    check c2.len == 60
    check c2.cCol == %~ "foo"

    let res = add(c1, c2)
    check res.kind == colObject
    check res.len == 100
    for i in 0 ..< 100:
      if i < 40:
        check res[i, Value] == %~ 12.0
      else:
        check res[i, Value] == %~ "foo"

  test "Conversion of constant column results to tensor":
    let c = constantColumn(12, 40)
    check c.toTensor(0 .. 10, int) == newTensorWith(11, 12)
    check c.toTensor(int) == newTensorWith(40, 12)

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

  test "Creation of DF w/ int, float other than int64, float64":
    let a = @[123'u8, 12, 55]
    let b = @[1.123'f32, 4.234, 1e12]
    let c = @[1001'i32, 1002, 1003]
    var df = seqsToDf({ "a" : a,
                        "b" : b })
    check df["a"].kind == colInt
    check df["b"].kind == colFloat
    check df["a"].toTensor(int) == a.toTensor.asType(int)
    check df["b"].toTensor(float) == b.toTensor.asType(float)
    # check toColumn directly
    df["c"] = toColumn c
    check df["c"].kind == colInt
    check df["c"].toTensor(int) == c.toTensor.asType(int)

  test "Accessed column of DF is mutable / reference semantics":
    let a = @[123'u8, 12, 55]
    let aRepl = @[123'u8, 12, 33]
    let b = @[1.123'f32, 4.234, 1e12]
    var df = seqsToDf({ "a" : a })
    check df["a"].kind == colInt
    check df["a"].toTensor(int) == a.toTensor.asType(int)
    df["a"][df.high] = 33
    check df["a"].kind == colInt
    check df["a"].toTensor(int) == aRepl.toTensor.asType(int)
    df["a"] = b
    check df["a"].kind == colFloat
    check df["a"].toTensor(float) == b.toTensor.asType(float)

    # check reference semantics
    let bMod = @[1.123'f32, 4.234, 1e4]
    var colB = df["a"]
    # modifying `colB` modifies `df["a"]`
    colB[df.high] = 1e4
    check df["a"].toTensor(float) == bMod.toTensor.asType(float)

    # modifying underlying tensor modifies data too
    let bMod2 = @[1.123'f32, 4.234, 1e6]
    var tensorB = df["a"].toTensor(float)
    tensorB[df.high] = 1e6
    check df["a"].toTensor(float) == bMod2.toTensor.asType(float)

  test "Extending a DF by a column":
    let a = [1, 2, 3]
    let b = [3, 4, 5]
    let c = [4, 5, 6]
    let d = [8, 9, 10]
    block:
      ## NOTE: This "manual" way of adding a column to an existing data frame
      ## is sort of "low level" at the moment. What this means is that the
      ## size of the given sequence is ``not`` checked at the moment. So take
      ## care that you actually hand a sequence of the same length as the DF!
      # create DF of the first 3 seqs
      var df = seqsToDf({ "one" : a,
                          "two" : b,
                          "three" : c })
      check "one" in df
      check "two" in df
      check "three" in df
      check "four" notin df
      # and now add fourth manually
      when defined(defaultBackend):
        df["four"] = toVector(%~ d)
      else:
        df["four"] = d
      check "four" in df

    block:
      ## This version checks the length and fails if they don't match
      # create DF of the first 3 seqs
      var df = seqsToDf({ "one" : a,
                          "two" : b,
                          "three" : c })
      check "one" in df
      check "two" in df
      check "three" in df
      check "four" notin df
      # and now add fourth manually
      df["four"] = d
      check "four" in df
    block:
      # check fails if length is longer
      let e = [1, 2, 3, 4, 5]
      # create DF of the first 3 seqs
      var df = seqsToDf({ "one" : a,
                          "two" : b,
                          "three" : c })
      check "one" in df
      check "two" in df
      check "three" in df
      check "five" notin df
      # and now add fourth manually
      expect(ValueError):
        df["five"] = e
    block:
      # check fails if length is shorter
      let e = [1, 2]
      # create DF of the first 3 seqs
      var df = seqsToDf({ "one" : a,
                          "two" : b,
                          "three" : c })
      check "one" in df
      check "two" in df
      check "three" in df
      check "five" notin df
      # and now add last manually
      expect(ValueError):
        df["five"] = e

    block:
      # check if we can override existing column
      let e = [11, 22, 33]
      # create DF of the first 3 seqs
      var df = seqsToDf({ "one" : a,
                          "two" : b,
                          "three" : c,
                          "four" : c}) # assign four as `c`
      check "one" in df
      check "two" in df
      check "three" in df
      check "four" in df
      # check `"four"` is `c`
      when defined(defaultBackend):
        check df["four"].vToSeq == %~ c
      else:
        check df["four"].toTensor(int) == c.toTensor
      # assign actual `"four"`
      df["four"] = e
      # check `"four"` is now `d`
      when defined(defaultBackend):
        check df["four"].vToSeq == %~ e
      else:
        check df["four"].toTensor(int) == e.toTensor


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
      when defined(defaultBackend):
        check toSeq(res["a"]) == %~ concat(@a, @c)
        check toSeq(res["b"]) == %~ concat(@b, @d)
      else:
        check res["a"].toTensor(int) == concat(a.toTensor(), c.toTensor(), axis = 0)
        check res["b"].toTensor(int) == concat(b.toTensor(), d.toTensor(), axis = 0)
      # without specifying `id`, no column will be added
      #check toSeq(res["id"]) == %~ concat(toSeq(0..<a.len).mapIt("0"),
      #                                      toSeq(0..<c.len).mapIt("1"))

    block:
      # bind_rows with automatic `ids`, having different columns
      let df = seqsToDf({"a" : a, "b" : b})
      let df2 = seqsToDf({"a" : c, "d" : d})
      let res = bind_rows([df, df2])
      when defined(defaultBackend):
        check toSeq(res["a"]) == %~ concat(@a, @c)
        check toSeq(res["b"]) == %~ concat(%~ b, toSeq(0 .. 3).mapIt(Value(kind: VNull)))
        check toSeq(res["d"]) == %~ concat(toSeq(0 .. 2).mapIt(Value(kind: VNull)), %~ d)
      else:
        check res["a"].toTensor(int) == concat(a.toTensor, c.toTensor, axis = 0)
        check res["b"].toTensor(Value) == concat(toTensor(%~ b),
                                                 toTensor(toSeq(0 .. 3).mapIt(Value(kind: VNull))),
                                                 axis = 0)
        check res["d"].toTensor(Value) == concat(toTensor(toSeq(0 .. 2).mapIt(Value(kind: VNull))),
                                                 toTensor(%~ d),
                                                 axis = 0)

      #check toSeq(res["id"]) == %~ concat(toSeq(0..<a.len).mapIt("0"),
      #                                      toSeq(0..<c.len).mapIt("1"))

    block:
      # bind_rows with custom `id` name, both having same columns
      let df = seqsToDf({"a" : a, "b" : b})
      let df2 = seqsToDf({"a" : c, "b" : d})
      let res = bind_rows([df, df2], id = "combine")
      when defined(defaultBackend):
        check toSeq(res["a"]) == %~ concat(@a, @c)
        check toSeq(res["b"]) == %~ concat(@b, @d)
        check toSeq(res["combine"]) == %~ concat(toSeq(0..<a.len).mapIt("0"),
                                                   toSeq(0..<c.len).mapIt("1"))
      else:
        check res["a"].toTensor(int) == concat(a.toTensor(), c.toTensor(), axis = 0)
        check res["b"].toTensor(int) == concat(b.toTensor(), d.toTensor(), axis = 0)
        check res["combine"].toTensor(string) == concat(toTensor(toSeq(0..<a.len).mapIt("0")),
                                                        toTensor(toSeq(0..<c.len).mapIt("1")),
                                                        axis = 0)


    block:
      # bind_rows with custom `id` name, custom `id` values, both having same columns
      let df = seqsToDf({"a" : a, "b" : b})
      let df2 = seqsToDf({"a" : c, "b" : d})
      let res = bind_rows([("one", df), ("two", df2)], id = "combine")
      when defined(defaultBackend):
        check toSeq(res["a"]) == %~ concat(@a, @c)
        check toSeq(res["b"]) == %~ concat(@b, @d)
        check toSeq(res["combine"]) == %~ concat(toSeq(0..<a.len).mapIt("one"),
                                               toSeq(0..<c.len).mapIt("two"))
      else:
        check res["a"].toTensor(int) == concat(a.toTensor, c.toTensor, axis = 0)
        check res["b"].toTensor(int) == concat(b.toTensor, d.toTensor, axis = 0)
        check res["combine"].toTensor(string) == concat(toTensor(toSeq(0..<a.len).mapIt("one")),
                                                        toTensor(toSeq(0..<c.len).mapIt("two")),
                                                        axis = 0)

  test "Group by":
    proc almostEqual(x, y: float, eps = 1e-6): bool =
      result = (x - y) < eps

    var mpg = toDf(readCsv("data/mpg.csv"))

    let mpggroup = mpg.group_by("cyl")

    # TODO: make this to `doAssert`!
    let summary = mpg.summarize(f{float: "mean_cyl" << mean(c"cyl")},
                                f{float: "mean_hwy" << mean(c"hwy")})
    when defined(defaultBackend):
      check almostEqual(summary["mean_cyl"][0].toFloat, 5.88889)
      check almostEqual(summary["mean_hwy"][0].toFloat, 23.4402)
    else:
      check almostEqual(summary["mean_cyl"][0, float], 5.88889)
      check almostEqual(summary["mean_hwy"][0, float], 23.4402)

    let sum_grouped = mpggroup.summarize(f{float: "mean_displ" << mean(c"displ")},
                                         f{float: "mean_hwy" << mean(c"hwy")})
      .arrange("cyl")
    check sum_grouped.len == 4
    when defined(defaultBackend):
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
    else:
      check sum_grouped["cyl"][0, int] == 4
      check sum_grouped["cyl"][1, int] == 5
      check sum_grouped["cyl"][2, int] == 6
      check sum_grouped["cyl"][3, int] == 8
      check almostEqual(sum_grouped["mean_displ"][0, float], 2.14568)
      check almostEqual(sum_grouped["mean_displ"][1, float], 2.5)
      check almostEqual(sum_grouped["mean_displ"][2, float], 3.40886)
      check almostEqual(sum_grouped["mean_displ"][3, float], 5.13286)
      check almostEqual(sum_grouped["mean_hwy"][0, float], 28.8025)
      check almostEqual(sum_grouped["mean_hwy"][1, float], 28.75)
      check almostEqual(sum_grouped["mean_hwy"][2, float], 22.8228)
      check almostEqual(sum_grouped["mean_hwy"][3, float], 17.6286)

    let mpg2groups = mpggroup.group_by("class", add = true)
    let classes = mpg["class"].unique
    let cyls = mpg["cyl"].unique
    let product = product([classes.toTensor(Value).toRawSeq,
                           cyls.toTensor(Value).toRawSeq])
    var subgroupCount = 0
    for (by, df) in groups(mpg2groups):
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


    let cylFiltered = mpg.filter(f{c"cyl" == 4})
    check cylFiltered.len == 81
    let cylDrvFiltered = cylFiltered.filter(f{c"drv" == "4"})
    check cylDrvFiltered.len == 23

  test "Unequal":
    let mpg = toDf(readCsv("data/mpg.csv"))

    let mpgNoSuv = mpg.filter(f{`class` != "suv"})
    check "suv" notin mpgNoSuv["class"].unique

  test "Filter - two comparisons using `and`":
    let x = toSeq(0 .. 100)
    let df = seqsToDf(x)
    let dfFilter = df.filter(f{c"x" >= 50 and
                               c"x" <= 75})
    when defined(defaultBackend):
      check dfFilter["x"].vToSeq == %~ toSeq(50 .. 75)
    else:
      check dfFilter["x"].toTensor(int) == toTensor toSeq(50 .. 75)

  test "Filter - comparisons using function":
    let x = toSeq(0 .. 100)
    let df = seqsToDf(x)
    let dfFilter = df.filter(f{float: c"x" >= max(c"x") * 0.5})
    when defined(defaultBackend):
      check dfFilter["x"].vToSeq == %~ toSeq(50 .. 100)
    else:
      check dfFilter["x"].toTensor(int) == toTensor toSeq(50 .. 100)

  test "Filter - data types":
    let x = toSeq(0 .. 100)
    let df = seqsToDf(x)
    let dfFiltered = df.filter(f{float: c"x" >= max(c"x") * 0.5})
    check dfFiltered["x"].kind == colInt
    let dfReduced1 = df.summarize(f{int: max(c"x")})
    check dfReduced1["(max x)"].kind == colInt
    let dfReduced2 = df.summarize(f{float: max(c"x")})
    check dfReduced2["(max x)"].kind == colFloat

  test "Transmute - float arithmetic":
    let x = toSeq(0 ..< 100)
    let y = x.mapIt(sin(it.float))
    let y2 = x.mapIt(pow(sin(it.float), 2.0))
    let df = seqsToDf(x, y)
    check df.len == 100
    let dfTrans = df.transmute(f{"x"}, f{"y2" ~ c"y" * c"y"})
    check "y" notin dfTrans
    check "y2" in dfTrans
    check "x" in dfTrans
    when defined(defaultBackend):
      check dfTrans["y2"].vToSeq == %~ y2
    else:
      check dfTrans["y2"].toTensor(float) == toTensor y2

  test "Transmute - parse floats in dataframe from string column":
    let x = toSeq(0 ..< 100)
    let y = x.mapIt($sin(it.float))
    let yFloat = x.mapIt(sin(it.float))
    let df = seqsToDf(x, y)
    check df.len == 100
    when defined(defaultBackend):
      liftScalarStringProc(parseFloat, toExport = false)
      let dfTrans = df.transmute(f{"x"}, f{"yFloat" ~ parseFloat("y")})
    else:
      let dfTrans = df.transmute(f{"x"},
                                 f{string -> float: "yFloat" ~ parseFloat(df["y"][idx])})
    check "y" notin dfTrans
    check "yFloat" in dfTrans
    check "x" in dfTrans
    when defined(defaultBackend):
      check dfTrans["yFloat"].vToSeq == %~ yFloat
    else:
      let trans = dfTrans["yFloat"].toTensor(float)
      let exp = toTensor yFloat
      for i in 0 ..< trans.len:
        check almostEqual(trans[i], exp[i])

  test "Gather - 2 columns":
    let x = toSeq(0 ..< 100)
    let y1 = x.mapIt(sin(it.float))
    let y2 = x.mapIt(sin(it.float - PI / 2.0) - 0.5)
    let yComb = concat(y1, y2)
    let df = seqsToDf(x, y1, y2)
    check df.len == 100
    let dfLong = df.gather(["y1", "y2"], key = "from", value = "y")
    check dfLong.len == 200
    when defined(defaultBackend):
      check dfLong["from"].unique == %~ @["y1", "y2"]
      check dfLong["y"].vToSeq == %~ yComb
    else:
      check dfLong["from"].unique.toTensor(string) == toTensor @["y1", "y2"]
      check dfLong["y"].toTensor(float) == toTensor(yComb)
    let dfY1FromLong = dfLong.filter(f{c"from" == "y1"})
    let dfY2FromLong = dfLong.filter(f{c"from" == "y2"})
    when defined(defaultBackend):
      check dfY1FromLong["y"].vToSeq == df["y1"].vToSeq
      check dfY2FromLong["y"].vToSeq == df["y2"].vToSeq
      check dfY1FromLong["x"].vToSeq == df["x"].vToSeq
      check dfY2FromLong["x"].vToSeq == df["x"].vToSeq
    else:
      check dfY1FromLong["y"].toTensor(float) == df["y1"].toTensor(float)
      check dfY2FromLong["y"].toTensor(float) == df["y2"].toTensor(float)
      check dfY1FromLong["x"].toTensor(float) == df["x"].toTensor(float)
      check dfY2FromLong["x"].toTensor(float) == df["x"].toTensor(float)

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
    when defined(defaultBackend):
      check dfLong["from"].unique == %~ @["y1", "y2", "y3"]
      check dfLong["y"].vToSeq == %~ yComb
    else:
      check dfLong["from"].unique.toTensor(string) == toTensor @["y1", "y2", "y3"]
      check dfLong["y"].toTensor(float) == toTensor yComb
    let dfY1FromLong = dfLong.filter(f{c"from" == "y1"})
    let dfY2FromLong = dfLong.filter(f{c"from" == "y2"})
    let dfY3FromLong = dfLong.filter(f{c"from" == "y3"})
    when defined(defaultBackend):
      check dfY1FromLong["y"].vToSeq == df["y1"].vToSeq
      check dfY2FromLong["y"].vToSeq == df["y2"].vToSeq
      check dfY3FromLong["y"].vToSeq == df["y3"].vToSeq
      check dfY1FromLong["x"].vToSeq == df["x"].vToSeq
      check dfY2FromLong["x"].vToSeq == df["x"].vToSeq
      check dfY3FromLong["x"].vToSeq == df["x"].vToSeq
    else:
      check dfY1FromLong["y"].toTensor(float) == toTensor(df["y1"], float)
      check dfY2FromLong["y"].toTensor(float) == toTensor(df["y2"], float)
      check dfY3FromLong["y"].toTensor(float) == toTensor(df["y3"], float)
      check dfY1FromLong["x"].toTensor(float) == toTensor(df["x"], float)
      check dfY2FromLong["x"].toTensor(float) == toTensor(df["x"], float)
      check dfY3FromLong["x"].toTensor(float) == toTensor(df["x"], float)


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
    when defined(defaultBackend):
      check dfLong["from"].unique == %~ @["y1", "yStr"]
      check dfLong["y"].vToSeq == yComb
    else:
      check dfLong["from"].unique.toTensor(string) == toTensor @["y1", "yStr"]
      check dfLong["y"].toTensor(Value) == toTensor yComb
    let dfY1FromLong = dfLong.filter(f{c"from" == "y1"})
    let dfYSTRFromLong = dfLong.filter(f{c"from" == "yStr"})
    when defined(defaultBackend):
      check dfY1FromLong["y"].vToSeq == df["y1"].vToSeq
      check dfYSTRFromLong["y"].vToSeq == df["yStr"].vToSeq
      check dfY1FromLong["x"].vToSeq == df["x"].vToSeq
      check dfYSTRFromLong["x"].vToSeq == df["x"].vToSeq
    else:
      check dfY1FromLong["y"].toTensor(float) == df["y1"].toTensor(float)
      check dfYSTRFromLong["y"].toTensor(string) == df["yStr"].toTensor(string)
      check dfY1FromLong["x"].toTensor(float) == df["x"].toTensor(float)
      check dfYSTRFromLong["x"].toTensor(float) == df["x"].toTensor(float)

  test "Gather - dropping null values":
    ## check that it works for 3 columns too
    let x = toSeq(0 ..< 100)
    var
      y1: seq[float]
      y2: seq[Value]
      x2s: seq[int]
    for i, val in x:
      y1.add sin(val.float)
      if val mod 3 == 0:
        y2.add (%~ (sin(val.float - PI / 2.0) - 0.5))
        x2s.add i
      else:
        y2.add Value(kind: VNull)
    let df = seqsToDf(x, y1, y2)
    let gathered = df.gather(["y1", "y2"], dropNulls = false)
    let onlyy2 = gathered.filter(f{Value: isNull(df["value"][idx]).toBool == false and
                                  c"key" == %~ "y2"})
    when defined(defaultBackend):
      check onlyy2["x"].vToSeq == %~ x2s
    else:
      check onlyy2["x"].toTensor(int) == toTensor x2s
    check onlyy2.len == x2s.len

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
    when defined(defaultBackend):
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
    else:
      let defaultExp = """
       Idx         x         y
    dtype:     float     float
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
    when defined(defaultBackend):
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
    else:
      let expPrecision12 = """
               Idx                 x                 y
            dtype:             float             float
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
    let csvDataStream = newStringStream("""
t_in_s,  C1_in_V,  C2_in_V,  type
-3.0000E-06,  -2.441E-04,  -6.836E-04,  T1
-2.9992E-06,  2.441E-04,  -6.836E-04 ,  T1
-2.9984E-06,  1.025E-03,  -8.789E-04 ,  T1
-2.9976E-06,  1.025E-03,  -2.930E-04 ,  T1
-2.9968E-06,  9.277E-04,  2.930E-04  ,  T2
-2.9960E-06,  4.395E-04,  4.883E-04  ,  T2
-2.9952E-06,  1.465E-04,  -2.930E-04 ,  T2
-2.9944E-06,  -3.418E-04,  -1.270E-03,  T2
""")
    let csvRead = readCsv(csvDataStream)
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
    when defined(defaultBackend):
      check df["t_in_s"].vToSeq == dfExp["t_in_s"].vToSeq
      check df["C1_in_V"].vToSeq == dfExp["C1_in_V"].vToSeq
      check df["C2_in_V"].vToSeq == dfExp["C2_in_V"].vToSeq
      check df["type"].vToSeq == dfExp["type"].vToSeq
    else:
      check df["t_in_s"].toTensor(float) == dfExp["t_in_s"].toTensor(float)
      check df["C1_in_V"].toTensor(float) == dfExp["C1_in_V"].toTensor(float)
      check df["C2_in_V"].toTensor(float) == dfExp["C2_in_V"].toTensor(float)
      check df["type"].toTensor(string) == dfExp["type"].toTensor(string)

  test "Summarize":
    let mpg = toDf(readCsv("data/mpg.csv"))
    block:
      # explicit LHS
      let res = mpg.summarize(f{int: "num" << sum(c"cyl")})
      check "num" in res
      check res.len == 1
      check res["num", 0] == %~ 1378
      # implicit LHS
      let resImplicit = mpg.summarize(f{int: sum(c"cyl")})
      let fname = "(sum cyl)"
      check fname in resImplicit
      check resImplicit.len == 1
      check resImplicit[fname, 0] == %~ 1378
    block:
      # explicit LHS
      let res = mpg.summarize(f{float: "mean" << mean(c"cyl")})
      check "mean" in res
      check res.len == 1
      check almostEqual(res["mean", 0].toFloat, 5.888888889)
      # implicit LHS
      let resImplicit = mpg.summarize(f{float: mean(c"cyl")})
      let fname = "(mean cyl)"
      check fname in resImplicit
      check resImplicit.len == 1
      check almostEqual(resImplicit[fname, 0].toFloat, 5.888888889)
    block:
      # summarize multiple groups at the same time
      let res = mpg.group_by(["class", "cyl"]).summarize(f{float: mean(c"hwy")})
      check res.len == 19
      # expected numbers. They seem reasonable, but ``I did NOT`` check them
      # manually!!
      # hence another test below with known numbers and their sum
      let exp = @[24.8, 24.8, 29.47, 29.47, 29, 29, 25.31, 25.31, 29.19, 29.19, 26.26, 26.26, 24, 24, 24, 24, 22.2, 22.2, 20.67, 20.67, 17.9, 17.9, 15.8, 15.8, 30.81, 30.81, 28.5, 28.5, 24.71, 24.71, 21.6, 21.6, 23.75, 23.75, 18.5, 18.5, 16.79, 16.79]
      when defined(defaultBackend):
        let resSet = res[$f{mean("hwy")}].vToSeq.toHashSet
        let expValSet = exp.mapIt(%~ it).toHashSet
        check resSet == expValSet
      else:
        let resSet = res[$f{float: mean(c"hwy")}].toTensor(float).map(x => x.round(2)).toHashSet
        let expSet = exp.toHashSet
        check resSet == expSet
    block:
      # generate numbers
      let num = toSeq(1 .. 100)
      let numVec = repeat(num, 26).flatten
      let sumNum = num.sum
      let lab1 = toSeq({'a'..'z'}).mapIt($it)
      let lab2 = toSeq({'A'..'Z'}).mapIt($it)
      var l1 = newSeq[string]()
      var l2 = newSeq[string]()
      var count = 0
      for j in 0 ..< lab1.len:
        for i in 0 ..< num.len:
          l1.add lab1[j]
          l2.add lab2[j]
          inc count
      check count == 2600
      let df = seqsToDf(l1, l2, numVec)
      let dfG = df.group_by(["l1", "l2"]).summarize(f{int: sum(c"numVec")})
      check dfG.len == 26
      check sumNum == 5050
      when defined(defaultBackend):
        for i, el in dfG[$f{sum("numVec")}].vToSeq:
          check el == %~ sumNum
      else:
        for el in dfG[$f{int: sum(c"numVec")}].toTensor(Value):
          check el == %~ sumNum

  test "Count":
    # count elements by group. Useful combination of group_by and summarize(len)
    let mpg = toDf(readCsv("data/mpg.csv"))
    # in manual case the order is not preserved, due to `summarize` impl!
    let exp = toHashSet({6 : 79, 8 : 70, 4 : 81, 5 : 4})
    block:
      # manually
      let res = mpg.group_by("cyl").summarize(f{int: "num" << c"cyl".len})
      check "num" in res
      check res.len == 4
      var resSet = initHashSet[(int, int)]()
      for row in res:
        resSet.incl (row["cyl"].toInt.int, row["num"].toInt.int)
      check resSet == exp
      # using `count` directly
      let resDirect = mpg.count("cyl")
      check "n" in resDirect
      check resDirect.len == 4
      var resDirectSet = initHashSet[(int, int)]()
      for row in resDirect:
        resDirectSet.incl (row["cyl"].toInt.int, row["n"].toInt.int)
      check resDirectSet == exp

  test "isNull":
    # tests removal of VNull elements in a column with VNull
    let x1 = toSeq(0 .. 100)
    let x2 = toSeq(0 .. 10)
    let df = seqsToDf(x1, x2)
    when defined(defaultBackend):
      check df.filter(f{isNull("x2") == false})["x2"].vToSeq == %~ x2
    else:
      check df.filter(f{Value: isNull(df["x2"][idx]).toBool == false})["x2"].toTensor(Value) == toTensor (%~ x2)

  test "Unique - duplicates using all columns":
    # given some data containing duplicates
    let dataDuplStream = newStringStream("""
t_in_s,  C1_in_V,  C2_in_V,  type
-3.0000E-06,  -2.441E-04,  -6.836E-04,  T1
-2.9992E-06,  2.441E-04,  -6.836E-04 ,  T1
-2.9984E-06,  1.025E-03,  -8.789E-04 ,  T1
-2.9976E-06,  1.025E-03,  -2.930E-04 ,  T1
-2.9992E-06,  2.441E-04,  -6.836E-04 ,  T1
-2.9984E-06,  1.025E-03,  -8.789E-04 ,  T1
-2.9976E-06,  1.025E-03,  -2.930E-04 ,  T1
-2.9968E-06,  9.277E-04,  2.930E-04  ,  T2
""")
    let df = toDf(readCsv(dataDuplStream))
    check df.len == 8
    let dfUnique = df.unique
    check dfUnique.len == 5

  test "Unique - duplicates using subset of columns":
    let s1 = @[1, 2, 3, 4, 5]
    let s2 = @["A", "E", "A", "D", "E"]
    let s3 = @["B", "G", "B", "G", "X"]
    let df = seqsToDF({ "id" : s1,
                        "Start" : s2,
                        "Stop" : s3 })
    check df.len == 5
    let dfUniqueAll = df.unique
    check dfUniqueAll.len == 5
    # now only use columns start and stop
    let dfUnique = df.unique("Start", "Stop")
    check dfUnique.len == 4

  test "setDiff":
    # remove duplicates of `mpg` (for some reason there are 9 duplicates..)
    let mpg = toDf(readCsv("data/mpg.csv")).unique
    let mpgS1 = mpg[0 .. 25]
    let mpgS2 = mpg[20 .. 29]
    block:
      # S1 is primary
      let exp = mpg[0 .. 19].arrange(toSeq(keys(mpg)))
      let res = setDiff(mpgS1, mpgS2).arrange(toSeq(keys(mpg)))
      check exp.len == res.len
      for i in 0 ..< exp.len:
        check row(exp, i) == row(res, i)
    block:
      # S2 is primary
      let exp = mpg[26 .. 29].arrange(toSeq(keys(mpg)))
      let res = setDiff(mpgS2, mpgS1).arrange(toSeq(keys(mpg)))
      check exp.len == res.len
      for i in 0 ..< exp.len:
        check row(exp, i) == row(res, i)
    block:
      # symmetric difference
      let exp = bind_rows(mpg[0 .. 19], mpg[26 .. 29], id = "")
        .arrange(toSeq(keys(mpg)))
      let res = setDiff(mpgS1, mpgS2, symmetric = true).arrange(toSeq(keys(mpg)))
      check exp.len == res.len
      for i in 0 ..< exp.len:
        check row(exp, i) == row(res, i)

  test "Custom column names when reading CSV like data":
    # given some data without a header and column names
    let dataDuplStream = newStringStream("""
-3.0000E-06,  -2.441E-04,  -6.836E-04,  T1
-2.9992E-06,  2.441E-04,  -6.836E-04 ,  T1
-2.9984E-06,  1.025E-03,  -8.789E-04 ,  T1
""")
    # define columns
    let cols = @["V1", "V2", "V3", "Channel"]
    let df = toDf(readCsv(dataDuplStream, colNames = cols))
    check df.len == 3
    check df.getKeys.sorted == cols.sorted

  test "Column names containing numbers":
    # given some data without a header and column names
    let dataDuplStream = newStringStream("""
-3.0000E-06,  -2.441E-04,  -6.836E-04,  T1
-2.9992E-06,  2.441E-04,  -6.836E-04 ,  T1
-2.9984E-06,  1.025E-03,  -8.789E-04 ,  T1
""")
    # define columns
    let cols = @["0", "1", "2", "3"]
    let colsNot = @["\"0\"", "\"1\"", "\"2\"", "\"3\""]
    let df = toDf(readCsv(dataDuplStream, colNames = cols))
    check df.len == 3
    check df.getKeys.sorted == cols.sorted
    # redundant but a showcase what happened previously
    for k in zip(df.getKeys, colsNot):
      check k[0] != k[1]

  test "Evaluate data frame using FormulaNode":
    let mpg = toDf(readCsv("data/mpg.csv"))
    let f = f{`hwy` ~ (`displ` + `cyl` - `cty`)} # this doesn't make sense, but anyways...
    # Displacement + Cylinders - City mpg. Yeah :D
    # use RHS of formula for calculation of 0 row.
    when defined(defaultBackend):
      check f.rhs.evaluate(mpg, 0) == %~ -12.2
    else:
      # not exactly possible on arraymancer backend
      check f.evaluate(mpg)[0, Value] == %~ -12.2

    # applying negative column results in expected
    # stringifaction of the formula
    let dfNeg = mpg.clone.transmute(f{-1 * c"hwy"})
    check "(* -1 hwy)" == getKeys(dfNeg)[0]

    # negative prefix of existing column results in what we expect
    when defined(defaultBackend):
      check evaluate(f{-"hwy"}, mpg).vToSeq == mpg["hwy"].vToSeq.mapIt((%~ -1) * it)
      # evaluate non existant key to vector of constant
      check evaluate(f{"nonExistant"}, mpg).vToSeq == toSeq(0 ..< mpg.len).mapIt(%~ "nonExistant")
      # evaluate formula without column on DF
      check evaluate(f{1 + 2}, mpg).vToSeq == toSeq(0 ..< mpg.len).mapIt(%~ 3)
    else:
      check evaluate(f{-1 * c"hwy"}, mpg).toTensor(float) == mpg["hwy"].toTensor(float).map(x => -x)
      # evaluate non existant key to vector of constant
      check evaluate(f{"nonExistant"}, mpg).toTensor(string) == toTensor toSeq(0 ..< mpg.len).mapIt("nonExistant")
      # evaluate formula without column on DF
      check evaluate(f{1 + 2}, mpg).toTensor(int) == toTensor toSeq(0 ..< mpg.len).mapIt(3)

  test "Reduce data frame using FormulaNode":
    let mpg = toDf(readCsv("data/mpg.csv"))
    # check reduction via a formula and VectorFloatProc
    check almostEqual(reduce(f{float: mean(c"hwy")}, mpg).toFloat, 23.44017, 1e-3)

    # combine with calculation
    check almostEqual(reduce(f{float: 235 / mean(c"hwy")}, mpg).toFloat, 10.0255, 1e-3)

  test "Allow `add` if first argument is still uninitialized":
    # uninitialized data frame (DataFrame is ref object)
    var df: DataFrame
    check df.isNil
    let dfToAdd = seqsToDf({ "x" : @[1, 2, 3],
                             "y" : @[4, 5, 6] })
    df.add dfToAdd
    check df == dfToAdd
    check dfToAdd["x"].toTensor(int) == [1, 2, 3].toTensor
    check dfToAdd["y"].toTensor(int) == [4, 5, 6].toTensor

  test "Inner join - fully qualified":
    let idents = @["A", "B", "C", "D"]
    let ids = @[1, 2, 3, 4]
    let words = @["suggest", "result", "from", "to"]
    let df1 = seqsToDf({ "Ident" : idents,
                         "Ids" : ids})
    let df2 = seqsToDf({ "Ident" : idents,
                         "Words" : words })
    let dfExp = seqsToDf({ "Ident" : idents,
                           "Words" : words,
                           "Ids" : ids })
    let dfRes = df1.innerJoin(df2, by = "Ident")
    check dfRes.len == dfExp.len
    check dfRes.getKeys == dfExp.getKeys
    check dfRes["Ident"].toTensor(string) == dfExp["Ident"].toTensor(string)
    check dfRes["Ids"].toTensor(int) == dfExp["Ids"].toTensor(int)
    check dfRes["Words"].toTensor(string) == dfExp["Words"].toTensor(string)

  test "Inner join - int & float column":
    let idents = @["A", "B", "C", "D"]
    let ids = @[1, 2, 3, 4]
    let idsFloat = @[1'f64, 2, 3, 4]
    let words = @["suggest", "result", "from", "to"]
    let df1 = seqsToDf({ "Ident" : idents,
                         "Ids" : ids})
    let df2 = seqsToDf({ "Ident" : idents,
                         "Ids" : idsFloat,
                         "Words" : words})
    let dfExp = seqsToDf({ "Ident" : idents,
                           "Words" : words,
                           "Ids" : idsFloat })
    let dfRes = df1.innerJoin(df2, by = "Ident")
    check dfRes.len == dfExp.len
    check dfRes.getKeys == dfExp.getKeys
    check dfRes["Ident"].toTensor(string) == dfExp["Ident"].toTensor(string)
    # result has enveloping column kind float
    check dfRes["Ids"].kind == colFloat
    check dfRes["Ids"].toTensor(float) == dfExp["Ids"].toTensor(float)
    check dfRes["Words"].toTensor(string) == dfExp["Words"].toTensor(string)

  test "Inner join - missing elements":
    let idents = @["A", "B", "C", "D", "E"]
    let ids = @[1, 2, 3, 4, 5]
    let idsFloat = @[1'f64, 2, 3, 4]
    let words = @["suggest", "result", "from", "to"]
    let df1 = seqsToDf({ "Ident" : idents,
                         "Ids" : ids})
    let df2 = seqsToDf({ "Ident" : idents[0 ..< ^1],
                         "Ids" : idsFloat,
                         "Words" : words})
    let dfExp = seqsToDf({ "Ident" : idents[0 ..< ^1],
                           "Words" : words,
                           "Ids" : idsFloat })
    let dfRes = df1.innerJoin(df2, by = "Ident")
    check dfRes.len == dfExp.len
    check dfRes.getKeys == dfExp.getKeys
    check dfRes["Ident"].toTensor(string) == dfExp["Ident"].toTensor(string)
    # result has enveloping column kind float
    check dfRes["Ids"].kind == colFloat
    check dfRes["Ids"].toTensor(float) == dfExp["Ids"].toTensor(float)
    check dfRes["Words"].toTensor(string) == dfExp["Words"].toTensor(string)

  test "Convert (one typed) object column to native":
    let a = @["A", "B", "C", "D", "E"]
    let b = @[1, 2, 3, 4, 5]
    let c = @[1.1, 1.2, 1.3, 1.5]
    let d = @[true, true, false, true]
    let aCol = toColumn(%~ a)
    let bCol = toColumn(%~ b)
    let cCol = toColumn(%~ c)
    let dCol = toColumn(%~ d)
    check aCol.kind == colObject
    check bCol.kind == colObject
    check cCol.kind == colObject
    check dCol.kind == colObject
    check aCol.toNativeColumn.kind == colString
    check bCol.toNativeColumn.kind == colInt
    check cCol.toNativeColumn.kind == colFloat
    check dCol.toNativeColumn.kind == colBool

  test "Convert multi typed (real object) column to native fails":
    let a = @["A", "B", "C", "D", "E"]
    let b = @[1, 2, 3, 4, 5]
    let ab = concat(%~ a, %~ b)
    let c = @[1.1, 1.2, 1.3, 1.5]
    let d = @[true, true, false, true]
    let cd = concat(%~ c, %~ d)
    let abCol = toColumn(%~ ab)
    let cdCol = toColumn(%~ cd)
    check abCol.kind == colObject
    check cdCol.kind == colObject
    try:
      # This actually works because ints can be converted to string!
      # that's not really desired behavior, is it?
      check abCol.toNativeColumn.kind == colString
      check false
    except AssertionError:
      check true
    try:
      check cdCol.toNativeColumn.kind == colFloat
      check false
    except AssertionError:
      check true

  test "Remove 'null' values from DF":
    let idents = @["A", "B", "C", "D", "E"]
    let ids = @[1, 2, 3, 4, 5]
    let ages = @[43, 27, 32, 43]
    let cities = @[%~ "NYC", %~ "London", %~ "Sydney", Value(kind: VNull),
                   %~ "Berlin"]
    let df = seqsToDf({ "Ident" : idents,
                        "Id" : ids,
                        "Age" : ages,
                        "City" : cities})
    # now check for:
    # -> filter by each individual column
    # -> filter by both columns in one call
    let dfExp1 = seqsToDf({ "Ident" : idents[0 ..< ^1],
                            "Id" : ids[0 ..< ^1],
                            "Age" : ages,
                            "City" : cities[0 ..< ^1] })
    let dfExp2 = seqsToDf({ "Ident" : @["A", "B", "C", "E"],
                            "Id" : @[1, 2, 3, 5],
                            "Age" : @[%~ 43, %~ 27, %~ 32, Value(kind: VNull)],
                            "City" : cities.filterIt(it.kind != VNull) })
    let dfExp3 = seqsToDf({ "Ident" : @["A", "B", "C"],
                            "Id" : @[1, 2, 3],
                            "Age" : %~ @[43, 27, 32],
                            "City" : %~ cities[0 ..< ^2]})
    let dfRes1 = df.drop_null("Age")
    check dfRes1["Age"].kind == colObject
    check dfRes1["City"].kind == colObject
    let dfRes2 = df.drop_null("City")
    check dfRes2["Age"].kind == colObject
    check dfRes2["City"].kind == colObject
    let dfRes3 = df.drop_null()
    check dfRes3["Age"].kind == colObject
    check dfRes3["City"].kind == colObject
    let keys = getKeys(df)
    for k in keys:
      check dfRes1[k].toTensor(Value) == dfExp1[k].toTensor(Value)
      check dfRes2[k].toTensor(Value) == dfExp2[k].toTensor(Value)
      check dfRes3[k].toTensor(Value) == dfExp3[k].toTensor(Value)

    # convert manually to correct dtypes
    check dfRes1["Age"].toNativeColumn.kind == colInt
    expect(AssertionDefect):
      check dfRes1["City"].toNativeColumn.kind == colString
    check dfRes1["City"].toNativeColumn(failIfImpossible = false).kind == colObject

    check dfRes2["City"].toNativeColumn.kind == colString
    expect(AssertionDefect):
      check dfRes2["Age"].toNativeColumn.kind == colInt
    check dfRes2["Age"].toNativeColumn(failIfImpossible = false).kind == colObject

    check dfRes3["Age"].toNativeColumn.kind == colInt
    check dfRes3["City"].toNativeColumn.kind == colString
