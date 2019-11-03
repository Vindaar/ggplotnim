import ggplotnim, unittest, sequtils
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
