import ggplotnim, unittest, sequtils

var mpg = toDf(readCsv("data/mpg.csv"))

let mpggroup = mpg.group_by("cyl")

echo mpg.summarize(f{"mean_cyl" ~ mean("cyl")},
                   f{"mean_hwy" ~ mean("hwy")})
echo "----"
echo mpggroup.summarize(f{"mean_displ" ~ mean("displ")},
                        f{"mean_hwy" ~ mean("hwy")})


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
    doAssert toSeq(res["a"]) == % concat(@a, @c)
    doAssert toSeq(res["b"]) == % concat(@b, @d)
    doAssert toSeq(res["id"]) == % concat(toSeq(0..<a.len).mapIt("0"),
                                          toSeq(0..<c.len).mapIt("1"))

  block:
    echo "-------"
    # bind_rows with automatic `ids`, having different columns
    let df = seqsToDf({"a" : a, "b" : b})
    let df2 = seqsToDf({"a" : c, "d" : d})
    let res = bind_rows([df, df2])
    echo "Resulting df "
    echo res
    echo res["a"]
    echo toVector(% concat(@a, @c))
    doAssert toSeq(res["a"]) == % concat(@a, @c)
    doAssert toSeq(res["b"]) == % concat(% b, toSeq(0 .. 3).mapIt(Value(kind: VNull)))
    doAssert toSeq(res["d"]) == % concat(toSeq(0 .. 2).mapIt(Value(kind: VNull)), % d)
    doAssert toSeq(res["id"]) == % concat(toSeq(0..<a.len).mapIt("0"),
                                          toSeq(0..<c.len).mapIt("1"))

  block:
    echo "-------"
    # bind_rows with custom `id` name, both having same columns
    let df = seqsToDf({"a" : a, "b" : b})
    let df2 = seqsToDf({"a" : c, "b" : d})
    let res = bind_rows([df, df2], id = "combine")
    echo "Resulting df "
    echo res
    doAssert toSeq(res["a"]) == % concat(@a, @c)
    doAssert toSeq(res["b"]) == % concat(@b, @d)
    doAssert toSeq(res["combine"]) == % concat(toSeq(0..<a.len).mapIt("0"),
                                               toSeq(0..<c.len).mapIt("1"))

  block:
    echo "-------"
    # bind_rows with custom `id` name, custom `id` values, both having same columns
    let df = seqsToDf({"a" : a, "b" : b})
    let df2 = seqsToDf({"a" : c, "b" : d})
    let res = bind_rows([("one", df), ("two", df2)], id = "combine")
    echo "Resulting df "
    echo res
    doAssert toSeq(res["a"]) == % concat(@a, @c)
    doAssert toSeq(res["b"]) == % concat(@b, @d)
    doAssert toSeq(res["combine"]) == % concat(toSeq(0..<a.len).mapIt("one"),
                                               toSeq(0..<c.len).mapIt("two"))
