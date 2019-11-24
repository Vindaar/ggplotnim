import ggplotnim, unittest, sequtils

test "Issue #5 - ragged dataframes":
  let x1 = toSeq(0 ..< 2560)
  let x2 = toSeq(0 ..< 10)
  let df = seqsToDf({ "x1" : x1,
                      "y1": x1.mapIt(it.float * 1.5),
                      "x2": x2,
                      "y2" : x2.mapIt(it.float * 2.2)})
  check df.len == 2560
  for k in keys(df):
    check df[k].len == 2560
  check df["x2", 10 ..< 2560] == %~ toSeq(0 ..< 2550).mapIt(Value(kind: VNull))
  check df["y2", 10 ..< 2560] == %~ toSeq(0 ..< 2550).mapIt(Value(kind: VNull))
