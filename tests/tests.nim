import unittest
# we include ggplotnim so that we can test non exported procs
include ../src/ggplotnim

import tables, sets
import sequtils
import math

suite "Value":
  let
    v1 = %~ 1
    v2 = %~ 1.5
    v3 = %~ true
    v4 = %~ 'a'
    # `v5` itself is already a test, whether we can hash `Value`
    v5 = %~ { "test" : v1,
              "some" : v2,
              "heterogeneous" : v3,
              "fields" : v4 }.toOrderedTable
    v6 = Value(kind: VNull)

  test "Storing in sets":
    var valueSet = initHashSet[Value]()
    valueSet.incl v1
    valueSet.incl v2
    valueSet.incl v3
    valueSet.incl v4
    valueSet.incl v5
    valueSet.incl v6
    check v1 in valueSet
    check v2 in valueSet
    check v3 in valueSet
    check v4 in valueSet
    check v5 in valueSet
    check v6 in valueSet
    check valueSet.card == 6

  test "Storing in tables":
    var tab = initTable[string, Value]()
    tab["v1"] = v1
    tab["v2"] = v2
    tab["v3"] = v3
    tab["v4"] = v4 # is converted to string!
    tab["v5"] = v5
    tab["v6"] = v6
    check tab.len == 6
    check tab["v1"] == v1
    check tab["v2"] == v2
    check tab["v3"] == v3
    check tab["v4"] == v4
    check tab["v5"] == v5
    check tab["v6"] == v6

  test "Extracting values":
    check v1.toInt == 1
    check v2.toFloat == 1.5
    check v3.toBool == true
    check v4.toStr == "a"
    check v1.toStr == "1"
    check v2.toStr == "1.5"
    check v3.toStr == "true"
    expect(ValueError):
      discard v5.toStr
    expect(ValueError):
      discard v6.toStr

  test "String conversion":
    let n1 = %~ "1.1"
    let n2 = %~ "1.3e5"
    let n3 = %~ "aba"
    let n4 = %~ "1..1"
    let n5 = %~ "1.123"
    let n6 = %~ "1.5e5E5"
    let n7 = %~ "e"
    let n8 = %~ "E"
    let n9 = %~ "."
    let n10 = %~ "1e"
    let n11 = %~ "1E"
    let n12 = %~ "1."
    let n13 = %~ "e1"
    let n14 = %~ "E1"
    let n15 = %~ ".1"
    check $n1 == "\"1.1\""
    check $n2 == "\"1.3e5\""
    check $n3 == "aba"
    check $n4 == "1..1"
    check $n5 == "\"1.123\""
    check $n6 == "1.5e5E5"
    check $n7 == "e"
    check $n8 == "E"
    check $n9 == "."
    check $n10 == "1e"
    check $n11 == "1E"
    check $n12 == "1."
    check $n13 == "e1"
    check $n14 == "E1"
    check $n15 == ".1"

suite "Formula":
  test "Testing ~ formula creation":
    let f = x ~ y
    let a = x ~ (a - b)
    let g = n ~ m + a * b * d
    let g2 = n ~ m + a - b + d
    let g3 = n ~ m + a * b / d
    let single = ~ x
    let gg1 = hwy ~ (displ + cyl - cty)
    let gg2 = hwy ~ displ + cyl - cty

    check $f == "(~ x y)"
    check $a == "(~ x (- a b))"
    check $g == "(~ n (+ m (* (* a b) d)))"
    check $g2 == "(~ n (+ (- (+ m a) b) d))"
    check $g3 == "(~ n (+ m (/ (* a b) d)))"
    check $single == "(~ \"\" x)" # LHS is empty string value
    check $gg1 == "(~ hwy (- (+ displ cyl) cty))"
    check $gg2 == "(~ hwy (- (+ displ cyl) cty))"

  test "Testing ~ formula creation using f{} macro":
    let f = f{"meanCty" ~ ("hwy" + "cty")}
    let g = meanCty ~ hwy + cty
    check $f == $g
    # TODO: Add more tests here...
    # create with `.` access
    let tup = (a: 5.5, b: "ok")
    let h = f{tup.a == tup.b}
    check $h == "(== 5.5 ok)"

  test "Evaluate ~ formula":
    let mpg = readCsv("data/mpg.csv")
    let f = hwy ~ (displ + cyl - cty) # this doesn't make sense, but anyways...
    # Displacement + Cylinders - City mpg. Yeah :D
    # use RHS of formula for calculation of 0 row.
    check f.rhs.evaluate(mpg, 0) == %~ -12.2

  test "Formula, literal on RHS":
    let f = f{"from" ~ 0}
    check $f == "(~ from 0)"

suite "Geom":
  test "application of aes, style works":
    # Write test which tests that the application of things like an
    # aesthetic and a style, e.g. color, line size etc, is properly
    # applied for all geoms!
    # Take a look at the style check in the first GgPlot test
    discard

suite "GgPlot":
  test "x,y aesthetics of geom picked over GgPlot":
    ## tests that the x, y aesthetics are picked from the present `geom`
    ## if x, y are defined, instead of the `GgPlot` object.
    let x = toSeq(0 .. 10).mapIt(it.float)
    let y1 = x.mapIt(cos(it))
    let y2 = x.mapIt(sin(it))
    let df = seqsToDf({"x" : x, "cos" : y1, "sin" : y2})

    let gplt = ggplot(df, aes(x ~ cos)) +
      geom_line() + # line for cos
      geom_line(aes(x ~ sin), # line for sin
                color = color(0.0, 0.0))
    # geoms[0].x and y won't be set, since the aes from ggplot is used
    check (not gplt.geoms[0].aes.x.isSome)
    check (not gplt.geoms[0].aes.y.isSome)
    check gplt.geoms[1].aes.x.isSome
    check gplt.geoms[1].aes.y.isSome
    check gplt.aes.x.get == "x"
    check gplt.aes.y.get == "cos"
    check gplt.geoms[1].aes.x.get == "x"
    check gplt.geoms[1].aes.y.get == "sin"

    # bonus check
    check gplt.geoms[1].style.isSome
    let style = gplt.geoms[1].style.get
    check style.color == color(0.0, 0.0)
    check style.lineWidth == 1.0
    check style.lineType == ltSolid
    check style.fillColor == transparent

    # we cannot guarantee in a test whether the order is preserved in the code other
    # than calling the proc, which handles the ordering
    let (x1v, y1v) = readXYcols(gplt, gplt.geoms[0], float)
    let (x2v, y2v) = readXYcols(gplt, gplt.geoms[1], float)

    check x1v == x
    check x2v == x
    check y1v == y1
    check y2v == y2
