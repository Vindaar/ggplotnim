import unittest
import ggplotnim

import tables, sets

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
    check $single == "(~  x)"  # NOTE: 2 spaces, since LHS is empty
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
