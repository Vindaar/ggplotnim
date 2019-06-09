import unittest
import ggplotnim


test "Testing ~ formula creation":
  let f = x ~ y
  let a = x ~ (a - b)
  let g = n ~ m + a * b * d
  let g2 = n ~ m + a - b + d
  let g3 = n ~ m + a * b / d
  let single = ~ x
  let gg1 = hwy ~ (displ + cyl - cty)
  let gg2 = hwy ~ displ + cyl - cty

  doAssert $f == "(~ x y)"
  doAssert $a == "(~ x (- a b))"
  doAssert $g == "(~ n (+ m (* (* a b) d)))"
  doAssert $g2 == "(~ n (+ (- (+ m a) b) d))"
  doAssert $g3 == "(~ n (+ m (/ (* a b) d)))"
  doAssert $single == "(~  x)"  # NOTE: 2 spaces, since LHS is empty
  doAssert $gg1 == "(~ hwy (- (+ displ cyl) cty))"
  doAssert $gg2 == "(~ hwy (- (+ displ cyl) cty))"

test "Testing ~ formula creation using f{} macro":
  let f = f{"meanCty" ~ ("hwy" + "cty")}
  let g = meanCty ~ hwy + cty
  doAssert $f == $g
  # TODO: Add more tests here...

test "Serializing ~ formula":
  let mpg = readCsv("data/mpg.csv")
  let f = hwy ~ (displ + cyl - cty) # this doesn't make sense, but anyways...
  # Displacement + Cylinders - City mpg. Yeah :D
  # use RHS of formula for calculation of 0 row.
  doAssert f.rhs.evaluate(mpg, 0) == -12.2
