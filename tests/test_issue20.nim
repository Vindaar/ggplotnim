import ggplotnim
import unittest

test "Issue #20 - `isDigit` was removed":
  let n1 = %~ "1.1"
  let n2 = %~ "1.3e5"
  let n3 = %~ "aba"
  let n4 = %~ "1..1"
  let n5 = %~ "123"
  let n6 = %~ "100_000"
  let n7 = %~ "_100_000_"
  check not n1.isInt
  check not n2.isInt
  check not n3.isInt
  check not n4.isInt
  check n5.isInt
  check n6.isInt
  check n7.isInt # this is a little unintuitive, but a downside of our simple def.
