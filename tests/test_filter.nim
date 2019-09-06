import ggplotnim, ginger
import seqmath, sequtils
import unittest

test "Filter error messages":
  let xdata = toSeq(0 ..< 2560)
  let ydata = xdata.mapIt(it.float * it.float)
  # this test is a reminder to fix the error message raised by this
  let df = seqsToDf({"x" : xdata, "y" : ydata})
  # works fine
  echo df.filter(f{"x" < 1000})
  let name = "x"
  # works fine too
  echo df.filter(f{name < 1000})
  # accessing not existing key gives really bad error message,
  # because we fail in `<` at a point where we should never fail
  echo df.filter(f{"xyz" < 1000})

  # currently results in:
  #[
/home/basti/CastData/ExternCode/ggplotnim/tests/test_filter.nim(18) test_filter
/home/basti/CastData/ExternCode/ggplotnim/src/ggplotnim/formula.nim(724) filter
/home/basti/CastData/ExternCode/ggplotnim/src/ggplotnim/formula.nim(697) getFilteredIdx
/home/basti/CastData/ExternCode/ggplotnim/src/ggplotnim/formula.nim(1610) evaluate
/home/basti/CastData/ExternCode/ggplotnim/src/ggplotnim/formula.nim(362) <
/home/basti/src/nim/nim_git_repo/lib/system/fatal.nim(39) sysFatal

Unhandled exception: 'str' is not accessible using discriminant 'kind' of type 'Value' [FieldError]
  ]#
  # have to fix that. Really misleading.
