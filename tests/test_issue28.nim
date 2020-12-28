import ggplotnim
import json

template accept(x) =
  static: assert(compiles(x))

template reject(x) =
  static: assert(not compiles(x))

accept:
  let f = fn {"Channel" == "Ch 0"}
accept:
  let f2 = fn({"Channel" == "Ch 0"})
accept:
  let f3 = fn:
    {"Channel" == "Ch 0"}

# this should fail, because `json` provides a `{}` proc, which
# means we do not resolve our untyped `{}` macro
# But for some reason on Github Actions it passes?!
#reject:
#  let f = f{"Channel" == "Ch 0"}
