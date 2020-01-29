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

reject:
  let f = f{"Channel" == "Ch 0"}
