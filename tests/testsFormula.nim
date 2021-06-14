import ggplotnim, unittest, sequtils, math, strutils, streams, sugar
import seqmath

type
  Foo = object
    fd: float

suite "Formulas":
  test "Tests":
    let a = [1, 2, 3]
    let b = [3, 4, 5]
    let c = [4, 5, 6]
    let d = [8, 9, 10]
    let e = [11, 12, 13]
    let df = seqsToDf(a, b, c, d, e)

    block:
      ## this is infix, but where both sides are ``not`` the same type!
      let fn = f{ idx("a") == 5 }
    block:
      let fn = f{ idx("a") == 5.5 }
    block:
      ## this is infix, but where both sides are ``not`` the same type!
      let fn = f{ idx("a").int8 in {1'i8, 3, 5, 7} }
    block:
      let fn = f{ 5 > idx("a") }
    block:
      let fn = f{ idx("a") > 5 }
    block:
      ## throws CT error, ambiguous type information
      let fn = f{ idx("a") > idx("b") }
    block:
      ## works, because RHS is float, infix means LHS needs to be float too
      let fn = f{idx("a") < idx("b").float }
    block:
      ## works, because RHS is float, infix means LHS needs to be float too
      let fn = f{ idx("a") == idx("b").float }
    block:
      var fm = Foo(fd: 5.2)
      let fn = f{ idx("a") > fm.fd }

    block:
      ## examples of determining type from unique procedure in a case where
      ## heuristic type extraction fails
      proc uniqueProcWithType(x: int): int =
        x + 5
      let fn = f{ idx("a").uniqueProcWithType }

    block:
      ## the following fails at CT, because type of output is ambiguous (max is overloaded)
      # let fn = f{ col("a").max }
      ## This one should always work
      let fn2 = f{float: col("a").max }

    block:
      proc someInt(): int = 2
      proc max(x: int, y: string, z: float, b: int): int =
        result = 5
      let fn = f{ max(idx("a"), "hello", 5.5, someInt()) }

    block:
      let fn = f{ idx("a") >= %~ 5.5 }

    block:
      let fn = f{int -> int: if `a` < 5:
                               `b`
                             else:
                               `c` }

    block:
      # prefix
      let fn = f{ not idx("a") }
    block:
      let fn = f{ idx("a").isNull }

    block:
      let fn = f{ idx("x") >= max(col("x")) * 0.5 }

    block:
      let fn = f{ parseInt(idx("a")) > 2 }


    block:
      let existKeys = ["hello"]
      let fn = f{string: `keys` notin existKeys}

    block:
      type
        MS = object
          trans: proc(x: float): float
      let col = %~ "someColumn"
      let ms = MS(trans: (proc(x: float): float = 5.5))
      let fn = f{float: colStr ~ ms.trans( df[col.toStr][idx] ) }
    block:
      let fn = f{ `x` >= max(`x`) * 0.5 }

    block:
      ## TODO: this is technically broken, because from `*` we take `float`
      ## as result and from the integer `-1` we determine the infix to be
      ## integer
      #let fn = f{ -1 * c"hwy"}



    ##block:
    ##  let fn = f{ max(idx("a"), 1) }
