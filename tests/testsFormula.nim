import ggplotnim, unittest, sequtils, math, strutils, streams, sugar
import seqmath

type
  Foo = object
    fd: float

suite "Formulas":
  let a = [1, 2, 3]
  let b = [3, 4, 5]
  let c = [4, 5, 6]
  let d = [8, 9, 10]
  let e = [11, 12, 13]
  let f = [false, true, false]
  let g = ["hello", "world", "foo"]
  let h = [2.5, 7.5, NaN]
  let df = seqsToDf(a, b, c, d, e, f, g, h)
  test "Basic `idx` tests with automatic type deduction from context":
    block:
      # - infix, "a" read as integer automatically
      let fn = f{ idx("a") == 5 }
      check fn.evaluate(df).bCol == [false, false, false].toTensor
    block:
      # - infix, a read as float automatically
      let fn = f{ idx("a") == 5.5 }
      check fn.evaluate(df).bCol == [false, false, false].toTensor
    block:
      # - infix involving `in`, type conversion on `idx` and set
      let fn = f{ idx("a").int8 in {1'i8, 3, 5, 7} }
      check fn.evaluate(df).bCol == [true, false, true].toTensor
    block:
      # - infix of `>` works
      # - type determined automatically
      let fn = f{ 5 > idx("a") }
      check fn.evaluate(df).bCol == [true, true, true].toTensor
    block:
      # - infix of `>` works w/ order switched around
      # - type determined automatically
      let fn = f{ idx("a") > 5 }
      check fn.evaluate(df).bCol == [false, false, false].toTensor
    block:
      # - type deduction on one side works with `Value`
      let fn = f{ idx("a") >= %~ 5.5 }
      check fn.evaluate(df).bCol == [false, false, false].toTensor
    block:
      # - reads data as `bool`
      # - runtime error due to a, b being int
      ## TODO: decide if this should become a CT error due to ambiguity.
      ## Probably yes, requires change to `assignType` I suppose (not to use
      ## default type info here)
      expect(ValueError):
        let fn = f{ idx("a") > idx("b") }
        discard fn.evaluate(df)
    block:
      # - RHS is float, infix means LHS will be read as float
      let fn = f{idx("a") < idx("b").float }
      check fn.evaluate(df).bCol == [true, true, true].toTensor
    block:
      # - above works with `==` too
      let fn = f{ idx("a") == idx("b").float }
      check fn.evaluate(df).bCol == [false, false, false].toTensor
    block:
      var fm = Foo(fd: 5.2)
      let fn = f{ idx("a") > fm.fd }
      check fn.evaluate(df).bCol == [false, false, false].toTensor

    block:
      # - prefix, automatic type deduction
      let fn = f{ not idx("f") }
      check fn.evaluate(df).bCol == [true, false, true].toTensor
    block:
      let fn = f{ idx("x") >= max(col("x")) * 0.5 }

    block:
      let fn = f{ parseInt(idx("a")) > 2 }

  test "Basic `col` test with type deduction from context":
    block:
      ## the following fails at CT, because type of output is ambiguous (max is overloaded)
      # let fn = f{ col("a").max }
      ## This one should always work
      let fn2 = f{float: col("a").max }
      check fn2.reduce(df).toInt == 3

    block:
      # - accessing column length works
      let fn = f{float: col("a").len }
      check fn.reduce(df).toInt == 3

    block:
      # - accessing tensor elments with bracket
      let fn = f{float: col("a")[1] }
      check fn.reduce(df).toInt == 2

  test "Automatic type deduction based on nnkDotExpr w/ a (non ambiguous) proc call":
    block:
      # - examples of determining type from unique procedure in a case where
      #   heuristic type extraction fails
      proc uniqueProcWithType(x: int): int =
        x + 5
      let fn = f{ idx("a").uniqueProcWithType }
      check fn.evaluate(df).iCol == [6, 7, 8].toTensor

  test "Automatic type deduction based on `idx` in argument of a call overloaded proc call":
    block:
      # - type deduction based on `idx` in specific argument of a typically overloaded
      #   symbol. Can be deduced due to only single overload matching the arguments
      proc someInt(): int = 2
      proc max(x: int, y: string, z: float, b: int): int =
        result = 5
      let fn = f{ max(idx("a"), "hello", 5.5, someInt()) }
      check fn.evaluate(df).iCol == [5, 5, 5].toTensor

    block:
      # - automatically determines that `a` should be read as `int`
      # - formula is mapping
      let fn = f{ max(idx("a"), 2) }
      check fn.evaluate(df).iCol == [2, 2, 3].toTensor

  test "Formula with an if expression accessing multiple columns":
    block:
      # - formula with an if expression accessing multiple columns
      let fn = f{int -> int: if `a` < 2:
                               `b`
                             else:
                               `c` }
      check fn.evaluate(df).iCol == [3, 5, 6].toTensor

    when (NimMajor, NimMinor, NimPatch) >= (1, 4, 0):
      block:
        ## TODO: 1. we need the parenthesis (otherwise lexer error)
        ## 2. return type is deduced to be bool. It should be taken from
        ## the if expression! `nnkIfExpr` not implemented yet.
        let fn = f{float -> float: "h" ~ (if classify(idx("h")) == fcNaN:
                                            -1.0
                                          else:
                                            `h`)}
        check fn.evaluate(df).fCol == [2.5, 7.5, -1.0].toTensor

  test "Dot expression requiring `Value` input works automatically":
    block:
      # - dot call requiring `Value` argument, output is object column (because
      #   `isNull` returns a boolean as a `Value`
      let fn = f{ idx("a").isNull }
      check fn.evaluate(df).oCol == [%~ false, %~ false, %~ false].toTensor

  test "Infix with `notin` and local array":
    block:
      # - `notin` works and determines `g`
      let existKeys = ["hello"]
      let fn = f{string: `g` notin existKeys}
      check fn.evaluate(df).bCol == [false, true, true].toTensor

  test "`ggplotnim` formula accessing (proc) field of an object":
    block:
      type
        MS = object
          trans: proc(x: float): float
      let col = %~ "a"
      let ms = MS(trans: (proc(x: float): float = 5.5))
      let colStr = "log10(x4)"
      let fn = f{float: colStr ~ ms.trans( df[col.toStr][idx] ) }
      check fn.evaluate(df).fCol == [5.5, 5.5, 5.5].toTensor

  test "`max` overload is resolved in context of infix with float":
    block:
      let fn = f{ `a` >= max(`a`) * 0.5 }
      check fn.evaluate(df).bCol == [false, true, true].toTensor

    block:
      ## TODO: this is technically broken, because from `*` we take `float`
      ## as result and from the integer `-1` we determine the infix to be
      ## integer
      #let fn = f{ -1 * c"hwy"}

  test "Reducing formula with boolean return value":
    block:
      let df2 = seqsToDf({"var1" : toSeq(0 ..< 10)})
      let fn = f{ sum(`var1`) > 20000 }
      check fn.reduce(df2).toBool == false

  test "Example of no `idx` but reducing proc (mean) as a mapping":
    block:
      ## example of a formula that contradicts our assumption that we should error in
      ## case the determined formula kind and the given one mismatch.
      ## In this case we might *want* to assign something + the mean for each element in
      ## the DF (in the context of a `group_by` call this makes sense!
      ## We'll turn it into a warning.
      ## Also: keep in mind that if the user writes something, same as with type hints, we
      ## should value that decision.
      # here we only check it compiles (no CT error anymore)
      let fn = f{float -> float: "subMeanHwy" ~ 0.0 + mean(col("hwy"))}

  test "Name test":
    let f = f{"meanCty" ~ (c"hwy" + c"cty")}
    # name is the full name. Manual parens (nnkPar) are included in representation.
    check f.name == "(~ meanCty ((+ hwy cty)))"

  test "Constant mapping of integer":
    let countCol = "count"
    let fn = f{int: countCol ~ 0}
    check fn.evaluate(df).iCol == [0, 0, 0].toTensor

  test "Name of long formula":
    const cut_rms_trans_low = 0.1
    const cut_rms_trans_high = 1.5
    proc inRegion(x, y: float, r: string): bool =
      discard

    let fn = f{float -> bool:
      `rmsTransverse` >= cut_rms_trans_low and
      `rmsTransverse` <= cut_rms_trans_high and
      inRegion(df["centerX"][idx], df["centerY"][idx], "crSilver") and
      `hits` < 500}

    check $fn == "(and (and (and (>= rmsTransverse cut_rms_trans_low) (<= rmsTransverse cut_rms_trans_high)) (inRegion df["centerX"][idx] df["centerY"][idx] crSilver)) (< hits 500))"
