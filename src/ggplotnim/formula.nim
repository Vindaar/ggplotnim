import macros, tables, strutils, options, fenv, sets, hashes

import persvector, sequtils, seqmath, stats, strformat, algorithm

# for error messages to print types
import typetraits

type
  ValueKind* = enum
    VNull,
    VBool,
    VInt,
    VFloat,
    VString,
    VObject

  Value* = object
    case kind*: ValueKind
    of VString:
      str*: string
    of VInt:
      num*: BiggestInt
    of VFloat:
      fnum*: float
    of VBool:
      bval*: bool
    of VObject:
      fields*: OrderedTable[string, Value]
    of VNull:
      discard

  FormulaKind* = enum
    fkTerm, fkVariable, fkFunction, #fkFormula

  VectorValuedFunc* = proc(s: PersistentVector[Value]): Value
  ScalarValuedFunc* = proc(s: Value): Value

  FuncKind* = enum
    funcVector, funcScalar

  ArithmeticKind* = enum
    amPlus = "+"
    amMinus = "-"
    amMul = "*"
    amDiv = "/"
    amDep = "~"
    amEqual = "=="
    amGreater = ">"
    amLess = "<"
    amGeq = ">="
    amLeq = "<="
    amAnd = "and"
    amOr = "or"
    amXor = "xor"

  FormulaNode* = ref FormulaNodeObj
  FormulaNodeObj = object
    # FormulaNode is either a Term, meaning it has a LHS and RHS
    # or a variable. The operator (function) is given as an enum for
    # the Term connecting the two sides
    case kind*: FormulaKind
    of fkTerm:
      lhs*: FormulaNode
      rhs*: FormulaNode
      op*: ArithmeticKind
    of fkVariable:
      val*: Value
    of fkFunction:
      # storing a function to be applied to the data
      fnName*: string
      arg*: FormulaNode
      case fnKind*: FuncKind
      of funcVector:
        fnV*: proc(s: PersistentVector[Value]): Value
        res: Option[Value] # the result of fn(arg), so that we can cache it
                           # instead of recalculating it for every index potentially
      of funcScalar:
        fnS*: proc(s: Value): Value

type
  DataFrameKind = enum
    dfNormal, dfGrouped

  DataFrame* = object
    len*: int
    data*: OrderedTable[string, PersistentVector[Value]]
    case kind: DataFrameKind
    of dfGrouped:
      # a grouped data frame stores the keys of the groups and maps them to
      # a set of the categories
      groupMap: OrderedTable[string, HashSet[Value]]
    else: discard
    #data: Table[string, seq[Value]]

proc evaluate*[T](node: var FormulaNode, data: T, idx: int): Value
proc evaluate*[T](node: var FormulaNode, data: T): Value

func `high`*(df: DataFrame): int = df.len - 1

iterator keys*(df: DataFrame): string =
  for k in keys(df.data):
    yield k

proc getKeys*[T](tab: OrderedTable[string, T]): seq[string] =
  ## returns the keys of the table as a seq
  for k in keys(tab):
    result.add k

proc getKeys*(df: DataFrame): seq[string] =
  ## returns the keys of a data frame as a seq
  for k in keys(df):
    result.add k

iterator mpairs*(df: var DataFrame): (string, var PersistentVector[Value]) =
  for k, mval in mpairs(df.data):
    yield (k, mval)

iterator keys*(row: Value): string =
  doAssert row.kind == VObject
  for k in keys(row.fields):
    yield k

iterator pairs*(row: Value): tuple[key: string, val: Value] =
  ## Iterator for the elements of `row`. `row` has to be a JObject
  ## representing a row of a `DataFrame`
  assert row.kind == VObject
  for key, val in pairs(row.fields):
    yield (key, val)

proc add*(v: PersistentVector[Value], w: PersistentVector[Value]): PersistentVector[Value] =
  ## adds all elements of `w` to `v` and returns the resulting vector
  if v.len > 100 or w.len > 100:
    # go the seq conversion route
    var res = toSeq(v)
    res.add toSeq(w)
    result = toPersistentVector(res)
  else:
    result = v
    for x in w:
      result = result.add x

proc `[]`*(df: DataFrame, k: string): PersistentVector[Value] =
#proc `[]`(df: DataFrame, k: string): seq[Value] =
  result = df.data[k]

proc `[]`*(df: DataFrame, k: string, idx: int): Value =
  ## returns the element at index `idx` in column `k` directly, without
  ## returning the whole vector first
  result = df.data[k][idx]

proc `[]`*(df: DataFrame, k: string, slice: Slice[int]): seq[Value] =
  ## returns the element at index `idx` in column `k` directly, without
  ## returning the whole vector first
  result = df.data[k][slice.a .. slice.b]

proc `[]=`*(df: var DataFrame, k: string, vec: PersistentVector[Value]) =
#proc `[]=`(df: var DataFrame, k: string, vec: seq[Value]) =
  df.data[k] = vec
  # doAssert df.len == vec.len

proc `[]`*(v: Value, key: string): Value =
  doAssert v.kind == VObject
  result = v.fields[key]

proc `[]=`*(v: var Value, key: string, val: Value) =
  doAssert v.kind == VObject
  v.fields[key] = val

proc `[]`*(df: DataFrame, rowSlice: Slice[int]): DataFrame =
  ## returns the vertical slice of the data frame given by `rowSlice`.
  result = DataFrame(len: 0)
  for k in keys(df):
    result[k] = toPersistentVector(df[k, rowSlice])
  # add 1, because it's an ``inclusive`` slice!
  result.len = (rowSlice.b - rowSlice.a) + 1

proc contains*(df: DataFrame, key: string): bool =
  ## Contains proc for `DataFrames`, which checks if the `key` names
  ## a column in the `DataFrame`
  result = df.data.hasKey(key)

proc contains*(v: Value, key: string): bool =
  doAssert v.kind == VObject
  result = v.fields.hasKey(key)

template `failed?`(cond: untyped): untyped {.used.} =
  # helper template
  debugecho "Failed? ", astToStr(cond), ": ", cond

func isNumber(s: string): bool =
  ## returns true, if `s` is a number according to our rules:
  ## - starts with {0..9}
  ## - ends with {0..9}
  ## - may contain a single `.`
  ## - may contain a single `e`, `E`
  ## - else may only contain {0..9}
  ## It is only used to decide whether the stringifaction of `s`
  ## will be surrounded by `"`.
  let normStr = s.normalize
  if count(s, '.') > 1 or
     count(s, 'e') > 1 or
     count(s, 'E') > 1 or
     count(s, '-') > 1 or
     (count(s, 'e') > 0 and count(s, 'E') > 0) or
     s[0] notin {'0'..'9'} or
     s[^1] notin {'0'..'9'}:
    result = false
  else:
    result = s.allCharsInSet({'0'..'9', '.', 'e', 'E', '_', '-', '+'})

proc `$`*(v: Value): string =
  ## converts the given value to its value as a string
  case v.kind
  of VInt:
    result = $v.num
  of VFloat:
    result = &"{v.fnum:g}"
  of VBool:
    result = $v.bval
  of VString:
    let vstr = v.str
    if vstr.len == 0 or vstr.isNumber:
      result = "\"" & vstr & "\""
    else:
      result = vstr
  of VObject:
    result.add "{"
    for k, x in v.fields:
      result.add (&"{k} : {x}")
    result.add "}"
  of VNull:
    result = "null"

proc hash*(x: Value): Hash =
  case x.kind
  of VInt:
    result = hash(x.num)
  of VFloat:
    result = hash(x.fnum)
  of VString:
    result = hash(x.str)
  of VBool:
    result = hash(x.bval)
  of VObject:
    for k, v in x.fields:
      result = result !& hash(k)
      result = result !& hash(v)
  of VNull:
    result = 0
  result = !$result

proc `%~`*(c: char): Value =
  ## we convert a `char` to a `string`!
  result = Value(kind: VString, str: $c)

proc `%~`*(v: string): Value =
  result = Value(kind: VString, str: v)

proc `%~`*(v: SomeFloat): Value =
  result = Value(kind: VFloat, fnum: v.float)

proc `%~`*(v: SomeInteger): Value =
  result = Value(kind: VInt, num: v.int)

proc `%~`*(v: bool): Value =
  result = Value(kind: VBool, bval: v)

#proc `%~`*(v: Table[string, Value]): Value =
#  result = Value(kind: VObject, fields: v.toOrderedTable)

proc `%~`*(v: OrderedTable[string, Value]): Value =
  result = Value(kind: VObject, fields: v)

proc newVObject*(): Value =
  result = Value(kind: VObject)
  result.fields = initOrderedTable[string, Value]()

proc `%~`*[T: not Value](s: openArray[T]): seq[Value] =
  ## converts a `seq[T]` to a `seq[Value]`
  result = newSeq[Value](s.len)
  for i, x in s:
    result[i] = %~ x

template `%~`*(s: openArray[Value]): seq[Value] = @s

func isInt(s: string): bool =
  result = s.isDigit

func isDigit(v: Value): bool =
  ## checks whether the string contained in `Value` is purely made from digits
  doAssert v.kind == VString
  result = v.str.isDigit

func isInt(v: Value): bool =
  ## checks whether the string stored in a `Value` is an integer
  doAssert v.kind == VString
  result = v.isDigit

func isFloat(v: Value): bool =
  ## checks whether the string stored in a `Value` is a float
  doAssert v.kind == VString
  result = v.str.replace(".", "").isDigit

func isBool(s: string): bool = false
func parseBool(s: string): bool = false

proc toFloat*(v: Value): float =
  doAssert v.kind in {VInt, VFloat}
  case v.kind
  of VInt: result = v.num.float
  of VFloat: result = v.fnum
  else: discard

proc toInt*(v: Value): BiggestInt =
  ## Converts a numeric value to an int. If the value is a float
  ## we round and convert to int
  doAssert v.kind in {VInt, VFloat}
  case v.kind
  of VInt: result = v.num
  of VFloat: result = v.fnum.round.int
  else: discard

proc toBool*(v: Value): bool =
  ## Checks if the value is a bool and returns its value
  doAssert v.kind == VBool
  result = v.bval

proc toStr*(v: Value): string =
  ## Returns the value `v` as a string. If the value is of kind `VString`,
  ## no conversion is required.
  ## This however will fail, if the input is of type
  ## - VNull
  ## - VObject
  ## if you want string representations of those value types, use `$`
  case v.kind
  of VInt, VFloat, VBool: result = $v
  of VString: result = v.str
  else:
    raise newException(ValueError, "Will not convert a Value of kind " &
      $v.kind & " to string! Use `$` for that!")

proc almostEqual*(a, b: float, epsilon = 1e-8): bool =
  # taken from
  # https://floating-point-gui.de/errors/comparison/
  let
    absA = abs(a)
    absB = abs(b)
    diff = abs(a - b)
  if a == b: # shortcut, handles infinities
    result = true
  elif a == 0 or b == 0 or (absA + absB) < minimumPositiveValue(float64):
    # a or b is zero or both are extremely close to it
    # relative error is less meaningful here
    result = diff < (epsilon * minimumPositiveValue(float64))
  else:
    # use relative error
    result = diff / min(absA + absB, maximumPositiveValue(float64)) < epsilon

proc `==`*(v, w: Value): bool =
  ## checks whether the values are equal.
  ## Note: if both values are numbers of different kind (`VInt` and `VFloat`) the
  ## values are both compared as floats!
  ## The float comparison happens with a floating point comparison with relatively
  ## large epsilon (1-e8).
  if v.kind != w.kind and
     v.kind in {VInt, VFloat} and
     w.kind in {VInt, VFloat}:
    result = almostEqual(v.toFloat, w.toFloat)
  elif v.kind != w.kind:
    result = false
  else:
    case v.kind
    of VString:
      result = v.str == w.str
    of VInt:
      result = v.num == w.num
    of VFloat:
      result = almostEqual(v.fnum, w.fnum)
    of VBool:
      result = v.bval == w.bval
    of VObject:
      # NOTE: taken from json module
      # we cannot use OrderedTable's equality here as
      # the order does not matter for equality here.
      if v.fields.len != w.fields.len: return false
      for key, val in v.fields:
        if not w.fields.hasKey(key): return false
        if w.fields[key] != val: return false
      result = true
    of VNull:
      result = true

proc `<`*(v, w: Value): bool =
  ## checks whether the `v` is smaller than `w`
  ## Note: this is only defined for a subset of the possible types!
  ## Note2: if both are numbers of different kind (`VInt` and `VFloat`) the
  ## values are compared as a float! For very large values this would be problematic,
  ## but here we are lenient and assume the user uses `Value` for small calculations!
  if v.kind != w.kind and
     v.kind in {VFloat, VInt} and
     w.kind in {VFloat, VInt}:
    result = v.toFloat < w.toFloat
  else:
    case v.kind
    of VString:
      result = v.str < w.str
    of VInt:
      result = v.num < w.num
    of VFloat:
      result = v.fnum < w.fnum
    of VBool:
      result = v.bval < v.bval
    of VObject:
      # checks if objects have the same field, and if so whether the
      # fields of `v` are smaller than those of `w`
      result = true
      for k in keys(v):
        if k notin w:
          return false
        if v[k] < w[k]:
          return true
        elif v[k] > w[k]:
          return false
        # else v[k] is equal to w[k], continue
    else:
      raise newException(Exception, "Comparison `<` does not make sense for " &
        "Value kind " & $v.kind & "!")

proc `<=`*(v, w: Value): bool =
  ## checks whether `v` is smaller or equal than `w`
  if v == w:
    result = true
  elif v < w:
    result = true

template makeMath(op: untyped): untyped =
  proc `op`*(v, w: Value): Value =
    ## Adds two Values together, if they are addeable.
    ## These operations only work for `VInt` and `VFloat`. `VInt` is converted
    ## to floats for the calculation. The result is always a `VFloat`!
    if v.kind in {VFloat, VInt} and
       w.kind in {VFloat, VInt}:
      result = Value(kind: VFloat, fnum: `op`(v.toFloat, w.toFloat))
    else:
      raise newException(Exception, "Math operation does not make sense for " &
        "Value kind " & $v.kind & "!")

makeMath(`+`)
makeMath(`-`)
makeMath(`*`)
makeMath(`/`)

proc pretty*(df: DataFrame, numLines = 20): string =
  ## converts the first `numLines` to a table.
  ## If the `numLines` argument is negative, will print all rows of the
  ## datafra.e
  var maxLen = 0
  for k in keys(df):
    maxLen = max(k.len, maxLen)
  echo "Dataframe with ", df.getKeys.len, " columns and ", df.len, " rows:"
  let alignBy = maxLen + 4
  let num = if numLines > 0: min(df.len, numLines) else: df.len
  # write header
  result.add align("Idx", alignBy)
  for k in keys(df):
    result.add align($k, alignBy)
  result.add "\n"
  for i in 0 ..< num:
    result.add align($i, alignBy)
    for k in keys(df):
      result.add align($df[k][i], alignBy)
    result.add "\n"

proc `$`*(df: DataFrame): string =
  result = df.pretty

proc toUgly*(result: var string, node: FormulaNode) =
  var comma = false
  case node.kind:
  of fkTerm:
    result.add "(" & $node.op & " "
    result.toUgly node.lhs
    result.add " "
    result.toUgly node.rhs
    result.add ")"
  of fkVariable:
    result.add $node.val
  of fkFunction:
    result.add "("
    result.add node.fnName
    result.add " "
    result.toUgly node.arg
    result.add ")"

proc `$`*(node: FormulaNode): string =
  ## Converts `node` to its string representation
  result = newStringOfCap(1024)
  toUgly(result, node)

proc toDf*(t: OrderedTable[string, seq[string]]): DataFrame =
  ## creates a data frame from a table of seq[string]
  ## TODO: currently does not allow to parse bool!
  result = DataFrame(len: 0)
  for k, v in t:
    var data = newSeq[Value](v.len)
    # check first element of v for type
    if v.len > 0:
      # NOTE: This is rather ugly. We just try by guessing. If we fail,
      # go with the next broader class
      var maybeNumber = v[0].isNumber
      var maybeInt = v[0].allCharsInSet({'0'..'9','_'})
      for i, x in v:
        if maybeNumber and maybeInt:
          try:
            data[i] = %~ x.parseInt
          except ValueError:
            maybeInt = false
            try:
              data[i] = %~ x.parseFloat
            except ValueError:
              maybeNumber = false
              data[i] = %~ x
        elif maybeNumber:
          try:
            data[i] = %~ x.parseFloat
          except ValueError:
            maybeNumber = false
            data[i] = %~ x
        else:
          data[i] = %~ x
    result.data[k] = data.toPersistentVector
    if result.len == 0:
      result.len = result.data[k].len

proc toVector*[T: not Value](s: openArray[T]): PersistentVector[Value] =
  var valSeq = newSeq[Value](s.len)
  for i, x in s:
    valSeq[i] = %~ x
  result = valSeq.toPersistentVector

proc toVector*(s: seq[Value]): PersistentVector[Value] =
  ## the overload of `toVector`, which simply calls `toPersistentVector` directly
  result = toPersistentVector(s)

proc toDf*(t: OrderedTable[string, seq[Value]]): DataFrame =
  ## creates a data frame from a table of `seq[Value]`. Simply have to convert
  ## the `seq[Value]` to a `PersistentVector[Value]` and add to DF.
  result = DataFrame(len: 0)
  for k, v in t:
    result[k] = v.toVector

macro toTab*(args: varargs[untyped]): untyped =
  expectKind(args, nnkArglist)
  var s = args
  if args.len == 1 and args[0].kind == nnkTableConstr:
    # has to be tableConstr or simple ident
    s = args[0]
  elif args.len == 1 and args[0].kind notin {nnkIdent, nnkSym}:
    error("If only single argument it has to be an ident or symbol, " &
      "but " & $args[0].repr & " is of kind: " & $args[0].kind)
  let data = ident"columns"
  result = newStmtList()
  result.add quote do:
    var `data`: DataFrame
  for a in s:
    case a.kind
    of nnkIdent:
      let key = a.strVal
      result.add quote do:
        `data`[`key`] = `a`.toVector
        `data`.len = `a`.len
    of nnkExprColonExpr:
      let nameCh = a[0]
      let seqCh = a[1]
      result.add quote do:
        `data`[`nameCh`] = `seqCh`.toVector
        `data`.len = `seqCh`.len
    else:
      error("Unsupported kind " & $a.kind)
  result = quote do:
    block:
      `result`
      `data`
  #echo result.treerepr
  #echo result.repr

template seqsToDf*(s: varargs[untyped]): untyped =
  ## converts an arbitrary number of sequences to a `DataFrame` or any
  ## number of key / value pairs where we have string / seq[T] pairs.
  toTab(s)

proc hasKey(df: DataFrame, key: string): bool =
  result = df.data.hasKey(key)

iterator items*(df: DataFrame): Value =
  # returns each row of the dataframe as a Value of kind VObject
  for i in 0 ..< df.len:
    var res = Value(kind: VObject)
    for k in keys(df):
      res[k] = df[k][i]
    yield res

iterator pairs*(df: DataFrame): (int, Value) =
  # returns each row of the dataframe as a Value of kind VObject
  for i in 0 ..< df.len:
    var res = Value(kind: VObject)
    for k in keys(df):
      res[k] = df[k][i]
    yield (i, res)

proc toSeq(v: PersistentVector[Value]): seq[Value] =
  result = v[0 ..< v.len]

proc toSeq(df: DataFrame, key: string): seq[Value] =
  result = df[key].toSeq

proc vToSeq*(v: PersistentVector[Value]): seq[Value] = toSeq(v)
proc vToSeq*(df: DataFrame, key: string): seq[Value] = toSeq(df, key)

proc toFloat*(s: string): float =
  # TODO: replace by `toFloat(v: Value)`!
  result = s.parseFloat

proc nearlyEqual(x, y: float, eps = 1e-10): bool =
  ## equality check for floats which tries to work around floating point
  ## errors
  ## Taken from: https://floating-point-gui.de/errors/comparison/
  let absX = abs(x)
  let absY = abs(y)
  let diff = abs(x - y)
  if x == y:
    # shortcut, handles infinities
    result = true
  elif x == 0 or
       y == 0 or
       diff < minimumPositiveValue(system.float):
    # a or b is zero or both are extremely close to it
    # relative error is less meaningful here
    result =  diff < (eps * minimumPositiveValue(system.float))
  else:
    # use relative error
    result = diff / min((absX + absY), maximumPositiveValue(system.float)) < eps

proc isValidVal(v: Value, f: FormulaNode): bool =
  doAssert v.kind != VObject
  doAssert f.kind == fkTerm
  doAssert f.op in {amEqual, amGreater, amLess, amGeq, amLeq, amAnd, amOr, amXor}
  case v.kind
  of VInt, VFloat:
    case f.op
    of amEqual:
      result = v.toFloat.nearlyEqual(f.rhs.val.toFloat)
    of amGreater:
      result = v > f.rhs.val
    of amLess:
      result = v < f.rhs.val
    of amGeq:
      result = v >= f.rhs.val
    of amLeq:
      result = v <= f.rhs.val
    else:
      raise newException(Exception, "comparison of kind " & $f.op & " does " &
        "not make sense for value kind of " & $v.kind & "!")
  of VString:
    doAssert not f.rhs.val.isDigit, "comparison must be with another string!"
    case f.op
    of amEqual:
      result = v == f.rhs.val
    of amGreater:
      result = v > f.rhs.val
    of amLess:
      result = v < f.rhs.val
    else:
      raise newException(Exception, "comparison of kind " & $f.op & " does " &
        "not make sense for value kind of " & $v.kind & "!")
  of VBool:
    doAssert f.rhs.val.kind == VBool, "comparison must be with another bool!"
    case f.op
    of amEqual:
      result = v == f.rhs.val
    of amGreater:
      result = v > f.rhs.val
    of amLess:
      result = v < f.rhs.val
    of amGeq:
      result = v >= f.rhs.val
    of amLeq:
      result = v <= f.rhs.val
    of amAnd:
      result = v.toBool and f.rhs.val.toBool
    of amOr:
      result = v.toBool or f.rhs.val.toBool
    of amXor:
      result = v.toBool xor f.rhs.val.toBool
    else:
      raise newException(Exception, "comparison of kind " & $f.op & " does " &
        "not make sense for value kind of " & $v.kind & "!")
  else:
    raise newException(Exception, "comparison for kind " & $v.kind &
      " not yet implemented!")

proc isValidRow(v: Value, f: FormulaNode): bool =
  doAssert v.kind == VObject
  doAssert f.kind == fkTerm
  doAssert f.op in {amEqual, amGreater, amLess, amGeq, amLeq}
  let lhsKey = f.lhs.val
  doAssert f.lhs.val.kind == VString
  result = v[lhsKey.str].isValidVal(f)

proc delete(df: DataFrame, rowIdx: int): DataFrame =
  result = df
  for k in keys(df):
    var s = df[k][0 ..< df.len]
    s.delete(rowIdx)
    #result[k] = s
    result[k] = toPersistentVector(s)
  result.len = result.len - 1

proc add(df: var DataFrame, row: Value) =
  for k in keys(row):
    #var s = df[k]
    #s.add row[k]
    #df[k] = s
    if not df.hasKey(k):
      df[k] = initVector[Value]()
    df[k] = df[k].add row[k]
    doAssert df.len + 1 == df[k].len
  df.len = df.len + 1

func buildCondition(conds: varargs[FormulaNode]): FormulaNode =
  if conds.len == 1:
    let c = conds[0]
    doAssert c.kind == fkTerm
    doAssert c.op in {amEqual, amGreater, amLess, amGeq, amLeq}
    result = c
  else: discard

template checkCondition(c: FormulaNode): untyped =
  doAssert c.kind == fkTerm
  doAssert c.op in {amEqual, amGreater, amLess, amGeq, amLeq, amAnd, amOr, amXor}

func buildCondProc(conds: varargs[FormulaNode]): proc(v: Value): bool =
  # returns a proc which contains the condition given by the Formulas
  result = (
    proc(v: Value): bool =
      result = false
      for c in conds:
        if not v.isValidVal(c):
          result = false
          break
  )

proc getFilteredIdx(df: DataFrame, cond: FormulaNode): seq[int] =
  ## return indices allowed after filter, by applying `cond` to each index
  ## and checking it's validity
  result = newSeqOfCap[int](df.len)
  var mcond = cond
  for i in 0 ..< df.len:
    if mcond.evaluate(df, i).toBool:
      result.add i

proc getFilteredIdx(idx: seq[int], df: DataFrame, cond: FormulaNode): seq[int] =
  ## return indices allowed after filter, starting from a given sequence
  ## of allowed indices
  result = newSeqOfCap[int](idx.len)
  var mcond = cond
  for i in idx:
    if mcond.evaluate(df, i).toBool:
      result.add i

func filter(p: PersistentVector[Value], idx: seq[int]): PersistentVector[Value] =
  result = toPersistentVector(idx.mapIt(p[it]))

#func filter(p: seq[Value], idx: seq[int]): seq[Value] =
#  result = idx.mapIt(p[it])

proc filter*(df: DataFrame, conds: varargs[FormulaNode]): DataFrame =
  ## returns the data frame filtered by the conditions given
  var fullCondition: FormulaNode
  var filterIdx: seq[int]
  for c in conds:
    checkCondition(c)
    if filterIdx.len > 0:
      filterIdx = filterIdx.getFilteredIdx(df, c)
    else:
      filterIdx = getFilteredIdx(df, c)
  #let condProc = buildCondProc(conds)

  #let filterIdx = getFilteredIdx(df, fullCondition)
  #let filterIdx = getFilteredIdx(df, condProc)
  for k in keys(df):
    result[k] = initVector[Value]()
    result[k] = df[k].filter(filterIdx)
  result.len = filterIdx.len

template liftVectorFloatProc*(name: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): Value =
    result = Value(kind: VFloat, fnum: `name`(v[0 ..< v.len].mapIt(it.toFloat)))

template liftVectorIntProc*(name: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): Value =
    result = Value(kind: VInt, num: `name`(v[0 ..< v.len].mapIt(it.toInt)))

template liftVectorStringProc(name: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): Value =
    result = Value(kind: VString, str: `name`(v[0 ..< v.len].mapIt(it.toInt)))

template liftScalarFloatProc*(name: untyped): untyped =
  proc `name`*(v: Value): Value =
    result = Value(kind: VFloat, fnum: `name`(v.toFloat))

template liftScalarIntProc*(name: untyped): untyped =
  proc `name`*(v: Value): Value =
    result = Value(kind: VInt, num: `name`(v.toInt))

template liftScalarStringProc(name: untyped): untyped =
  proc `name`*(v: Value): Value =
    result = Value(kind: VString, str: `name`(v.toString))

liftVectorFloatProc(mean)
liftScalarFloatProc(abs)
liftVectorFloatProc(min)
liftVectorFloatProc(max)
liftScalarFloatProc(ln)
liftScalarFloatProc(log10)

template liftVectorProcToPersVec(name: untyped, outType: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): `outType` =
    result = v[0 ..< v.len].mapIt(`name`(it.toFloat))

# liftVectorProcToPersVec(ln, seq[float])

#template liftProcToString(name: untyped, outType: untyped): untyped =
#  proc `name`(df: DataFrame, x: string): `outType` =
#    result = `name`(df[x])
#
#liftProcToString(mean, float)

proc isValidFunc(fn: NimNode): bool =
  ## Checks if the given `fn` sym node represents a valid function
  ## of either `VectorValuedFunc` or `ScalarValuedFunc`.
  let impl = fn.getTypeImpl
  result = false
  case impl.kind
  of nnkProcTy:
    let argType = impl[0][1][1]
    if argType.kind == nnkBracketExpr:
      if eqIdent(argType[0], "PersistentVector") and
         eqIdent(argType[1], "Value"):
        result = true
    else:
      if eqIdent(argType, "Value"):
        result = true
  of nnkBracketExpr:
    expectKind(impl[1], nnkProcTy)
    result = isValidFunc(impl[1])
  else:
    error("Invalid kind " & $impl.kind)

macro extractFunction(fn: typed): untyped =
  ## returns the correct function from a potential `nnkClosedSymChoice`.
  ## If `fn` is already a SymNode, will return the function, if if is
  ## a valid function under `isValidFunc`.
  result = newEmptyNode()
  case fn.kind
  of nnkSym:
    if isValidFunc(fn):
      # if a valid function, return it
      result = fn
  of nnkClosedSymChoice, nnkOpenSymChoice:
    # if a generic, check if there exists a valid choice
    for ch in fn:
      if isValidFunc(ch):
        result = ch
        return result
  else:
    error("Invalid node kind " & $fn.kind & " is " & fn.treeRepr)
  if result.kind == nnkEmpty:
    error("Could not find an appropriate function of `VectorValuedKind` or " &
      "`ScalarValuedKind`! Make sure to lift the `" & $fn.repr & "` proc you " &
      "wish to use!")

proc createFormula[T](name: string, fn: T, arg: FormulaNode): FormulaNode
# introduce identity `%~` for value to avoid having to check whether
# a variable is already a value in macro construction
proc `%~`(v: Value): Value = v
proc constructVariable*(n: NimNode, identIsVar: static bool = true): NimNode =
  var val: NimNode
  case n.kind
  of nnkNilLit:
    # empty value meaning no comparison. Only allowed for something like
    # ~ x
    val = newLit("")
  of nnkIdent:
    when identIsVar:
      # identifier corresopnds to variable in local scope, take it
      val = n
    else:
      # identifier corresponds to key in data frame (`constructVariable` called
      # from untyped templates)
      val = newLit n.strVal
  of nnkStrLit:
    val = n#.strVal
  of nnkIntLit .. nnkFloat64Lit:
    val = n
  of nnkDotExpr:
    # probably field access of some object
    # echo n.treeRepr
    val = n
  else:
    error("Unsupported kind to construct variable " & $n.kind)
  result = quote do:
    FormulaNode(kind: fkVariable, val: %~ `val`)

proc constructFunction*(n: NimNode): NimNode =
  let fname = n[0].strVal
  let fn = n[0]
  let arg = constructVariable(n[1])
  result = quote do:
    # potentially extract the function from a generic
    let fnArg = extractFunction(`fn`)
    createFormula(`fname`, fnArg, `arg`)

proc buildFormula(n: NimNode): NimNode
proc handleSide(n: NimNode): NimNode =
  case n.kind
  of nnkInfix:
    result = buildFormula(n)
  of nnkIntLit .. nnkFloat64Lit, nnkStrLit:
    result = constructVariable(n)
  of nnkIdent:
    # should correspond to a known identifier in the calling scope
    result = constructVariable(n)
  of nnkCall:
    result = constructFunction(n)
  of nnkPar:
    result = buildFormula(n[0]) #constructFunction(n[0])
  of nnkDotExpr:
    result = constructVariable(n)
  else:
    raise newException(Exception, "Not implemented! " & $n.kind)

proc reorderRawTilde(n: NimNode, tilde: NimNode): NimNode =
  ## a helper proc to reorder an nnkInfix tree according to the
  ## `~` contained in it, so that `~` is at the top tree.
  ## (the actual result is simply the tree reordered, but without
  ## the tilde. Reassembly must happen outside this proc)
  result = copyNimTree(n)
  for i, ch in n:
    case ch.kind
    of nnkIdent, nnkStrLit, nnkIntLit .. nnkFloat64Lit, nnkPar, nnkCall:
      discard
    of nnkInfix:
      if ch == tilde:
        result[i] = tilde[2]
      else:
        result[i] = reorderRawTilde(ch, tilde)
    else:
      error("Unsupported kind " & $ch.kind)

proc recurseFind(n: NimNode, cond: NimNode): NimNode =
  ## a helper proc to find a node matching `cond` recursively
  for i, ch in n:
    if ch == cond:
      result = n
      break
    else:
      let found = recurseFind(ch, cond)
      if found.kind != nnkNilLIt:
        result = found

proc buildFormula(n: NimNode): NimNode =
  expectKind(n, nnkInfix)

  let tilde = recurseFind(n,
                          cond = ident"~")
  var node = n
  if tilde.kind != nnkNilLit and n[0].ident != toNimIdent"~":
    # only reorder the tree, if it does contain a tilde and the
    # tree is not already ordered (i.e. nnkInfix at top with tilde as
    # LHS)
    let replaced = reorderRawTilde(n, tilde)
    let full = nnkInfix.newTree(tilde[0],
                                tilde[1],
                                replaced)
    node = full

  let opid = node[0].strVal
  let op = quote do:
    parseEnum[ArithmeticKind](`opid`)
  let lhs = handleSide(node[1])
  let rhs = handleSide(node[2])
  result = quote do:
    FormulaNode(kind: fkTerm, lhs: `lhs`, rhs: `rhs`, op: `op`)

macro `{}`*(x, y: untyped): untyped =
  if x.repr == "f":
    result = buildFormula(y)

proc unique*(v: PersistentVector[Value]): seq[Value] =
  ## returns a seq of all unique values in `v`
  result = v.vToSeq.deduplicate

proc calcNewColumn(df: DataFrame, fn: FormulaNode): (string, PersistentVector[Value]) =
  ## calculates a new column based on the `fn` given
  doAssert fn.lhs.kind == fkVariable, " was " & $fn
  doAssert fn.lhs.val.kind == VString, " was " & $fn
  let colName = $fn.lhs.val
  # mutable copy so that we can cache the result of `fn(arg)` if such a
  # function call is involved
  var mfn = fn
  var newCol = newSeq[Value](df.len)
  for i in 0 ..< df.len:
    newCol[i] = mfn.rhs.evaluate(df, i)
  result = (colName, toPersistentVector(newCol))

proc mutate*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## Returns the data frame with an additional mutated column, described
  ## by the functions `fns`.
  ## Each formula `fn` given will be used to create a new column in the
  ## dataframe.
  ## We assume that the LHS of the formula corresponds to a fkVariable
  ## that's used to designate the new name.
  result = df
  for fn in fns:
    if fn.kind == fkVariable:
      doAssert fn.val.kind == VString
      result[fn.val.str] = df[fn.val.str]
    else:
      let (colName, newCol) = result.calcNewColumn(fn)
      result[colName] = newCol

proc transmute*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## Returns the data frame cut to the columns created by `fns`, which
  ## should involve a calculation. To only cut to one or more columns
  ## use the `select` proc.
  ## A function may only contain a `fkVariable` in order to keep the
  ## column without modification.
  ## We assume that the LHS of the formula corresponds to a fkVariable
  ## that's used to designate the new name.
  # since result dataframe is empty, copy len of input
  result.len = df.len
  for fn in fns:
    if fn.kind == fkVariable:
      doAssert fn.val.kind == VString
      result[fn.val.str] = df[fn.val.str]
    else:
      let (colName, newCol) = df.calcNewColumn(fn)
      result[colName] = newCol

proc select*[T: string | FormulaNode](df: DataFrame, cols: varargs[T]): DataFrame =
  ## Returns the data frame cut to the names given as `cols`. The argument
  ## may either be the name of a column as a string, or a `FormulaNode` describing
  ## either a selection with a name applied in form of an "equation" (c/f mpg dataset):
  ## mySelection ~ hwy
  ## or just an `fkVariable` stating the name of the column. Using the former approach
  ## it's possible to select and rename a column at the same time.
  ## Note that the columns will be ordered from left to right given by the order
  ## of the `cols` argument!
  result.len = df.len
  for fn in cols:
    when type(T) is string:
      result[fn] = df[fn]
    else:
      if fn.kind == fkVariable:
        doAssert fn.val.kind == VString
        result[fn.val.str] = df[fn.val.str]
      else:
        doAssert fn.rhs.kind == fkVariable, "if you wish to perform a calculation " &
          "of one or more columns, please use `transmute` or `mutate`!"
        doAssert fn.lhs.val.kind == VString
        doAssert fn.rhs.val.kind == VString
        result[fn.lhs.val.str] = df[fn.rhs.val.str]
        #let (colName, newCol) = df.calcNewColumn(fn)
        #result[colName] = newCol

proc rename*(df: DataFrame, cols: varargs[FormulaNode]): DataFrame =
  ## Returns the data frame with the columns described by `cols` renamed to
  ## the names on the LHS of the given `FormulaNode`. All other columns will
  ## be left untouched.
  ## Note that the renamed columns will be stacked on the right side of the
  ## data frame!
  ## NOTE: The operator between the LHS and RHS of the formulas does not
  ## have to be `~`, but for clarity it should be.
  result = df
  for fn in cols:
    doAssert fn.kind == fkTerm, "The formula must be term!"
    doAssert fn.rhs.kind == fkVariable, "the RHS of the formula must be a name " &
      "given as a `fkVariable`!"
    doAssert fn.lhs.val.kind == VString
    doAssert fn.rhs.val.kind == VString
    result[fn.lhs.val.str] = df[fn.rhs.val.str]
    # remove the column of the old name
    result.data.del(fn.rhs.val.str)

proc getColsAsRows(df: DataFrame, keys: seq[string]): seq[Value] =
  ## Given a dataframe `df` and column keys `keys`, returns a `seq[Value]`
  ## where each `Value` is a `VObject` containing a single row, with
  ## (key, value) pairs.
  # now build the rows
  result = newSeq[Value](df.len)
  for i in 0 ..< result.len:
    result[i] = newVObject()
    for k in keys:
      result[i][k] = df[k, i]

proc arrangeSortImpl(toSort: var seq[(int, Value)], order: SortOrder) =
  ## sorts the given `(index, Value)` pair according to the `Value`
  toSort.sort(
      cmp = (
        proc(x, y: (int, Value)): int =
          result = system.cmp(x[1], y[1])
      ),
      order = order
    )

proc arrange*(df: DataFrame, by: seq[string], order = SortOrder.Ascending): DataFrame =
  ## sorts the data frame in ascending / descending `order` by key `by`
  # now sort by cols in ascending order of each col, i.e. ties will be broken
  # in ascending order of the columns
  var idxCol: seq[(int, Value)]
  if by.len == 1:
    let col = toSeq(df[by[0]])
    let idx = toSeq(0 .. col.high)
    idxCol = zip(idx, col)
    idxCol.arrangeSortImpl(order)
  else:
    # in case of having multiple strings to sort by, first create a sequence of all
    # rows (only containig the columns to be sorted)
    let colRows = getColsAsRows(df, by)
    let idx = toSeq(0 .. colRows.high)
    idxCol = zip(idx, colRows)
    idxCol.arrangeSortImpl(order)
  result.len = df.len
  for k in keys(df):
    result[k] = idxCol.mapIt(df[k][it[0]]).toPersistentVector

proc arrange*(df: DataFrame, by: string, order = SortOrder.Ascending): DataFrame =
  result = df.arrange(@[by], order)

proc innerJoin*(df1, df2: DataFrame, by: string): DataFrame =
  ## returns a data frame joined by the given key `by` in such a way as to only keep
  ## rows found in both data frames
  # build sets from both columns and seqs of their corresponding indices
  let
    df1S = df1.arrange(by)
    df2S = df2.arrange(by)
  let
    col1 = toSeq(df1S, by)
    col2 = toSeq(df2S, by)
  let colSet1 = col1.toSet
  let colSet2 = col2.toSet
  let intersection = colSet1 * colSet2
  let idxDf1 = toSeq(0 ..< col1.len).filterIt(col1[it] in intersection)
  let idxDf2 = toSeq(0 ..< col2.len).filterIt(col2[it] in intersection)

  var
    i = 0
    j = 0
  let
    # for some reason we can't do toSeq(keys(df1S)) anymore...
    # This is due to https://github.com/nim-lang/Nim/issues/7322. `toSeq` isn't exported for now.
    keys1 = getKeys(df1S).toSet
    keys2 = getKeys(df2S).toSet
    allKeys = keys1 + keys2
  var row = Value(kind: VObject)
  var seqTab = initOrderedTable[string, seq[Value]]()
  for k in allKeys:
    seqTab[k] = newSeq[Value](max(idxDf1.len, idxDf2.len))
  var count = 0

  while i < idxDf1.len and
        j < idxDf2.len:
    let il = idxDf1[i]
    let jl = idxDf2[j]
    # indices point to same row, merge row
    if df1S[by][il] == df2S[by][jl]:
      for k in allKeys:
        if k in keys1 and k in keys2:
          doAssert df1S[k][il] == df2S[k][jl]
          seqTab[k][count] = df1S[k][il]
        elif k in keys1:
          seqTab[k][count] = df1S[k][il]
        elif k in keys2:
          seqTab[k][count] = df2S[k][jl]
      inc count
    # now increase the indices as required
    if i != idxDf1.high and
       j != idxDf2.high and
       (df1S[by][idxDf1[i+1]] == df2S[by][idxDf2[j+1]]):
      inc i
      inc j
    elif i != idxDf1.high and (df1S[by][idxDf1[i+1]] == df2S[by][jl]):
      inc i
    elif j != idxDf2.high and (df1S[by][il] == df2S[by][idxDf2[j+1]]):
      inc j
    elif i == idxDf1.high and j == idxDf2.high:
      break
    else:
      raise newException(Exception, "This should not happen")
  result.len = count
  for k in keys(seqTab):
    result[k] = seqTab[k].toPersistentVector

proc setDiff*(df1, df2: DataFrame): DataFrame =
  ## returns a `DataFrame` with all elements in `df1` that are not found in
  ## `df2`
  ## NOTE: Currently simple implementation based on `HashSet`. Iterates
  ## both dataframes once to generate sets, calcualtes intersection and returns
  ## difference as new `DataFrame`
  ## Considers whole rows for comparison. The result is potentially unsorted!
  template dfToSet(df: DataFrame): untyped =
    var rowSet = HashSet[Value]()
    for row in df:
      rowSet.incl row
    rowSet
  let diff = dfToSet(df1) - dfToSet(df2)
  for row in diff:
    result.add row
  result.len = diff.card

proc group_by*(df: DataFrame, by: varargs[string], add = false): DataFrame =
  ## returns a grouped data frame grouped by all keys `by`
  ## A grouped data frame is a lazy affair. It only calculates the groups,
  ## but unless e.g. `summarize` is called on it, remains unchanged.
  ## If `df` is already a grouped data frame and `add` is `true`, the
  ## groups given by `by` will be added as additional groups!
  if df.kind == dfGrouped and add:
    # just copy `df`
    result = df
  else:
    # copy over the data frame into new one of kind `dfGrouped` (cannot change
    # kind at runtime!)
    result = DataFrame(kind: dfGrouped)
    result.data = df.data
    result.len = df.len
  for key in by:
    result.groupMap[key] = toSet(toSeq(result[key]))

iterator groups*(df: DataFrame): (seq[(string, Value)], DataFrame) =
  ## yields the subgroups of a grouped DataFrame `df` and the `(key, Value)`
  ## pairs that were used to create the subgroup. If `df` has more than
  ## one grouping, a subgroup is defined by the pair of the groupings!
  ## E.g. mpg.group_by("class", "cyl")
  ## will yield all pairs of car ("class", "cyl")!
  ## Note: only non empty data frames will be yielded!
  doAssert df.kind == dfGrouped
  # sort by keys
  let keys = getKeys(df.groupMap)
  # arrange by all keys in ascending order
  let dfArranged = df.arrange(keys)
  # having the data frame in a sorted order, walk it and return each combination
  var
    currentKeys: seq[(string, Value)]
    lastKeys: seq[(string, Value)]
    startIdx, stopIdx: int # indices which indicate from where to where a subgroup is located
  for i in 0 ..< dfArranged.len:
    currentKeys = keys.mapIt((it, dfArranged[it, i]))
    if currentKeys == lastKeys:
      # continue accumulating
      discard
    elif i > 0:
      # found the end of a subgroup or we're at the end of the DataFrame
      stopIdx = i - 1
      # return subgroup of startIdx .. stopIdx
      yield (lastKeys, dfArranged[startIdx .. stopIdx])
      # set new start and stop idx
      startIdx = i
    else:
      # should only happen for i == 0
      doAssert i == 0
    lastKeys = currentKeys
  # finally yield the last subgroup or the whole group, in case we only
  # have a single key
  yield (currentKeys, dfArranged[startIdx .. dfArranged.high])

  when false:
    # Old implementation based on `filter`. `filter` has to scan the whole data frame once for
    # each `pair`. This is fine for few pairs, but if we have a lot and the data frame is large
    # this is very inefficient. The above just scans the data frame only twice. Once to sort it
    # according to the keys and then to extract the sub data frame
    if df.groupMap.len > 1:
      # classes to store the `values` of each group. One sequence of values for each
      var classes = newSeq[seq[(string, Value)]]()
      for k, classSet in df.groupMap:
        classes.add toSeq(classSet).mapIt((k, it))
      # calculate the cartesian product of the classes
      let combinations = product(classes)
      for pair in combinations:
        var res = df
        for (key, val) in pair:
          res = res.filter(f{key == val})
        # yield if this is a non empty data frame
        if res.len > 0:
          yield (pair, res)
    else:
      # if only a single group, just filtered by each class
      for key, classes in df.groupMap:
        for class in classes:
          yield (@[(key, class)], df.filter(f{key == class}))

proc summarize*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## returns a data frame with the summaries applied given by `fn`. They
  ## are applied in the order in which they are given
  result = DataFrame(kind: dfNormal)
  for fn in fns:
    var mfn = fn
    # TODO: take next assert out, by adding option to create pure function with
    # f{} macro, i.e. f{mean("cyl")}
    doAssert fn.kind == fkTerm, "function must have named result!"
    doAssert fn.rhs.kind == fkFunction
    doAssert fn.lhs.kind == fkVariable
    case df.kind
    of dfNormal:
      # just apply the function
      let res = toPersistentVector(@[mfn.rhs.evaluate(df)])
      result[fn.lhs.val.str] = res
      result.len = res.len
    of dfGrouped:
      # apply the function to each ``group``
      for k, classes in df.groupMap:
        for class in classes:
          # add current class to `k`, but only if not already done on a
          # previous function
          if result.hasKey(k) and result[k].len < classes.len:
            result[k] = result[k].add class
          else:
            result[k] = toPersistentVector(@[class])
          var dfcopy = df.filter(f{k == class})
          let x = mfn.rhs.evaluate(dfcopy)
          let lhsKey = mfn.lhs.val.str
          if result.hasKey(lhsKey):
            result[lhsKey] = result[lhsKey].add x
          else:
            result[lhsKey] = toPersistentVector(@[x])
        # at some point `k` should have the correct length of the dataframe
        result.len = result[k].len

proc bind_rows*(dfs: varargs[(string, DataFrame)], id: string = ""): DataFrame =
  ## `bind_rows` combines several data frames row wise (i.e. data frames are
  ## stacked on top of one another).
  ## If a given column does not exist in one of the data frames, the corresponding
  ## rows of the data frame missing it, will be filled with `VNull`.
  result = DataFrame(len: 0)
  var totLen = 0
  for (idVal, df) in dfs:
    totLen += df.len
    # first add `id` column
    if id.len > 0 and id notin result:
      result[id] = toVector(toSeq(0 ..< df.len).mapIt(%~ idVal))
    elif id.len > 0:
      result[id] = result[id].add toVector(toSeq(0 ..< df.len).mapIt(%~ idVal))
    var lastSize = 0
    for k in keys(df):
      if k notin result:
        # create this new column consisting of `VNull` up to current size
        if result.len > 0:
          result[k] = toVector(toSeq(0 ..< result.len).mapIt(Value(kind: VNull)))
        else:
          result[k] = initVector[Value]()
      # now add the current vector
      if k != id:
        # TODO: write a test for multiple bind_rows calls in a row!
        result[k] = result[k].add df[k]
      lastSize = max(result[k].len, lastSize)
    result.len = lastSize

  # possibly extend vectors, which have not been filled with `VNull` (e.g. in case
  # the first `df` has a column `k` with `N` entries, but another `M` entries are added to
  # the `df`. Since `k` is not found in another `df`, it won't be extend in the loop above
  for k in keys(result):
    if result[k].len < result.len:
      # extend this by `VNull`
      result[k] = result[k].add toVector(toSeq(result[k].len ..< result.len)
          .mapIt(Value(kind: VNull)))
  doAssert totLen == result.len, " totLen was: " & $totLen & " and result.len " & $result.len

template bind_rows*(dfs: varargs[DataFrame], id: string = ""): DataFrame =
  ## Overload of `bind_rows` above, for automatic creation of the `id` values.
  ## Using this proc, the different data frames will just be numbered by their
  ## order in the `dfs` argument and the `id` column is filled with those values.
  ## The values will always appear as strings, even though we use integer
  ## numbering.
  ## `bind_rows` combines several data frames row wise (i.e. data frames are
  ## stacked on top of one another).
  ## If a given column does not exist in one of the data frames, the corresponding
  ## rows of the data frame missing it, will be filled with `VNull`.
  var ids = newSeq[string]()
  for i, df in dfs:
    ids.add $i
  let args = zip(ids, dfs)
  bind_rows(args, id)

################################################################################
####### FORMULA
################################################################################

proc isSingle(x, y: NimNode, op: ArithmeticKind): NimNode
proc expand(n: NimNode): NimNode =
  case n.kind
  of nnkObjConstr:
    result = n
  of nnkInfix:
    let
      kind = parseEnum[ArithmeticKind](n[0].strVal)
      n1 = n[1]
      n2 = n[2]
    result = isSingle(n1, n2, kind)
  of nnkPar:
    let
      kind = parseEnum[ArithmeticKind](n[0][0].strVal)
      n1 = n[0][1]
      n2 = n[0][2]
    result = isSingle(n1, n2, kind)
  of nnkPrefix:
    let
      kind = parseEnum[ArithmeticKind](n[0].strVal)
      n1 = n[1]
    result = isSingle(nil, n1, kind)
  else:
    error("Unsupported kind " & $n.kind)

proc getFuncKind(fn: NimNode): NimNode =
  ## returns the type of function of `fn`. It is assumed that generics have
  ## already been resolved by `extractFunction`. It is called by the
  ## `getFunctionType` macro.
  let impl = fn.getTypeImpl
  case impl.kind
  of nnkProcTy:
    let argType = impl[0][1][1]
    if argType.kind == nnkBracketExpr:
      doAssert eqIdent(argType[0], "PersistentVector")
      doAssert eqIdent(argType[1], "Value")
      result = ident"VectorValuedFunc"
    else:
      doAssert eqIdent(argType, "Value")
      result = ident"ScalarValuedFunc"
  of nnkBracketExpr:
    expectKind(impl[1], nnkProcTy)
    result = getFuncKind(impl[1])
  else:
    error("Invalid kind " & $impl.kind)

macro getFunctionType(fn: typed): untyped =
  ## helper macro to work around issue in `createFormula`.
  ## Returns the type of the function that we are handed. Either a
  ## - `VectorValuedFunc` == proc(s: PersistentVector[Value]): Value
  ## - `ScalarValuedFunc` == proc(s: Value): Value
  ## Using `when T is VectorValuedFunc` in `createFormula` always enters
  ## the `else` branch?!
  case fn.kind
  of nnkSym:
    result = getFuncKind(fn)
  else:
    error("Invalid node kind " & $fn.kind)

proc createFormula[T](name: string, fn: T, arg: FormulaNode): FormulaNode =
  ## creates a `FormulaNode` of `fkFunction` with the correct `funcKind` based on the
  ## given `fn`.
  type fnType = getFunctionType(T)
  when fnType is VectorValuedFunc:
    result = FormulaNode(kind: fkFunction, fnName: name, arg: arg,
                         fnKind: funcVector, fnV: fn)
  elif fnType is ScalarValuedFunc:
    result = FormulaNode(kind: fkFunction, fnName: name, arg: arg,
                         fnKind: funcScalar, fnS: fn)
  else:
    raise newException(Exception, "Invalid function type: " & $type(fn).name)

proc isSingle(x, y: NimNode, op: ArithmeticKind): NimNode =
  var
    lhs: NimNode
    rhs: NimNode
  if x.len == 0:
    # is single
    lhs = constructVariable(x, identIsVar = false)
  else:
    lhs = expand(x)
  if y.len == 0:
    # is single
    rhs = constructVariable(y, identIsVar = false)
  else:
    rhs = expand(y)

  if x.kind == nnkNilLit and y.kind == nnkNilLit:
    error("Not both values can be nil at the same time!")
  elif y.kind == nnkNilLit:
    # assign nil lit always to `lhs`
    var tmp = lhs
    lhs = rhs
    rhs = tmp
  elif x.kind == nnkNilLit:
    doAssert rhs[2][1].strVal.len > 0, "Nil value cannot be at RHS!"

  let lit = newLit op
  result = quote do:
    FormulaNode(kind: fkTerm, lhs: `lhs`, rhs: `rhs`, op: `lit`)

proc findTilde(n: NimNode): NimNode =
  ## searches for the ~ node in the LHS branch of the given node
  ## returns a tuple of:
  ## - ~ node
  ## - whole tree with ~ node replaced by ~.rhs
  ## No, do it recursively on ``mutable (!)`` node, replace the ~ node
  ## with the RHS value of it and have result be copy of old ~ node
  expectKind(n, nnkObjConstr)
  for ch in n:
    case ch.kind
    of nnkSym:
      discard
    of nnkExprColonExpr:
      if ch[0].strVal == "lhs":
        # Index 3
        result = findTilde(ch[1])
      elif ch[0].strVal == "op":
        # found operator, check if `~`
        if (ch[1].kind == nnkCall or ch[1].kind == nnkConv) and ch[1][1] == newLit 4: # 4 == amDep
          result = copyNimTree(n)
      else:
        discard # RHS can be ignored
    else:
      error("Unsupported tree kind: " & $ch.kind)

proc replaceTilde(n: NimNode, tilde: NimNode): NimNode =
  ## searches for the ~ node in the LHS branch of the given node
  ## returns a tuple of:
  ## - ~ node
  ## - whole tree with ~ node replaced by ~.rhs
  ## No, do it recursively on ``mutable (!)`` node, replace the ~ node
  ## with the RHS value of it and have result be copy of old ~ node
  expectKind(n, nnkObjConstr)
  result = copyNimTree(n)
  for ch in n:
    case ch.kind
    of nnkSym:
      discard
    of nnkExprColonExpr:
      if ch[0].strVal == "lhs":
        # Index 2
        let res = replaceTilde(ch[1], tilde)
        case res.kind
        of nnkExprColonExpr:
          # replace the whole LHS part of the constructor (replaceTilde *did* do
          # something)
          result[2] = res
        of nnkObjConstr:
          # only replace the LHS Obj constructor part. (replaceTilde *did not* do
          # anything on the last call. *However* it may have done something one or
          # more levels deeper, so we *have* to copy it!
          result[2][1] = res
        else:
          error("Unsupported kind to copy " & $ch.kind)

      elif ch[0].strVal == "op":
        # found operator, check if `~`
        if (ch[1].kind == nnkCall or ch[1].kind == nnkConv) and ch[1][1] == newLit 4: # 4 == amDep
          # copy the tree again and assign tilde to RHS branch
          # Have to copy again, because above might have changed `result` in an
          # undesirable way!
          # -> if we *are* in the `~` branch, we do *NOT* care about the result of call to
          # replaceTilde, since that would reproduce the LHS part of it we're trying to get
          # rid of!
          result = copyNimTree(n)
          result = tilde[3]
          return result
        else:
          # repair the "RHS" ident in result. Due to a previous call in `deconstruct`, the
          # LHS field may still have a `RHS` attached to it. Fix that.
          result[2][0] = ident"lhs"
      else:
        discard # RHS can be ignored
    else:
      error("Unsupported tree kind: " & $ch.kind)

macro deconstruct(x, y: untyped, op: static ArithmeticKind): untyped =
  result = isSingle(x, y, op)
  let tilde = findTilde(result)
  if tilde.kind != nnkNilLit:
    let replaced = replaceTilde(result, tilde)
    let tildeLeft = tilde[2][1]
    var newRight: NimNode
    case replaced.kind
    of nnkObjConstr:
      newRight = replaced
    of nnkExprColonExpr:
      newRight = replaced[1]
    else: error("Unsupported " & $replaced.kind)
    let op = nnkCall.newTree(ident"ArithmeticKind", newLit 4)
    result = quote do:
      FormulaNode(kind: fkTerm, lhs: `tildeLeft`, rhs: `newRight`, op: `op`)

template `~`*(x: untyped): FormulaNode =
  deconstruct(x, nil, amDep)

template `~`*(x, y: untyped): FormulaNode =
  deconstruct(x, y, amDep)

template `+`*(x: FormulaNode, y: untyped): FormulaNode =
  deconstruct(x, y, amPlus)

template `-`*(x: FormulaNode, y: untyped): FormulaNode =
  deconstruct(x, y, amMinus)

template `*`*(x: FormulaNode, y: untyped): FormulaNode =
  deconstruct(x, y, amMinus)

template `/`*(x: FormulaNode, y: untyped): FormulaNode =
  deconstruct(x, y, amDiv)

proc initVariable[T](x: T): FormulaNode =
  result = FormulaNode(kind: fkVariable,
                       val: %~ x)

template makeMathProc(operator, opKind: untyped): untyped =
  #proc `operator`*(x, y: string): FormulaNode =
  proc `operator`*[T, U](x: T, y: U): FormulaNode =
    let
      lhs = initVariable(x)
      rhs = initVariable(y)
    result = FormulaNode(kind: fkTerm, lhs: lhs, rhs: rhs,
                         op: opKind)
  proc `operator`*[T](lhs: FormulaNode, y: T): FormulaNode =
    let rhs = initVariable(y)
    result = FormulaNode(kind: fkTerm, lhs: lhs, rhs: rhs,
                         op: opKind)
  proc `operator`*[T](x: T, rhs: FormulaNode): FormulaNode =
    let lhs = initVariable(x)
    result = FormulaNode(kind: fkTerm, lhs: lhs, rhs: rhs,
                         op: opKind)

# there are no overloads using `:` syntax for +, -, *, / since
# then the operator precedence would be overwritten!
# For comparison operators this does not matter.
#makeMathProc(`+`, amPlus)
#makeMathProc(`-`, amMinus)
#makeMathProc(`*`, amMul)
#makeMathProc(`/`, amDiv)
#makeMathProc(`~`, amDep)
makeMathProc(`:~`, amDep)
makeMathProc(`:=`, amEqual)
makeMathProc(equal, amEqual)
makeMathProc(`:>`, amGreater)
makeMathProc(greater, amGreater)
makeMathProc(`:<`, amLess)
makeMathProc(less, amLess)
makeMathProc(`:>=`, amGeq)
makeMathProc(geq, amGeq)
makeMathProc(`:<=`, amLeq)
makeMathProc(leq, amLeq)

proc evaluate*[T](node: var FormulaNode, data: T, idx: int): Value =
  #echo "Node ", node
  case node.kind
  of fkVariable:
    case node.val.kind
    of VString:
      # the given node corresponds to a key of the data frame
      # TODO: maybe extend this so that if `node.val` is ``not`` a key of the dataframe
      # we take the literal string value instead?
      when type(data) is DataFrame:
        if node.val.str in data:
          result = data[node.val.str][idx]
        else:
          # if it's not a key, we use the literal
          result = node.val
      elif type(data) is Table[string, seq[string]]:
        result = %~ data[node.val.str][idx].parseFloat
      elif type(data) is OrderedTable[string, seq[string]]:
        result = %~ data[node.val.str][idx].parseFloat
      else:
        raise newException(Exception, "Unsupported type " & $type(data) & " for serialization!")
    of VFloat, VInt, VBool:
      # take the literal value of the node
      result = node.val
    else:
      raise newException(Exception, "Node kind of " & $node.kind & " does not " &
        "make sense for evaluation!")
  of fkTerm:
    case node.op
    of amPlus:
      result = node.lhs.evaluate(data, idx) + node.rhs.evaluate(data, idx)
    of amMinus:
      result = node.lhs.evaluate(data, idx) - node.rhs.evaluate(data, idx)
    of amMul:
      result = node.lhs.evaluate(data, idx) * node.rhs.evaluate(data, idx)
    of amDiv:
      result = node.lhs.evaluate(data, idx) / node.rhs.evaluate(data, idx)
    # For booleans we have to wrap the result again in a `Value`, since boolean
    # operators of `Value` will still return a `bool`
    of amGreater:
      result = %~ (node.lhs.evaluate(data, idx) > node.rhs.evaluate(data, idx))
    of amLess:
      result = %~ (node.lhs.evaluate(data, idx) < node.rhs.evaluate(data, idx))
    of amGeq:
      result = %~ (node.lhs.evaluate(data, idx) >= node.rhs.evaluate(data, idx))
    of amLeq:
      result = %~ (node.lhs.evaluate(data, idx) <= node.rhs.evaluate(data, idx))
    of amAnd:
      result = %~ (node.lhs.evaluate(data, idx).toBool and node.rhs.evaluate(data, idx).toBool)
    of amOr:
      result = %~ (node.lhs.evaluate(data, idx).toBool or node.rhs.evaluate(data, idx).toBool)
    of amXor:
      result = %~ (node.lhs.evaluate(data, idx).toBool xor node.rhs.evaluate(data, idx).toBool)
    of amEqual:
      result = %~ (node.lhs.evaluate(data, idx) == node.rhs.evaluate(data, idx))
    of amDep:
      raise newException(Exception, "Cannot evaluate a term still containing a dependency!")
  of fkFunction:
    # for now assert that the argument to the function is just a string
    # Extend this if support for statements like `mean("x" + "y")` (whatever
    # that is even supposed to mean) is to be added.
    doAssert node.arg.kind == fkVariable
    # we also convert to float for the time being. Implement a different proc or make this
    # generic, we want to support functions returning e.g. `string` (maybe to change the
    # field name at runtime via some magic proc)
    #echo "Accessing ", data[node.arg.val]
    when type(data) is DataFrame:
      case node.fnKind
      of funcVector:
        # a function taking a vector. Check if result already computed, else apply
        # to the column and store the result
        doAssert node.arg.val.kind == VString
        if node.res.isSome:
          result = node.res.unsafeGet
        else:
          result = node.fnV(data[node.arg.val.str])
          node.res = some(result)
      of funcScalar:
        # just a function taking a scalar. Apply to current `idx`
        result = node.fnS(data[node.arg.val.str][idx])
    else:
      raise newException(Exception, "Cannot evaluate a fkFunction for a data " &
        " frame of this type: " & $(type(data).name) & "!")

proc evaluate*[T](node: var FormulaNode, data: T): Value =
  ## evaluation of a data frame under a given `FormulaNode`. This is a reducing
  ## operation. It returns a single value from a whole data frame (by working on
  ## a single column)
  case node.kind
  of fkFunction:
    # for now assert that the argument to the function is just a string
    # Extend this if support for statements like `mean("x" + "y")` (whatever
    # that is even supposed to mean) is to be added.
    doAssert node.arg.kind == fkVariable
    # we also convert to float for the time being. Implement a different proc or make this
    # generic, we want to support functions returning e.g. `string` (maybe to change the
    # field name at runtime via some magic proc)
    #echo "Accessing ", data[node.arg.val]
    when type(data) is DataFrame:
      case node.fnKind
      of funcVector:
        # here we do ``not`` store the result of the calculation in the `node`, since
        # we may run the same function on different datasets + we only call this
        # "once" anyways
        doAssert node.arg.val.kind == VString
        result = node.fnV(data[node.arg.val.str])
      of funcScalar:
        raise newException(Exception, "The given evaluator function must work on" &
          " a whole column!")
    else:
      raise newException(Exception, "Cannot evaluate a fkFunction for a data " &
        " frame of this type: " & $(type(data).name) & "!")
  else:
    raise newException(Exception, "Only `fkFunction` is supported, not " & $node.kind)
