import macros, tables, strutils, options, fenv, sets, hashes

import persvector, sequtils, seqmath, stats, strformat, algorithm, parseutils

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
    amUnequal = "!="
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

  DataFrameKind = enum
    dfNormal, dfGrouped

  DataFrame* = object
    len*: int
    data*: OrderedTable[string, PersistentVector[Value]]
    # TODO: we could possibly add the following two fields to a dataframe
    # They're easy to determine when first creating a dataframe from either
    # a file or seqs. Scales help us when calling `setInitialScale` for instane,
    # so that we don't have to walk all of the data again in order to determine
    # min and max. Same goes for the value kind. The only downside is when working
    # with dataframes using dplyr-like procs we have to be careful to keep these
    # up to data!
    # `colScales: OrderedTable[string, ginger.Scale]`
    # `colVkinds: OrderedTable[string, ValueKind]`
    case kind: DataFrameKind
    of dfGrouped:
      # a grouped data frame stores the keys of the groups and maps them to
      # a set of the categories
      groupMap: OrderedTable[string, HashSet[Value]]
    else: discard
    #data: Table[string, seq[Value]]

const ValueNull* = Value(kind: VNull)

proc evaluate*(node: FormulaNode): Value
proc evaluate*(node: FormulaNode, data: DataFrame, idx: int): Value
proc reduce*(node: FormulaNode, data: DataFrame): Value
proc evaluate*(node: FormulaNode, data: DataFrame): PersistentVector[Value]

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

iterator items*(row: Value): Value =
  doAssert row.kind == VObject
  for v in values(row.fields):
    yield v

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

proc drop*(df: var DataFrame, key: string) {.inline.} =
  ## drops the given key from the DataFrame
  df.data.del(key)

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

proc `[]`*(df: DataFrame, k: string): PersistentVector[Value] {.inline.} =
  result = df.data[k]

proc `[]`*(df: DataFrame, k: string, idx: int): Value {.inline.} =
  ## returns the element at index `idx` in column `k` directly, without
  ## returning the whole vector first
  result = df.data[k][idx]

proc `[]`*(df: DataFrame, k: string, slice: Slice[int]): seq[Value] {.inline.} =
  ## returns the elements in `slice` in column `k` directly, without
  ## returning the whole vector first
  result = df.data[k][slice.a .. slice.b]

proc `[]=`*(df: var DataFrame, k: string, vec: PersistentVector[Value]) {.inline.} =
  df.data[k] = vec

proc `[]=`*[T](df: var DataFrame, k: string, data: openArray[T]) {.inline.} =
  ## Extends the given DataFrame by the column `k` with the `data`.
  ## This proc raises if the given data length if not the same as the
  ## DataFrames' length. In case `k` already exists, `data` will override
  ## the current content!
  if data.len == df.len:
    df.data[k] = toPersistentVector(%~ data)
  else:
    raise newException(ValueError, "Given `data` length of " & $data.len &
      " does not match DF length of: " & $df.len & "!")

proc `[]=`*(df: var DataFrame, k: string, idx: int, val: Value) {.inline.} =
  df.data[k] = df.data[k].update(idx, val)

proc `[]`*(v: Value, key: string): Value {.inline.} =
  doAssert v.kind == VObject
  result = v.fields[key]

proc `[]=`*(v: var Value, key: string, val: Value) {.inline.} =
  doAssert v.kind == VObject
  v.fields[key] = val

template `^^`(df, i: untyped): untyped =
  (when i is BackwardsIndex: df.len - int(i) else: int(i))

proc `[]`*[T, U](df: DataFrame, rowSlice: HSlice[T, U]): DataFrame =
  ## returns the vertical slice of the data frame given by `rowSlice`.
  result = DataFrame(len: 0)
  let a = (df ^^ rowSlice.a)
  let b = (df ^^ rowSlice.b)
  for k in keys(df):
    result[k] = toPersistentVector(df[k, a .. b])
  # add 1, because it's an ``inclusive`` slice!
  result.len = (b - a) + 1

proc row*(df: DataFrame, idx: int, cols: varargs[string]): Value {.inline.} =
  ## Returns the row `idx` of the DataFrame `df` as a `Value` of kind `VObject`.
  ## If `cols` are given, only those columns will appear in the resulting `Value`.
  result = Value(kind: VObject)
  let mcols = if cols.len == 0: getKeys(df) else: @cols
  for col in mcols:
    result[col] = df[col][idx]

template `[]`*(df: DataFrame, idx: int): Value =
  ## convenience template around `row` to access the `idx`-th row of the
  ## DF as a `VObject Value`.
  df.row(idx)

proc contains*(df: DataFrame, key: string): bool =
  ## Contains proc for `DataFrames`, which checks if the `key` names
  ## a column in the `DataFrame`
  result = df.data.hasKey(key)

proc contains*(v: Value, key: string): bool =
  doAssert v.kind == VObject
  result = v.fields.hasKey(key)

func isColumn*(fn: FormulaNode, df: DataFrame): bool =
  case fn.kind
  of fkVariable:
    case fn.val.kind
    of VString: result = fn.val.str in df
    else: result = false
  else: result = false

template `failed?`(cond: untyped): untyped {.used.} =
  # helper template
  debugecho "Failed? ", astToStr(cond), ": ", cond

func isNumber*(s: string): bool =
  ## returns true, if `s` is a number according to our rules:
  ## - starts with {0..9}
  ## - ends with {0..9}
  ## - may contain a single `.`
  ## - may contain a single `e`, `E`
  ## - may contain one minus, one plus at beginning and one for exponent
  ## - else may only contain {0..9}
  ## - `e`, `+`, `-`, `.` may not appear one after another
  ## - may contain space before and after the number
  ## It is only used to decide whether the stringifaction of `s`
  ## will be surrounded by `"`.
  var idx = skipWhile(s, toSkip = {' '})
  template next(checkFor: untyped): untyped =
    if idx < s.len - 1:
      s[idx + 1] in checkFor
    else:
      false
  var
    negMinus = false
    posPlus = false
    expMinus = false
    expPlus = false
    numBeforeDot = false
    numBeforeExp = false
    dot = false
    expE = false
    sinceLastSpace = -1
  while idx < s.len:
    case s[idx]
    of '-':
      if next({'-', '+', '.'}):
        # another `-+.` after `-`
        return false
      elif not negMinus:
        negMinus = true
      elif not expMinus:
        expMinus = true
      else:
        # apparently has 3 minus
        return false
    of '+':
      if next({'+', '-'}):
        # another `-+.` after `-`
        return false
      elif not posPlus:
        posPlus = true
      elif not expPlus:
        posPlus = true
      else:
        # apparently has 3 plus
        return false
    of '0' .. '9':
      if not dot:
        numBeforeDot = true
      if not expE:
        numBeforeExp = true
      inc idx
      continue
    of '.':
      if next({'e', 'E'}):
        # `e` after `.`
        return false
      elif not dot and numBeforeDot:
        dot = true
      else:
        # multiple dots or number before `dot`
        return false
    of 'e', 'E':
      if not next({'0'..'9', '-', '+'}):
        # apparently ends with an 'e', 'E'
        return false
      if not expE and numBeforeExp:
        expE = true
      else:
        # multiple `e` or no number before `e`
        return false
    of ' ':
      if sinceLastSpace == -1 or sinceLastSpace == 1:
        # when we encounter a space, set our `spaceCounter` to 0 to start
        # increasing it every itereation in main loop
        sinceLastSpace = 0
      elif sinceLastSpace > 1:
        # apparently something between last space and this space
        return false
    else: return false # something not part of a number
    inc idx
    if sinceLastSpace >= 0:
      # last iter found a space, so count spaces
      inc sinceLastSpace
  return true

func isNumber*(v: Value): bool =
  doAssert v.kind == VString
  result = v.str.isNumber

func almostEqual*(a, b: float, epsilon = 1e-8): bool
proc formatFloatValue(v: Value, precision: int): string =
  ## Performs the formatting of a value of kind `VFloat` to string.
  ## If the values are smaller < 1e-5 or > 1e5 scientific notation is
  ## used.
  doAssert v.kind == VFloat
  let f = v.fnum
  if almostEqual(abs(f), 0.0):
    # to make sure zero is not formatted in scientific
    result = f.formatBiggestFloat(format = ffDefault,
                                  precision = precision)
  elif abs(f) >= 1e5 or abs(f) <= 1e-5:
    result = f.formatBiggestFloat(format = ffScientific,
                                  precision = precision)
  else:
    result = f.formatBiggestFloat(format = ffDefault,
                                  precision = precision)
  result.trimZeros()

proc pretty*(v: Value, precision = 4, emphStrNumber = true): string =
  ## converts the given value to its value as a string. For `VFloat` the
  ## precision can be given.
  ## If `emphStrNumber` is true, a number stored as a string will be emphasized
  ## by enclosing it with explicit `"`. This is mainly for printing DFs to show
  ## the user if a number is a number or a string.
  case v.kind
  of VInt:
    result = $v.num
  of VFloat:
    result = formatFloatValue(v, precision = precision)
  of VBool:
    result = $v.bval
  of VString:
    let vstr = v.str
    if emphStrNumber and (vstr.len == 0 or vstr.isNumber):
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

template `$`*(v: Value): string = pretty(v)

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

func isInt*(s: string): bool =
  ## simple "most likely int" check. If the string only contains digits and
  ## `_` we consider it an Int
  s.allCharsInSet({'0' .. '9', '_'})

func isInt*(v: Value): bool =
  ## checks whether the string contained in `Value` is likely an integer
  ## For an `isFloat` equivalent see `isNumber`.
  doAssert v.kind == VString
  result = v.str.isInt

proc toFloat*(v: Value, allowNull: static bool = false): float =
  when not allowNull:
    doAssert v.kind in {VInt, VFloat}
  else:
    doAssert v.kind in {VInt, VFloat, VNull}
  case v.kind
  of VInt: result = v.num.float
  of VFloat: result = v.fnum
  of VNull:
    # This branch is forbidden for `allowNull = false` due to `doAssert` at top!
    result = 0.0
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

func isNull*(v: Value): Value =
  ## returns whether `v` is a `VNull` value as a `VBool`
  result = %~ (v.kind == VNull)

func almostEqual*(a, b: float, epsilon = 1e-8): bool =
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
    elif v.kind == VNull or w.kind == VNull:
      result = Value(kind: VNull)
    else:
      raise newException(Exception, "Math operation does not make sense for " &
        "Value kind " & $v.kind & "!")

makeMath(`+`)
makeMath(`-`)
makeMath(`*`)
makeMath(`/`)

proc pretty*(df: DataFrame, numLines = 20, precision = 4, header = true): string =
  ## converts the first `numLines` to a table.
  ## If the `numLines` argument is negative, will print all rows of the
  ## dataframe.
  ## The precision argument is relevant for `VFloat` values, but can also be
  ## (mis-) used to set the column width, e.g. to show long string columns.
  ## The `header` is the `Dataframe with ...` information line, which is not part
  ## of the returned values for simplicity if the output is to be assigned to some
  ## variable. TODO: we could change that (current way makes a test case easier...)
  ## TODO: need to improve printing of string columns if length of elements
  ## more than `alignBy`.
  var maxLen = 6 # default width for a column name
  for k in keys(df):
    maxLen = max(k.len, maxLen)
  if header:
    echo "Dataframe with ", df.getKeys.len, " columns and ", df.len, " rows:"
  let alignBy = max(maxLen + precision, 10)
  let num = if numLines > 0: min(df.len, numLines) else: df.len
  # write header
  result.add align("Idx", alignBy)
  for k in keys(df):
    result.add align($k, alignBy)
  result.add "\n"
  for i in 0 ..< num:
    result.add align($i, alignBy)
    for k in keys(df):
      let element = pretty(df[k, i], precision = precision)
      if element.len < alignBy - 1:
        result.add align(element,
                         alignBy)
      else:
        result.add align(element[0 ..< alignBy - 4] & "...",
                         alignBy)
    result.add "\n"

template `$`*(df: DataFrame): string = df.pretty

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

proc toVector*[T: not Value](s: openArray[T]): PersistentVector[Value] =
  var valSeq = newSeq[Value](s.len)
  for i, x in s:
    valSeq[i] = %~ x
  result = valSeq.toPersistentVector

proc toVector*(s: seq[Value]): PersistentVector[Value] =
  ## the overload of `toVector`, which simply calls `toPersistentVector` directly
  result = toPersistentVector(s)

func nullVector(num: int): PersistentVector[Value] =
  ## returns a `PersistentVector[Value]` with `N` values, which are
  ## all `VNull`
  var nullseq = newSeq[Value](num)
  for i in 0 ..< num:
    nullseq[i] = Value(kind: VNull)
  result = toVector(nullseq)

proc extendShortColumns*(df: var DataFrame) =
  ## initial calls to `seqsToDf` and other procs may result in a ragged DF, which
  ## has less entries in certain columns than the data frame length.
  ## This proc fills up the mutable dataframe in those columns
  for k in keys(df):
    if df[k].len < df.len:
      let nFill = df.len - df[k].len
      df[k] = df[k].add nullVector(nFill)

proc toDf*(t: OrderedTable[string, seq[string]]): DataFrame =
  ## creates a data frame from a table of seq[string]
  ## NOTE: This proc assumes that the given entries in the `seq[string]`
  ## have been cleaned of white space. The `readCsv` proc takes care of
  ## this.
  ## TODO: currently does not allow to parse bool!
  result = DataFrame(len: 0)
  for k, v in t:
    var data = newSeq[Value](v.len)
    # check first element of v for type
    if v.len > 0:
      # NOTE: This is rather ugly. We just try by guessing. If we fail,
      # go with the next broader class
      var maybeNumber = v[0].isNumber
      var maybeInt = v[0].isInt
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
    result.len = max(result.data[k].len, result.len)
  result.extendShortColumns()

proc toDf*(t: OrderedTable[string, seq[Value]]): DataFrame =
  ## creates a data frame from a table of `seq[Value]`. Simply have to convert
  ## the `seq[Value]` to a `PersistentVector[Value]` and add to DF.
  result = DataFrame(len: 0)
  for k, v in t:
    result[k] = v.toVector
    result.len = max(v.len, result.len)
  result.extendShortColumns()

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
        `data`.len = max(`data`.len, `a`.len)
    of nnkExprColonExpr:
      let nameCh = a[0]
      let seqCh = a[1]
      result.add quote do:
        `data`[`nameCh`] = `seqCh`.toVector
        `data`.len = max(`data`.len, `seqCh`.len)
    else:
      error("Unsupported kind " & $a.kind)
  result = quote do:
    block:
      `result`
      # finally fill up possible columns shorter than df.len
      `data`.extendShortColumns()
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
    yield df.row(i)

iterator pairs*(df: DataFrame): (int, Value) =
  # returns each row of the dataframe as a Value of kind VObject
  for i in 0 ..< df.len:
    yield (i, df.row(i))

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
    of amUnequal:
      result = not v.toFloat.nearlyEqual(f.rhs.val.toFloat)
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
    doAssert not f.rhs.val.isNumber, "comparison must be with another string!"
    case f.op
    of amEqual:
      result = v == f.rhs.val
    of amUnequal:
      result = v != f.rhs.val
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
    of amUnequal:
      result = v != f.rhs.val
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
  doAssert f.op in {amEqual, amUnequal, amGreater, amLess, amGeq, amLeq}
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
  if c.op == amDep:
    raise newException(Exception, "A formula containing `~` is not allowed for" &
      "filter, since filter does not assign to a column. Did you accidentally add it?")
  doAssert c.op in {amEqual, amUnequal, amGreater, amLess, amGeq, amLeq, amAnd, amOr, amXor}

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

template liftVectorFloatProc*(name: untyped,
                              toExport: static bool = true): untyped =
  ## Lifts a proc, which takes a `seq[float]` to act on a `PersistentVector[Value]`
  ## so that it can be used in a formula to act on a whole DF column.
  ## `toExport` can be set to `false` so that the resulting proc is not exported.
  ## This is useful to lift procs only locally (e.g. in a test case etc.)
  when toExport:
    proc `name`*(v: PersistentVector[Value]): Value =
      result = Value(kind: VFloat, fnum: `name`(v[0 ..< v.len].mapIt(it.toFloat)))
  else:
    proc `name`(v: PersistentVector[Value]): Value =
      result = Value(kind: VFloat, fnum: `name`(v[0 ..< v.len].mapIt(it.toFloat)))

template liftVectorIntProc*(name: untyped,
                            toExport: static bool = true): untyped =
  ## Lifts a proc, which takes a `seq[int]` to act on a `PersistentVector[Value]`
  ## so that it can be used in a formula to act on a whole DF column.
  ## `toExport` can be set to `false` so that the resulting proc is not exported.
  ## This is useful to lift procs only locally (e.g. in a test case etc.)
  when toExport:
    proc `name`*(v: PersistentVector[Value]): Value =
      result = Value(kind: VInt, num: `name`(v[0 ..< v.len].mapIt(it.toInt)))
  else:
    proc `name`(v: PersistentVector[Value]): Value =
      result = Value(kind: VInt, num: `name`(v[0 ..< v.len].mapIt(it.toInt)))

template liftVectorStringProc*(name: untyped,
                               toExport: static bool = true): untyped =
  ## Lifts a proc, which takes a `seq[string]` to act on a `PersistentVector[Value]`
  ## so that it can be used in a formula to act on a whole DF column.
  ## `toExport` can be set to `false` so that the resulting proc is not exported.
  ## This is useful to lift procs only locally (e.g. in a test case etc.)
  when toExport:
    proc `name`*(v: PersistentVector[Value]): Value =
      result = Value(kind: VString, str: `name`(v[0 ..< v.len].mapIt(it.toStr)))
  else:
    proc `name`(v: PersistentVector[Value]): Value =
      result = Value(kind: VString, str: `name`(v[0 ..< v.len].mapIt(it.toStr)))

template liftScalarFloatProc*(name: untyped,
                              toExport: static bool = true): untyped =
  ## Lifts a proc, which takes a `float` to act on a `Value`
  ## so that it can be used in a formula to act on an element in a DF.
  ## `toExport` can be set to `false` so that the resulting proc is not exported.
  ## This is useful to lift procs only locally (e.g. in a test case etc.)
  when toExport:
    proc `name`*(v: Value): Value =
      result = %~ `name`(v.toFloat)
  else:
    proc `name`(v: Value): Value =
      result = %~ `name`(v.toFloat)

template liftScalarIntProc*(name: untyped,
                           toExport: static bool = true): untyped =
  ## Lifts a proc, which takes a `int` to act on a `Value`
  ## so that it can be used in a formula to act on an element in a DF.
  ## `toExport` can be set to `false` so that the resulting proc is not exported.
  ## This is useful to lift procs only locally (e.g. in a test case etc.)
  when toExport:
    proc `name`*(v: Value): Value =
      result = %~ `name`(v.toInt)
  else:
    proc `name`(v: Value): Value =
      result = %~ `name`(v.toInt)

template liftScalarStringProc*(name: untyped,
                               toExport: static bool = true): untyped =
  ## Lifts a proc, which takes a `string` to act on a `Value`
  ## so that it can be used in a formula to act on an element in a DF.
  ## `toExport` can be set to `false` so that the resulting proc is not exported.
  ## This is useful to lift procs only locally (e.g. in a test case etc.)
  when toExport:
    proc `name`*(v: Value): Value =
      result = %~ `name`(v.toStr)
  else:
    proc `name`(v: Value): Value =
      result = %~ `name`(v.toStr)

proc length*(v: PersistentVector[Value]): Value =
  ## returns the `length` of the given vector (DF column) as a `Value`.
  ## Essentially just a working version of `len` for use in formulas, e.g.
  ## for `summarize`. Does not use the `len` name for two reasons:
  ## 1. Nim does not allow overload by return type
  ## 2. `length` is the name in R
  result = %~ v.len

proc colMin*(df: DataFrame, col: string, ignoreInf = true): float =
  ## Returns the minimum of a DF column.
  ## If `ignoreInf` is true `-Inf` values are ignored. This porc
  ## is mainly used to determine the data scales for a plot and not
  ## as a user facing proc!
  let colVals = df[col].vToSeq
  for i, x in colVals:
    let xFloat = x.toFloat
    if i == 0:
      result = xFloat
    if ignoreInf and classify(xFloat) == fcNegInf:
      continue
    result = min(xFloat, result)

proc colMax*(df: DataFrame, col: string, ignoreInf = true): float =
  ## Returns the maximum of a DF column.
  ## If `ignoreInf` is true `Inf` values are ignored. This proc
  ## is mainly used to determine the data scales for a plot and not
  ## as a user facing proc!
  let colVals = df[col].vToSeq
  for i, x in colVals:
    let xFloat = x.toFloat
    if i == 0:
      result = xFloat
    if ignoreInf and classify(xFloat) == fcInf:
      continue
    result = max(xFloat, result)

liftVectorFloatProc(mean)
liftVectorFloatProc(sum)
liftScalarFloatProc(abs)
liftVectorFloatProc(min)
liftVectorFloatProc(max)

# lifted procs from `stats` module
liftVectorFloatProc(variance)
liftVectorFloatProc(standardDeviation)
liftVectorFloatProc(skewness)
liftVectorFloatProc(kurtosis)

# The following lifted procs are all lifted from the stdlib and the lifting to
# work on seqs is done in seqmath. Not all work atm, since some take additional args
# or return bools
# ---- from math.nim --------------
#liftScalarFloatProc(classify)
#liftScalarFloatProc(binom)
#liftScalarFloatProc(fac)
#liftScalarFloatProc(isPowerOfTwo)
#liftScalarFloatProc(nextPowerOfTwo)
#liftScalarFloatProc(countBits32)
#liftScalarFloatProc(random)
liftScalarFloatProc(sqrt)
liftScalarFloatProc(cbrt)
liftScalarFloatProc(log10)
liftScalarFloatProc(log2)
liftScalarFloatProc(ln)
liftScalarFloatProc(exp)
#liftScalarFloatProc2(fexp)
liftScalarFloatProc(arccos)
liftScalarFloatProc(arcsin)
liftScalarFloatProc(arctan)
#liftScalarFloatProc2(arctan2)
liftScalarFloatProc(cos)
liftScalarFloatProc(cosh)
#liftScalarFloatProc2(hypot)
liftScalarFloatProc(sin)
liftScalarFloatProc(sinh)
liftScalarFloatProc(tan)
liftScalarFloatProc(tanh)
#liftScalarFloatProc2(pow)
liftScalarFloatProc(erf)
liftScalarFloatProc(erfc)
liftScalarFloatProc(lgamma)
liftScalarFloatProc(tgamma)
liftScalarFloatProc(trunc)
liftScalarFloatProc(floor)
liftScalarFloatProc(ceil)
liftScalarFloatProc(degToRad)
liftScalarFloatProc(radToDeg)
#liftScalarFloatProc(gcd)
#liftScalarFloatProc(lcm)



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
  # TODO: this is essentialy the IMPL of `getFuncKind` too!!!
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
    doAssert impl[1].kind in {nnkProcTy, nnkSym}
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
  of nnkCheckedFieldExpr:
    result = fn
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
  of nnkDotExpr, nnkBracketExpr:
    # probably field access of some object
    # echo n.treeRepr
    val = n
  of nnkPrefix:
    doAssert n[0].eqIdent(ident"-")
    doAssert n[1].kind in {nnkIntLit .. nnkFloat64Lit}
    val = n
  else:
    error("Unsupported kind to construct variable " & $n.kind)
  result = quote do:
    FormulaNode(kind: fkVariable, val: %~ `val`)

proc constructFunction*(n: NimNode): NimNode =
  let fname = n[0].repr #.strVal
  let fn = n[0]
  let arg = constructVariable(n[1])
  result = quote do:
    # potentially extract the function from a generic
    let fnArg = extractFunction(`fn`)
    createFormula(`fname`, fnArg, `arg`)

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

proc buildFormula(n: NimNode): NimNode
proc handleInfix(n: NimNode): NimNode =
  ## Builds the formula given by `f{}`
  ## If it is infix, a `fkTerm` is created. If it's a literal a `fkVariable` is
  ## created.
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

  let opid = node[0].repr
  let op = quote do:
    parseEnum[ArithmeticKind](`opid`)
  let lhs = buildFormula(node[1])
  let rhs = buildFormula(node[2])
  result = quote do:
    FormulaNode(kind: fkTerm, lhs: `lhs`, rhs: `rhs`, op: `op`)

proc handlePrefix(n: NimNode): NimNode =
  if n[0].eqIdent(ident"-"):
    case n[1].kind
    of nnkStrLit, nnkRStrLit:
      let rhs = buildFormula(n[1])
      result = quote do:
        FormulaNode(kind: fkTerm, lhs: f{-1}, rhs: `rhs`, op: amMul)
    of nnkIntLit .. nnkFloat64Lit:
      result = constructVariable(n)
    else:
      raise newException(Exception, "Not implemented `nnkPrefix` for " & $n[1].kind)
  else:
    raise newException(Exception, "Not implemented `nnkPrefix` other than `-`! " & $n.repr)


proc buildFormula(n: NimNode): NimNode =
  ## Builds the formula given by `f{}`
  ## If it is infix, a `fkTerm` is created. If it's a literal a `fkVariable` is
  ## created.
  case n.kind
  of nnkInfix:
    result = handleInfix(n)
  of nnkIntLit .. nnkFloat64Lit, nnkStrLit:
    result = constructVariable(n)
  of nnkIdent:
    # should correspond to a known identifier in the calling scope
    result = constructVariable(n)
  of nnkCall:
    result = constructFunction(n)
  of nnkPar:
    result = buildFormula(n[0]) #constructFunction(n[0])
  of nnkDotExpr, nnkBracketExpr:
    result = constructVariable(n)
  of nnkPrefix:
    result = handlePrefix(n)
  else:
    raise newException(Exception, "Not implemented! " & $n.kind)

macro `{}`*(x: untyped{ident}, y: untyped): untyped =
  if x.strVal == "f":
    result = buildFormula(y)

macro `fn`*(x: untyped): untyped =
  let arg = if x.kind == nnkStmtList: x[0] else: x
  expectKind arg, nnkCurly
  result = buildFormula(arg[0])

proc unique*(v: PersistentVector[Value]): seq[Value] =
  ## returns a seq of all unique values in `v`
  result = v.vToSeq.deduplicate

proc calcNewColumn(df: DataFrame, fn: FormulaNode): (string, PersistentVector[Value]) =
  ## calculates a new column based on the `fn` given
  doAssert fn.lhs.kind == fkVariable, " was " & $fn
  doAssert fn.lhs.val.kind == VString, " was " & $fn
  # for column names we don't want explicit highlighting of string numbers, since
  # we are dealing with strings anyways (`emphStrNumber = false`).
  let colName = if fn.lhs.val.kind == VString:
                  fn.lhs.val.str
                else:
                  pretty(fn.lhs.val, emphStrNumber = false)
  # mutable copy so that we can cache the result of `fn(arg)` if such a
  # function call is involved
  var mfn = fn
  var newCol = newSeq[Value](df.len)
  for i in 0 ..< df.len:
    newCol[i] = mfn.rhs.evaluate(df, i)
  result = (colName, toPersistentVector(newCol))

proc selectInplace*[T: string | FormulaNode](df: var DataFrame, cols: varargs[T]) =
  ## Inplace variant of `select` below.
  var toDrop = toHashSet(df.getKeys)
  for fn in cols:
    when type(T) is string:
      toDrop.excl fn
    else:
      if fn.kind == fkVariable:
        doAssert fn.val.kind == VString
        toDrop.excl fn.val.str
      else:
        doAssert fn.rhs.kind == fkVariable, "if you wish to perform a calculation " &
          "of one or more columns, please use `transmute` or `mutate`!"
        echo fn.lhs
        doAssert fn.lhs.val.kind == VString
        doAssert fn.rhs.val.kind == VString
        df[fn.lhs.val.str] = df[fn.rhs.val.str]
        toDrop.excl fn.lhs.val.str
  # now drop all required keys
  for key in toDrop: df.drop(key)

proc select*[T: string | FormulaNode](df: DataFrame, cols: varargs[T]): DataFrame =
  ## Returns the data frame cut to the names given as `cols`. The argument
  ## may either be the name of a column as a string, or a `FormulaNode` describing
  ## either a selection with a name applied in form of an "equation" (c/f mpg dataset):
  ## mySelection ~ hwy
  ## or just an `fkVariable` stating the name of the column. Using the former approach
  ## it's possible to select and rename a column at the same time.
  ## Note that the columns will be ordered from left to right given by the order
  ## of the `cols` argument!
  result = df
  result.selectInplace(cols)

proc mutateImpl(df: var DataFrame, fns: varargs[FormulaNode],
                dropCols: static bool) =
  ## implementation of mutation / transmutation. Allows to statically
  ## decide whether to only keep touched columns or not.
  var colsToKeep: seq[string]
  for fn in fns:
    if fn.kind == fkVariable:
      doAssert fn.val.kind == VString
      colsToKeep.add fn.val.str
    elif fn.kind == fkTerm:
      case fn.op
      of amDep:
        let (colName, newCol) = df.calcNewColumn(fn)
        df[colName] = newCol
        colsToKeep.add colName
      else:
        df[$fn] = fn.evaluate(df)
        colsToKeep.add $fn
    else:
      df[$fn] = fn.evaluate(df)
      colsToKeep.add $fn
  when dropCols:
    df.selectInplace(colsToKeep)

proc mutateInplace*(df: var DataFrame, fns: varargs[FormulaNode]) =
  ## Inplace variant of `mutate` below.
  df.mutateImpl(fns, dropCols = false)

proc mutate*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## Returns the data frame with an additional mutated column, described
  ## by the functions `fns`.
  ## Each formula `fn` given will be used to create a new column in the
  ## dataframe.
  ## We assume that the LHS of the formula corresponds to a fkVariable
  ## that's used to designate the new name.
  ## NOTE: If a given `fn` is a term (`fkTerm`) without an assignment
  ## (using `~`, kind `amDep`) or a function (`fkFunction`), the resulting
  ## column will be named after the stringification of the formula.
  ##
  ## E.g.: `df.mutate(f{"x" * 2})` will add the column `(* x 2)`.
  result = df
  result.mutateInplace(fns)

proc transmuteInplace*(df: var DataFrame, fns: varargs[FormulaNode]) =
  ## Inplace variant of `transmute` below.
  df.mutateImpl(fns, dropCols = true)

proc transmute*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## Returns the data frame cut to the columns created by `fns`, which
  ## should involve a calculation. To only cut to one or more columns
  ## use the `select` proc.
  ## A function may only contain a `fkVariable` in order to keep the
  ## column without modification.
  ## We assume that the LHS of the formula corresponds to a fkVariable
  ## that's used to designate the new name.
  ## NOTE: If a given `fn` is a term (`fkTerm`) without an assignment
  ## (using `~`, kind `amDep`) or a function (`fkFunction`), the resulting
  ## column will be named after the stringification of the formula.
  ##
  ## E.g.: `df.transmute(f{"x" * 2})` will create the column `(* x 2)`.
  # since result dataframe is empty, copy len of input
  result = df
  result.transmuteInplace(fns)

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

proc getColsAsRowsIdx(df: DataFrame, keys: seq[string]): seq[(int, Value)] =
  ## Given a dataframe `df` and column keys `keys`, returns a `seq[(int, Value)]`
  ## where each `Value` is a `VObject` containing a single row, with
  ## (key, value) pairs and `int` contains the index
  # now build the rows
  result = newSeq[(int, Value)](df.len)
  for i in 0 ..< result.len:
    result[i][0] = i
    result[i][1] = newVObject()
    for k in keys:
      result[i][1][k] = (df[k, i])

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
    idxCol.setLen(df.len)
    let col = by[0]
    for i in 0 ..< df.len:
      idxCol[i] = (i, df[col, i])
    idxCol.arrangeSortImpl(order)
  else:
    # in case of having multiple strings to sort by, first create a sequence of all
    # rows (only containig the columns to be sorted)
    idxCol = getColsAsRowsIdx(df, by)
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

proc setDiff*(df1, df2: DataFrame, symmetric = false): DataFrame =
  ## returns a `DataFrame` with all elements in `df1` that are not found in
  ## `df2`. If `symmetric` is true, the symmetric difference of the dataset is
  ## returned, i.e. elements which are either not in `df1` ``or`` not in `df2`.
  ## NOTE: Currently simple implementation based on `HashSet`. Iterates
  ## both dataframes once to generate sets, calcualtes intersection and returns
  ## difference as new `DataFrame`
  ## Considers whole rows for comparison. The result is potentially unsorted!
  template dfToSet(df: DataFrame): untyped =
    var rowSet = HashSet[Value]()
    for row in df:
      rowSet.incl row
    rowSet
  var diff: HashSet[Value]
  if symmetric:
    diff = symmetricDifference(dfToSet(df1), dfToSet(df2))
  else:
    diff = dfToSet(df1) - dfToSet(df2)
  for row in diff:
    result.add row
  result.len = diff.card

proc group_by*(df: DataFrame, by: varargs[string], add = false): DataFrame =
  ## returns a grouped data frame grouped by all keys `by`
  ## A grouped data frame is a lazy affair. It only calculates the groups,
  ## but unless e.g. `summarize` is called on it, remains unchanged.
  ## If `df` is already a grouped data frame and `add` is `true`, the
  ## groups given by `by` will be added as additional groups!
  doAssert by.len > 0, "Need at least one argument to group by!"
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

iterator groups*(df: DataFrame, order = SortOrder.Ascending): (seq[(string, Value)], DataFrame) =
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
  let dfArranged = df.arrange(keys, order = order)
  # having the data frame in a sorted order, walk it and return each combination
  var
    currentKeys = newSeq[(string, Value)](keys.len)
    lastKeys = newSeq[(string, Value)](keys.len)
    startIdx, stopIdx: int # indices which indicate from where to where a subgroup is located
  for i in 0 ..< dfArranged.len:
    for j, key in keys:
      currentKeys[j] = (key, dfArranged[key, i])
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
      lastKeys = currentKeys
    else:
      # should only happen for i == 0
      doAssert i == 0
      lastKeys = currentKeys
  # finally yield the last subgroup or the whole group, in case we only
  # have a single key
  yield (currentKeys, dfArranged[startIdx .. dfArranged.high])

proc summarize*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## returns a data frame with the summaries applied given by `fn`. They
  ## are applied in the order in which they are given
  result = DataFrame(kind: dfNormal)
  var fnEval: FormulaNode
  var lhsName = ""
  for fn in fns:
    if fn.kind == fkTerm:
      doAssert fn.rhs.kind == fkFunction, "`summarize` RHS argument must be a function!"
      doAssert fn.lhs.kind == fkVariable
      lhsName = fn.lhs.val.str
      fnEval = fn.rhs
    else:
      doAssert fn.kind == fkFunction, "`summarize` argument must be a function!"
      lhsName = $fn
      fnEval = fn
    case df.kind
    of dfNormal:
      # just apply the function
      let res = toPersistentVector(@[fnEval.reduce(df)])
      result[lhsName] = res
      result.len = res.len
    of dfGrouped:
      # apply the function to each ``group``
      # TODO: replace by impl which makes use of `groups` iterator? Check `count` impl
      # below!
      for k, classes in df.groupMap:
        for class in classes:
          # add current class to `k`, but only if not already done on a
          # previous function
          if result.hasKey(k) and result[k].len < classes.len:
            result[k] = result[k].add class
          else:
            result[k] = toPersistentVector(@[class])
          var dfcopy = df.filter(f{k == class})
          let x = fnEval.reduce(dfcopy)
          if result.hasKey(lhsName):
            result[lhsName] = result[lhsName].add x
          else:
            result[lhsName] = toPersistentVector(@[x])
        # at some point `k` should have the correct length of the dataframe
        result.len = result[k].len

proc count*(df: DataFrame, col: string, name = "n"): DataFrame =
  ## counts the number of elements per type in `col` of the data frame.
  ## Basically a shorthand for df.group_by.summarize(f{length(col)}).
  ## TODO: handle already grouped dataframes.
  let grouped = df.group_by(col)
  result = DataFrame()
  for class, subdf in groups(grouped):
    let key = class[0][0]
    if key in result:
      result[key] = result[key].add class[0][1]
    else:
      result[key] = toPersistentVector(@[class[0][1]])
    if name in result:
      result[name] = result[name].add (%~ subDf.len)
    else:
      result[name] = toPersistentVector(@[%~ subDf.len])
    inc result.len

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
          result[k] = nullVector(result.len)
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
      result[k] = result[k].add nullVector(result.len - result[k].len)
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

proc add*(df: var DataFrame, dfToAdd: DataFrame) =
  ## The simplest form of "adding" a data frame. If the keys match exactly or
  ## `df` is empty `dfToAdd` will be stacked below. This makes a key check and then
  ## calls `bind_rows` for the job.
  if df.len == 0:
    df = dfToAdd
  else:
    doAssert df.getKeys == dfToAdd.getKeys, "all keys must match to add dataframe!"
    df = bind_rows([("", df), ("", dfToAdd)])

proc head*(df: DataFrame, num: int): DataFrame =
  ## returns the head of the DataFrame. `num` elements
  result = df[0 ..< num]

proc tail*(df: DataFrame, num: int): DataFrame =
  ## returns the tail of the DataFrame. `num` elements
  result = df[^num .. df.high]

proc gather*(df: DataFrame, cols: varargs[string],
             key = "key", value = "value", dropNulls = false): DataFrame =
  ## gathers the `cols` from `df` and merges these columns into two new columns
  ## where the `key` column contains the name of the column from which the `value`
  ## entry is taken. I.e. transforms `cols` from wide to long format.
  let remainCols = getKeys(df).toSet.difference(cols.toSet)
  var newLen = 0
  # TODO: improve this...
  for col in cols:
    var toAdd = newSeqOfCap[Value](df.len)
    var idxToKeep = newSeqOfCap[int](df.len)
    let colVec = df[col]
    for i in 0 ..< df.len:
      if not dropNulls or colVec[i].kind != VNull:
        toAdd.add colVec[i]
        idxToKeep.add i
    if value notin result:
      result[value] = toVector(toAdd)
      result[key] = toVector(toSeq(0 ..< toAdd.len).mapIt(col))
      newLen = toAdd.len
    else:
      result[value] = result[value].add toVector(toAdd)
      result[key] = result[key].add toVector(toSeq(0 ..< toAdd.len).mapIt(col))
      newLen += toAdd.len
    for rem in remainCols:
      var data = newSeq[Value](idxToKeep.len)
      let colVec = df[rem]
      for i in idxToKeep:
        data[i] = colVec[i]
      if rem notin result:
        result[rem] = toVector(data)
      else:
        result[rem] = result[rem].add toVector(data)
  result.len = newLen

proc unique*(df: DataFrame, cols: varargs[string]): DataFrame =
  ## returns a DF with only distinct rows. If one or more `cols` are given
  ## the uniqueness of a row is only determined based on those columns. By
  ## default all columns are considered.
  ## NOTE: The corresponding `dplyr` function is `distinct`. The choice for
  ## `unique` was made, since `distinct` is a keyword in Nim!
  var mcols = @cols
  if mcols.len == 0:
    mcols = getKeys(df)
  # `rowSet` will contain all rows as `VObjects` of the given `cols`. These
  # are used to determine if a row is distinct or not
  var rowSet = initHashSet[Value]()
  # `seqTab` will store all elements that remain in the DF
  var seqTab = initOrderedTable[string, seq[Value]]()
  for i in 0 ..< df.len:
    # get a VObject of all `mcols`
    let row = df.row(i, mcols)
    if row notin rowSet:
      # add whole row to `seqTab`
      for k in keys(df):
        if k notin seqTab:
          seqTab[k] = newSeqOfCap[Value](df.len)
        seqTab[k].add df[k][i]
      rowSet.incl row
  result.len = rowSet.card
  for k in keys(seqTab):
    result[k] = seqTab[k].toPersistentVector

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
    doAssert impl[1].kind in {nnkProcTy, nnkSym}
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


## TODO: We can certainly simplify up the below =evalute=, =reduce= procs below?
proc evaluate*(node: FormulaNode): Value =
  ## evaluates the given `FormulaNode`. This requires the node to be
  ## a pure math node (i.e. does not depend on a data frame column).
  case node.kind
  of fkVariable:
    result = node.val
  of fkTerm:
    case node.op
    of amPlus:
      result = %~ (node.lhs.evaluate + node.rhs.evaluate)
    of amMinus:
      result = %~ (node.lhs.evaluate - node.rhs.evaluate)
    of amMul:
      result = %~ (node.lhs.evaluate * node.rhs.evaluate)
    of amDiv:
      result = %~ (node.lhs.evaluate / node.rhs.evaluate)
    of amGreater:
      result = %~ (node.lhs.evaluate > node.rhs.evaluate)
    of amLess:
      result = %~ (node.lhs.evaluate < node.rhs.evaluate)
    of amGeq:
      result = %~ (node.lhs.evaluate >= node.rhs.evaluate)
    of amLeq:
      result = %~ (node.lhs.evaluate <= node.rhs.evaluate)
    of amAnd:
      result = %~ (node.lhs.evaluate.toBool and node.rhs.evaluate.toBool)
    of amOr:
      result = %~ (node.lhs.evaluate.toBool or node.rhs.evaluate.toBool)
    of amXor:
      result = %~ (node.lhs.evaluate.toBool xor node.rhs.evaluate.toBool)
    of amEqual:
      result = %~ (node.lhs.evaluate == node.rhs.evaluate)
    of amUnequal:
      result = %~ (node.lhs.evaluate != node.rhs.evaluate)
    of amDep:
      raise newException(Exception, "Cannot evaluate `amDep` FormulaNode!")
  of fkFunction:
    case node.fnKind
    of funcScalar:
      result = node.fnS(evaluate(node.arg))
    else:
      raise newException(Exception, "Implement checking compatibility of function " &
        " and its argument type somehow?")

proc evaluate*(node: FormulaNode, data: DataFrame, idx: int): Value =
  case node.kind
  of fkVariable:
    case node.val.kind
    of VString:
      # the given node corresponds to a key of the data frame
      if node.val.str in data:
        result = data[node.val.str][idx]
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
    of amUnequal:
      result = %~ (node.lhs.evaluate(data, idx) != node.rhs.evaluate(data, idx))
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

proc reduce*(node: FormulaNode, data: DataFrame): Value =
  ## Reduces the data frame under a given `FormulaNode`.
  ## It returns a single value from a whole data frame (by working on
  ## a single column)
  case node.kind
  of fkVariable:
    result = node.val
  of fkFunction:
    # for now assert that the argument to the function is just a string
    # Extend this if support for statements like `mean("x" + "y")` (whatever
    # that is even supposed to mean) is to be added.
    doAssert node.arg.kind == fkVariable
    # we also convert to float for the time being. Implement a different proc or make this
    # generic, we want to support functions returning e.g. `string` (maybe to change the
    # field name at runtime via some magic proc)
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
  of fkTerm:
    let lhs = reduce(node.lhs, data)
    let rhs = reduce(node.rhs, data)
    result = evaluate FormulaNode(kind: fkTerm, op: node.op, lhs: f{lhs}, rhs: f{rhs})

proc evaluate*(node: FormulaNode, data: DataFrame): PersistentVector[Value] =
  ## evaluation of a data frame under a given `FormulaNode`. This is a non-reducing
  ## operation. It returns a `PersitentVector[Value]` from a whole data frame (by working on
  ## a single column) and applying `node` to each element.
  case node.kind
  of fkVariable:
    case node.val.kind
    of VString:
      # the given node corresponds to a key of the data frame
      # TODO: maybe extend this so that if `node.val` is ``not`` a key of the dataframe
      # we take the literal string value instead?
      if node.val.str in data:
        result = data[node.val.str]
      else:
        # if it's not a key, we use the literal
        result = toPersistentVector(toSeq(0 ..< data.len).mapIt(node.val))
    of VFloat, VInt, VBool:
      # take the literal value of the node
      result = toPersistentVector(toSeq(0 ..< data.len).mapIt(node.val))
    else:
      raise newException(Exception, "Node kind of " & $node.kind & " does not " &
        "make sense for evaluation!")
  of fkTerm:
    let lhs = evaluate(node.lhs, data)
    let rhs = evaluate(node.rhs, data)
    doAssert lhs.len == rhs.len
    var res = newSeq[Value](lhs.len)
    for i in 0 ..< lhs.len:
      res[i] = evaluate FormulaNode(kind: fkTerm, op: node.op, lhs: f{lhs[i]}, rhs: f{rhs[i]})
    result = toPersistentVector(res)
  of fkFunction:
    case node.fnKind
    of funcScalar:
      # just a function taking a scalar. Apply to current `idx`
      var res = newSeq[Value](data.len)
      for i in 0 ..< data.len:
        res[i] = node.evaluate(data, i)
      result = toPersistentVector(res)
    of funcVector:
      raise newException(Exception, "Reductive vector like proc cannot be evaluated to " &
        "return a vector!")
