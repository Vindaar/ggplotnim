import macros, tables, strutils, options, fenv, sets, hashes, sugar, math
import sequtils, stats, strformat, algorithm, parseutils

# for error messages to print types
import typetraits

from ginger import Scale

import arraymancer
export arraymancer.tensor

import value
export value

import column
export column

# formulaNameMacro contains a macro and type based on the fallback `FormulaNode`,
# which is used to generate the names of each `FormulaNode` in lisp representation
import formulaNameMacro
export formulaNameMacro

type
  FormulaKind* = enum
    fkVariable, fkAssign, fkVector, fkScalar

  FormulaNode* = object
    name*: string # stringification of whole formula. Only for printing and
                  # debugging
    case kind*: FormulaKind
    of fkVariable:
      # just some constant value. Result of a simple computation as a `Value`
      # This is mainly used to rename columns / provide a constant value
      val*: Value
    of fkAssign:
      lhs*: string # can this be something else?
      rhs*: Value
    of fkVector:
      colName*: string
      resType*: ColKind
      fnV*: proc(df: DataFrame): Column
    of fkScalar:
      valName*: string
      valKind*: ValueKind
      fnS*: proc(c: DataFrame): Value

  DataFrameKind = enum
    dfNormal, dfGrouped

  # where value is as usual
  # then
  DataFrame* = ref object
    len*: int
    data*: OrderedTable[string, Column]
    case kind: DataFrameKind
    of dfGrouped:
      # a grouped data frame stores the keys of the groups and maps them to
      # a set of the categories
      groupMap*: OrderedTable[string, HashSet[Value]]
    else: discard

const ValueNull* = Value(kind: VNull)

proc newDataFrame*(size = 8,
                   kind = dfNormal): DataFrame =
  ## initialize a DataFrame, which initializes the table for `size` number
  ## of columns. Given size will be rounded up to the next power of 2!
  result = DataFrame(kind: kind,
                     data: initOrderedTable[string, Column](nextPowerOfTwo(size)),
                     len: 0)

template ncols*(df: DataFrame): int = df.data.len

proc `high`*(df: DataFrame): int = df.len - 1

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

proc raw*(node: FormulaNode): string =
  ## prints the raw stringification of `node`
  result = node.name

proc toUgly*(result: var string, node: FormulaNode) =
  var comma = false
  case node.kind:
  of fkVariable:
    result = $node.val
  of fkAssign:
    result.add "(<- "
    result.add $node.lhs & " "
    result.add $node.rhs & ")"
  of fkVector:
    result = $node.colName
    #result.add "(" & $node.colName & " "
    #result.add $node.resType & ")"
  of fkScalar:
    result = $node.valName
    #result.add "(" & $node.valName & " "
    #result.add $node.valKind & ")"

proc `$`*(node: FormulaNode): string =
  ## Converts `node` to its string representation
  result = newStringOfCap(1024)
  toUgly(result, node)

## The following 3 procs `evaluate` and `reduce` are for API compliance with
## ggplotnims default backend.

func len*[T](t: Tensor[T]): int =
  assert t.shape.len == 1
  result = t.size

#iterator mpairs*(df: var DataFrame): (string, var PersistentVector[Value]) =
#  for k, mval in mpairs(df.data):
#    yield (k, mval)
#
proc drop*(df: var DataFrame, key: string) {.inline.} =
  ## drops the given key from the DataFrame
  df.data.del(key)

#proc add*(v: PersistentVector[Value], w: PersistentVector[Value]): PersistentVector[Value] =
#  ## adds all elements of `w` to `v` and returns the resulting vector
#  if v.len > 100 or w.len > 100:
#    # go the seq conversion route
#    var res = toSeq(v)
#    res.add toSeq(w)
#    result = toPersistentVector(res)
#  else:
#    result = v
#    for x in w:
#      result = result.add x
proc contains*(df: DataFrame, key: string): bool =
  ## Contains proc for `DataFrames`, which checks if the `key` names
  ## a column in the `DataFrame`
  result = df.data.hasKey(key)

proc `[]`*(df: DataFrame, k: string): var Column {.inline.} =
  assert not df.isNil, "DF is used in uninitialized context!"
  result = df.data[k]

proc `[]`*(df: DataFrame, k: Value): Column {.inline.} =
  assert not df.isNil, "DF is used in uninitialized context!"
  result = df.data[k.toStr]

func isColumn*(fn: FormulaNode, df: DataFrame): bool =
  result = $fn in df

func isConstant*(fn: FormulaNode, df: DataFrame): bool =
  result = $fn in df and df[$fn].isConstant

proc `[]`*(df: DataFrame, k: string, idx: int): Value {.inline.} =
  ## returns the element at index `idx` in column `k` directly, without
  ## returning the whole vector first
  assert not df.isNil, "DF is used in uninitialized context!"
  result = df.data[k][idx, Value]

proc `[]`*[T](df: DataFrame, k: string, idx: int, dtype: typedesc[T]): T {.inline.} =
  ## returns the element at index `idx` in column `k` directly, without
  ## returning the whole vector first
  assert not df.isNil, "DF is used in uninitialized context!"
  result = df.data[k][idx, dtype]

proc `[]`*[T](df: DataFrame, k: string, slice: Slice[int], dtype: typedesc[T]): Tensor[T] {.inline.} =
  ## returns the elements in `slice` in column `k` directly, without
  ## returning the whole vector first as a tensor of type `dtype`
  assert not df.isNil, "DF is used in uninitialized context!"
  result = df.data[k][slice.a .. slice.b, dtype]

proc `[]`*(df: DataFrame, k: string, slice: Slice[int]): Column {.inline.} =
  ## returns the elements in `slice` in column `k` directly, without
  ## returning the whole vector first
  assert not df.isNil, "DF is used in uninitialized context!"
  result = df.data[k][slice.a .. slice.b]

proc `[]`*[T](df: DataFrame, key: string, dtype: typedesc[T]): Tensor[T] =
  ## returns the column `key` as a Tensor of type `dtype`. This is useful to
  ## quickly call a procedure on a column of the DF. If `dtype` matches the
  ## actual datatype of the column, this is a no copy operation.
  ##
  ## .. code-block:: nim
  ##
  ##   df["x", int].max
  result = df.data[key].toTensor(dtype)

proc `[]=`*(df: var DataFrame, k: string, col: Column) {.inline.} =
  ## Assigns a full column to the DF. In debug mode it checks that the size of
  ## the input column matches the DF size, unless the DF is empty.
  if df.isNil:
    df = newDataFrame()
  df.data[k] = col
  if df.len == col.len or df.len == 0:
    df.len = col.len
  else:
    raise newException(ValueError, "Given column length of " & $col.len &
      " does not match DF length of: " & $df.len & "!")

proc asgn*(df: var DataFrame, k: string, col: Column) {.inline.} =
  # low level assign, which does not care about sizes of column. Used in `toTab`.
  # Shorter columns are extended afterwards.
  df.data[k] = col

proc clone(data: OrderedTable[string, Column]): OrderedTable[string, Column] =
  ## clones the given table by making sure the columns are copied
  result = initOrderedTable[string, Column]()
  for key in keys(data):
    result[key] = data[key].clone

proc clone*(df: DataFrame): DataFrame =
  ## returns a cloned version of `df` so that the tensors don't share
  ## data.
  # NOTE: This should actually just use `clone` on each tensor, but if
  # we do that, we get random GC segfaults later
  result = DataFrame(kind: df.kind)
  result.len = df.len
  result.data = df.data.clone
  # TODO: raise Nim issue about this. If the next line is in use,
  # we get a GC related segfault when running `testDf`, which happens
  # somewhere within `readCsv` (thus unrelated to this code here) in the
  # test "Reduce data frame using FormulaNode":
  # test case.
  # result.data = df.data.clone
  case df.kind
  of dfGrouped:
    result.groupMap = df.groupMap
  else: discard

template withCombinedType*(df: DataFrame,
                           cols: seq[string],
                           body: untyped): untyped =
  ## A helper template to work with a `dtype` of that encompasses all
  ## data types found in the `cols` of the DataFrame.
  var colKinds = newSeq[ColKind]()
  for k in cols:
    colKinds.add df[k].kind
  let combKind = combinedColKind(colKinds)
  case combKind
  of colInt:
    type dtype {.inject.} = int
    body
  of colFloat:
    type dtype {.inject.} = float
    body
  of colString:
    type dtype {.inject.} = string
    body
  of colBool:
    type dtype {.inject.} = bool
    body
  of colObject:
    type dtype {.inject.} = Value
    body
  of colNone, colConstant: doAssert false, "No valid type!"

proc `[]=`*[T: Tensor | seq | array](df: var DataFrame, k: string, t: T) {.inline.} =
  df[k] = toColumn t

proc `[]=`*[T](df: var DataFrame, k: string, idx: int, val: T) {.inline.} =
  ## WARNING: only use this if you know that `T` is the correct data type!
  # we depend on df.len != df.data.len in `innerJoin` among others. This is a somewhat
  # unsafe procedure!
  assert df.data[k].len > idx, "Invalid index " & $idx & " for DF column of length " & $df.data.len
  when T is float:
    df.data[k].fcol[idx] = val
  elif T is int:
    df.data[k].icol[idx] = val
  elif T is string:
    df.data[k].scol[idx] = val
  elif T is bool:
    df.data[k].bcol[idx] = val
  elif T is Value:
    df.data[k].ocol[idx] = val

proc `[]=`*[T](df: var Dataframe, fn: FormulaNode, key: string, val: T) =
  ## evaluates the `fn` (which needs to be a function returning a bool, i.e. filter)
  ## and assigns a constant value `val` to all rows of column `key` matching the condition
  # eval boolean function on DF
  doAssert fn.kind == fkVector, "Function must be of kind `fkVector` " &
    "(i.e. function acting on a whole column)!"
  let filterIdx = fn.fnV(df)
  doAssert filterIdx.kind == colBool, "Function must return bool values! " &
    "Returns " & $fn.resType
  var col = df[key] # make mutable copy, reference semantics so data will be modified
  let bTensor = filterIdx.bCol
  for idx in 0 ..< bTensor.size:
    if bTensor[idx]: # if condition true
      col[idx] = val
  df[key] = col

proc add*[T: tuple](df: var DataFrame, args: T) =
  ## This procedure adds a given tuple as a new row to the DF. This should
  ## almost always be avoided, because it comes at a huge performance penalty.
  ## Every add causes a new allocation of every tensor of each column of
  ## length (N + 1). Only use this to add ``few`` (!!) rows to a DF. Otherwise
  ## consider storing your intermediate rows to be added in individual seqs
  ## or Tensors (if you know the length in advance) and add the new DF to
  ## the existing one using `bind_rows` or `add`.
  ##
  ## Possibly use the `add` template, which takes a `varargs[untyped]` if you
  ## do not wish to construct a tuple manually.
  ##
  ## NOTE: the input is treated in the order of the columns as they are
  ## stored in the internal `OrderedTable`! Make sure the order is as you
  ## think it is!
  {.warning: "Using `add` to add rows to a DF individually is very slow. Be " &
    "sure to only add very few rows using this proc!".}
  doAssert args.tupleLen == df.ncols
  let keys = df.getKeys()
  var i = 0
  for arg in fields(args):
    df.asgn(keys[i], df[keys[i]].add toColumn(arg))
    inc i
  df.len = df.len + (i div args.tupleLen)

macro varargsToTuple(args: varargs[untyped]): untyped =
  ## helper macro to convert a `varargs` to a tuple
  result = nnkTupleConstr.newTree()
  for arg in args:
    result.add arg

template add*(df: var DataFrame, args: varargs[untyped]): untyped =
  let tup = varargsToTuple(args)
  df.add(tup)

template `^^`(df, i: untyped): untyped =
  (when i is BackwardsIndex: df.len - int(i) else: int(i))

proc `[]`*[T, U](df: DataFrame, rowSlice: HSlice[T, U]): DataFrame =
  ## returns the vertical slice of the data frame given by `rowSlice`.
  let keys = getKeys(df)
  result = newDataFrame(df.ncols)
  let a = (df ^^ rowSlice.a)
  let b = (df ^^ rowSlice.b)
  for k in keys:
    result[k] = df[k, a .. b]
  # add 1, because it's an ``inclusive`` slice!
  result.len = (b - a) + 1

proc row*(df: DataFrame, idx: int, cols: varargs[string]): Value {.inline.} =
  ## Returns the row `idx` of the DataFrame `df` as a `Value` of kind `VObject`.
  ## If `cols` are given, only those columns will appear in the resulting `Value`.
  result = newVObject(length = cols.len)
  let mcols = if cols.len == 0: getKeys(df) else: @cols
  for col in mcols:
    result[col] = df[col][idx, Value]

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
    result.add "Dataframe with " & $df.getKeys.len & " columns and " & $df.len & " rows:\n"
  let alignBy = max(maxLen + precision, 10)
  let num = if numLines > 0: min(df.len, numLines) else: df.len
  # write header
  result.add align("Idx", alignBy)
  for k in keys(df):
    result.add align($k, alignBy)
  result.add "\n"
  # now add data types
  result.add align("dtype:", alignBy)
  for k in keys(df):
    result.add align(toNimType(df[k].kind), alignBy)
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

proc extendShortColumns*(df: var DataFrame) =
  ## initial calls to `seqsToDf` and other procs may result in a ragged DF, which
  ## has less entries in certain columns than the data frame length.
  ## This proc fills up the mutable dataframe in those columns
  for k in keys(df):
    if df[k].len < df.len:
      let nFill = df.len - df[k].len
      df[k] = df[k].add nullColumn(nFill)

proc toDf*(t: OrderedTable[string, seq[string]]): DataFrame =
  ## creates a data frame from a table of seq[string]
  ## NOTE: This proc assumes that the given entries in the `seq[string]`
  ## have been cleaned of white space. The `readCsv` proc takes care of
  ## this.
  ## TODO: currently does not allow to parse bool!
  result = DataFrame(len: 0)
  for k, v in t:
    var col = newColumn()
    # check first element of v for type
    if v.len > 0:
      # TODO: CLEAN UP
      var maybeNumber = v[0].isNumber
      var maybeInt = v[0].isInt
      if maybeNumber and maybeInt:
        # try as int
        try:
          let data = v.mapIt(it.parseInt)
          col = data.toColumn
        except ValueError:
          try:
            let data = v.mapIt(it.parseFloat)
            col = data.toColumn
          except ValueError:
            # then parse as value
            var data = newSeq[Value](v.len)
            for i, x in v:
              try:
                data[i] = %~ x.parseInt
              except ValueError:
                try:
                  data[i] = %~ x.parseFloat
                except ValueError:
                  data[i] = %~ x
            col = toColumn data
      elif maybeNumber:
        try:
          let data = v.mapIt(it.parseFloat)
          col = data.toColumn
        except ValueError:
          # then parse as value
          var data = newSeq[Value](v.len)
          for i, x in v:
            try:
              data[i] = %~ x.parseFloat
            except ValueError:
              data[i] = %~ x
          col = data.toColumn
      else:
        # try bool?
        try:
          let data = v.mapIt(it.parseBool)
          col = data.toColumn
        except ValueError:
          # keep as string
          col = v.toColumn
    result.data[k] = col
    result.len = max(result.data[k].len, result.len)
  result.extendShortColumns()

proc toDf*(t: OrderedTable[string, seq[Value]]): DataFrame =
  ## creates a data frame from a table of `seq[Value]`. Simply have to convert
  ## the `seq[Value]` to a `Column` and add to DF.
  result = DataFrame(len: 0)
  for k, v in t:
    result[k] = v.toColumn
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
    var `data` = newDataFrame()
  for a in s:
    case a.kind
    of nnkIdent:
      let key = a.strVal
      result.add quote do:
        asgn(`data`, `key`, `a`.toColumn)
        `data`.len = max(`data`.len, `a`.len)
    of nnkExprColonExpr:
      let nameCh = a[0]
      let seqCh = a[1]
      result.add quote do:
        asgn(`data`, `nameCh`, `seqCh`.toColumn)
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

template colsToDf*(s: varargs[untyped]): untyped =
  ## converts an arbitrary number of columns to a `DataFrame` or any
  ## number of key / value pairs where we have string / seq[T] pairs.
  toTab(s)

template dataFrame*(s: varargs[untyped]): untyped =
  ## alias for `seqsToDf`
  toTab(s)

template toDf*(s: varargs[untyped]): untyped =
  ## alias for `seqsToDf`
  toTab(s)

proc hasKey(df: DataFrame, key: string): bool =
  result = df.data.hasKey(key)

iterator items*(df: DataFrame): Value =
  # returns each row of the dataframe as a Value of kind VObject
  for i in 0 ..< df.len:
    yield df.row(i)

iterator values*(df: DataFrame, cols: varargs[string]): Tensor[Value] {.inline.} =
  ## yields all `cols` of `df` as `Tensor[Value]` rows
  let mcols = if cols.len == 0: getKeys(df) else: @cols
  var res = newTensor[Value](mcols.len)
  # fill col seq with column references, so that we don't have to hash the keys
  # every single iteration
  var colSeq = newSeq[Column](mcols.len)
  for idx, k in mcols:
    colSeq[idx] = df.data[k]
  for idx in 0 ..< df.len:
    for j in 0 ..< mcols.len:
      res[j] = colSeq[j][idx, Value]
    yield res

proc get*(df: DataFrame, key: string): Column {.inline.} =
  if key in df:
    result = df[key]
  else:
    # create column of constants or raise?
    raise newException(KeyError, "Given string " & $key & " is not a valid column!")

#proc createFormula(

#colMin(df, $s.col), high: colMax(df, $s.col))
proc colMax*(df: DataFrame, col: string, ignoreInf = true): float =
  ## Returns the maximum of a given `seq[Value]`.
  ## If `ignoreInf` is true `Inf` values are ignored. This porc
  ## is mainly used to determine the data scales for a plot and not
  ## as a user facing proc!
  let t = df[col].toTensor(float)
  var idx = 0
  for x in t:
    if idx == 0:
      result = x
    if ignoreInf and classify(x) == fcInf:
      inc idx
      continue
    result = max(x, result)
    inc idx

proc colMin*(df: DataFrame, col: string, ignoreInf = true): float =
  ## Returns the minimum of a given `seq[Value]`.
  ## If `ignoreInf` is true `-Inf` values are ignored. This porc
  ## is mainly used to determine the data scales for a plot and not
  ## as a user facing proc!
  let t = df[col].toTensor(float)
  var idx = 0
  for x in t:
    if idx == 0:
      result = x
    if ignoreInf and classify(x) == fcNegInf:
      inc idx
      continue
    result = min(x, result)
    inc idx

proc scaleFromData*(c: Column, ignoreInf: static bool = true): ginger.Scale =
  ## Combination of `colMin`, `colMax` to avoid running over the data
  ## twice. For large DFs to plot this makes a big difference.
  if c.len == 0: return (low: 0.0, high: 0.0)
  let t = c.toTensor(float, dropNulls = true)
  var
    minVal = t[0]
    maxVal = t[0]
  for x in t:
    when ignoreInf:
      if (classify(x) == fcNegInf or
          classify(x) == fcInf):
        continue
    minVal = min(x, minVal)
    maxVal = max(x, maxVal)
  result = (low: minVal, high: maxVal)

proc reorderRawTilde(n: NimNode, tilde: NimNode): NimNode =
  ## a helper proc to reorder an nnkInfix tree according to the
  ## `~` contained in it, so that `~` is at the top tree.
  ## (the actual result is simply the tree reordered, but without
  ## the tilde. Reassembly must happen outside this proc)
  result = copyNimTree(n)
  for i, ch in n:
    case ch.kind
    of nnkIdent, nnkStrLit, nnkIntLit .. nnkFloat64Lit, nnkPar, nnkCall,
       nnkAccQuoted, nnkCallStrLit:
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
      if found.kind != nnkNilLit:
        result = found


type
  ReplaceKind = enum
    byValue, # replace identifier (`c"someString"`, etc.) by `t[idx]`
    byTensor # replace identifier (`c"someString"`, etc.) by `t`

proc replaceColumns(body: NimNode, idents: var seq[NimNode],
                    typeNodeTuples: seq[(ReplaceKind, NimNode)],
                    rpKind: ReplaceKind): NimNode

proc replaceDfNodes(n: NimNode, idents: var seq[NimNode],
                    typeNodeTuples: seq[(ReplaceKind, NimNode)],
                    rpKind: ReplaceKind): NimNode =
  result = copyNimTree(n)
  expectKind(n, nnkBracketExpr)
  case result[0].kind
  of nnkBracketExpr:
    case result[0][0].kind
    of nnkIdent:
      if eqIdent(result[0][0], ident"df"):
        # replace `nnkExprColonExpr` node w/ `colName_...`.
        # `idx` part already in place and will stay
        doAssert result[1].kind == nnkIdent
        doAssert eqIdent(result[1], ident"idx")
        result[0] = idents.pop
    else: discard
  of nnkIdent:
    if eqIdent(result[0], ident"df"):
      # replace whole node simply by `colName_...`. N
      result = idents.pop
  else:
    result = replaceColumns(result, idents,
                            typeNodeTuples,
                            rpKind = rpKind)

proc replaceColumns(body: NimNode, idents: var seq[NimNode],
                    typeNodeTuples: seq[(ReplaceKind, NimNode)],
                    rpKind: ReplaceKind): NimNode =
  result = copyNimTree(body)
  # essentially we have to do the following:
  # - replace all strings by calls to `df["String"]`
  #   how do we determine if something should be a call?
  #   use `get` proc, which returns string as value if not a
  #   valid column? Or raise in that case?
  # - determine the resulting data type of the proc. How?
  #   If arithmetic:
  #   - +, -, *, /, mod
  #   in formula body, then it's float
  #   elif and, or, xor, not in body, then it's bool
  #   else string?
  for i in 0 ..< body.len:
    case body[i].kind
    of nnkCall:
      # if a call, take into account `typeNodeTuples` on how to replace content
      # and recurse with overriden `rpKind`
      for (thisRpKind, name) in typeNodeTuples:
        if eqIdent(name, body[i][0]):
          # replace it with given rpKind
          result[i] = replaceColumns(body[i], idents, typeNodeTuples,
                                     rpKind = thisRpKind)
    of nnkAccQuoted, nnkCallStrLit:
      case rpKind
      of byValue:
        let idIdx = ident"idx"
        result[i] = nnkBracketExpr.newTree(idents.pop,
                                           idIdx)
        #nnkCall.newTree(ident"get", ident"df", body[i])
      of byTensor:
        # just the full column
        # TODO: change such that we determine which ident needs to be
        # scalar treated and which vector, based on determining
        # the formula kind the way we do in default backend!
        result[i] = idents.pop
    of nnkBracketExpr:
      result[i] = replaceDfNodes(result[i], idents, typeNodeTuples, rpKind)
    else:
      result[i] = replaceColumns(body[i], idents,
                                 typeNodeTuples,
                                 rpKind = rpKind)

proc unwrapAccQuote(n: NimNode, strAllowed: static bool = false): NimNode =
  case n.kind
  of nnkAccQuoted:
    # remove the accented quote and replace it by a string literal
    if n.len > 0:
      # concat individual parts
      var str: string
      for ch in n:
        str.add ch.strVal & " "
      result = newLit str[0 .. ^2] # remove last space
    else:
      result = n[0].toStrLit
  of nnkCallStrLit:
    result = n[1]
  of nnkStrLit:
    when strAllowed:
      result = n
    else:
      result = n
  else: result = n

proc collectColumns(body: NimNode): seq[NimNode]
proc collectDfNode(n: NimNode): seq[NimNode] =
  expectKind(n, nnkBracketExpr)
  case n[0].kind
  of nnkBracketExpr:
    # is of form: `df["colName"][idx]`
    if eqIdent(n[0][0], ident"df"):
      result.add unwrapAccQuote(n[0][1], strAllowed = true)
    else:
      result.add collectColumns(n[0])
  of nnkIdent:
    if eqIdent(n[0], ident"df"):
      # refers to a column
      result.add unwrapAccQuote(n[1], strAllowed = true)
  else:
    result.add collectColumns(n[0])

proc collectColumns(body: NimNode): seq[NimNode] =
  result = newSeq[NimNode]()
  for i in 0 ..< body.len:
    case body[i].kind
    of nnkAccQuoted, nnkCallStrLit:
      result.add unwrapAccQuote(body[i])
    of nnkBracketExpr:
      result.add collectDfNode(body[i])
    else:
      result.add collectColumns(body[i])

proc genIdentsFromColumns(columns: seq[NimNode]): seq[NimNode] =
  result = newSeq[NimNode]()
  var cols = initOrderedTable[string, string]()
  for i, c in columns:
    if c.strVal notin cols:
      let col = "col" & $i
      result.add genSym(nskVar, ident = col)
      cols[c.strVal] = col
    else:
      let el = result.filterIt(it.strVal == cols[c.strVal])[0]
      result.add el

func genColDefsFromIdents(idents: seq[NimNode],
                          columns: seq[NimNode],
                          funcKind: FormulaKind,
                          resSym: NimNode,
                          dtype, resDtype: NimNode): NimNode =
  result = nnkVarSection.newTree()
  var added = initHashSet[string]()
  for i, c in idents:
    if idents[i].strVal notin added:
      let rhs = nnkCall.newTree(ident"toTensor",
                                nnkBracketExpr.newTree(ident"df",
                                                       columns[i]),
                                dtype)
      result.add nnkIdentDefs.newTree(
        idents[i],
        newEmptyNode(),
        rhs)
      added.incl idents[i].strVal

  # now add `res` tensor
  let dfIdent = ident"df"
  case funcKind
  of fkScalar:
    result.add nnkIdentDefs.newTree(
      resSym,
      resDtype,
      newEmptyNode())
  of fkVector:
    result.add nnkIdentDefs.newTree(
      resSym,
      newEmptyNode(),
      nnkCall.newTree(nnkBracketExpr.newTree(ident"newTensor",
                                             resDtype),
                      nnkDotExpr.newTree(dfIdent,
                                         ident"len"))
    )
  else: error("not supported")

proc compileVectorFormula(rawName, name, body, dtype, resDtype: NimNode,
                          typeNodeTuples: seq[(ReplaceKind, NimNode)],
                          isRaw: bool): NimNode =
  let columns = collectColumns(body)
  var idents = genIdentsFromColumns(columns)
  let resSym = genSym(nskVar, ident = "res")
  let colDefs = genColDefsFromIdents(idents, columns, fkVector,
                                     resSym,
                                     dtype, resDtype)
  # reverse the idents, since we use `pop`
  idents.reverse()
  let forLoopBody = replaceColumns(body, idents,
                                   typeNodeTuples,
                                   rpKind = byValue)
  let
    idIdx = ident"idx"
    dfIdent = ident"df"
  let resultId = ident"result"
  let bodyFinal = quote do:
    `colDefs`
    for `idIdx` in 0 ..< `dfIdent`.len:
      `resSym`[`idIdx`] = `forLoopBody`
    `resultId` = toColumn `resSym`
  # given columns
  let containsDfAccess = true
  var procImpl: NimNode
  if containsDfAccess:
    let params = [ident"Column",
                  nnkIdentDefs.newTree(ident"df",
                                       ident"DataFrame",
                                       newEmptyNode())]
    procImpl = newProc(newEmptyNode(),
                       params = params,
                       body = bodyFinal,
                       procType = nnkLambda)
  var colName = name
  if colName.kind == nnkNilLit:
    # means there is no LHS (implicit fkVector). Use stringification of formula
    # as name for column
    colName = rawName
  result = quote do:
    FormulaNode(name: `rawName`,
                colName: `colName`, kind: fkVector,
                resType: toColKind(`dtype`),
                fnV: `procImpl`)
  #echo result.repr

proc compileScalarFormula(rawName, name, body, dtype, resDtype: NimNode,
                          typeNodeTuples: seq[(ReplaceKind, NimNode)],
                          isRaw: bool): NimNode =
  let columns = collectColumns(body)
  var idents = genIdentsFromColumns(columns)
  let resSym = genSym(nskVar, ident = "res")
  let colDefs = genColDefsFromIdents(idents, columns, fkScalar,
                                     resSym,
                                     dtype, resDtype)
  # reverse the idents, since we use `pop`
  idents.reverse()
  let scalarBody = replaceColumns(body, idents,
                                  typeNodeTuples,
                                  rpKind = byTensor)
  let idIdx = ident"idx"
  let resultId = ident"result"

  let bodyFinal = quote do:
    `colDefs`
    `resSym` = `scalarBody`
    `resultId` = %~ (`resSym`)
  # given columns
  let containsDfAccess = true
  var procImpl: NimNode
  if containsDfAccess:
    let params = [ident"Value",
                  nnkIdentDefs.newTree(ident"df",
                                       ident"DataFrame",
                                       newEmptyNode())]
    procImpl = newProc(newEmptyNode(),
                       params = params,
                       body = bodyFinal,
                       procType = nnkLambda)
  let valName = if name.kind == nnkNilLit: rawName else: name
  result = quote do:
    FormulaNode(name: `rawName`,
                valName: `valName`, kind: fkScalar,
                valKind: toValKind(`dtype`),
                fnS: `procImpl`)
  #echo result.repr

proc checkDtype(body: NimNode,
                floatSet: HashSet[string],
                stringSet: HashSet[string],
                boolSet: HashSet[string]):
                  tuple[isFloat: bool,
                        isString: bool,
                        isBool: bool] =
  for i in 0 ..< body.len:
    case body[i].kind
    of nnkIdent:
      # check
      result = (isFloat: body[i].strVal in floatSet or result.isFloat,
                isString: body[i].strVal in stringSet or result.isString,
                isBool: body[i].strVal in boolSet or result.isBool)
    of nnkCallStrLit:
      # skip this node completely, don't traverse further, since it represents
      # a column!
      continue
    of nnkStrLit, nnkTripleStrLit, nnkRStrLit:
      result.isString = true
    of nnkIntLit .. nnkFloat64Lit:
      result.isFloat = true
    else:
      let res = checkDtype(body[i], floatSet, stringSet, boolSet)
      result = (isFloat: result.isFloat or res.isFloat,
                isString: result.isString or res.isString,
                isBool: result.isBool or res.isBool)

proc determineFormulaKind(body: NimNode): FormulaKind =
  result = fkVariable
  for i in 0 ..< body.len:
    case body[i].kind
    of nnkAccQuoted, nnkCallStrLit: #, nnkStrLit .. nnkTripleStrLit:
      # assume this refers to a column, so vector
      # (how to diff scalar?)
      result = fkVector
    of nnkBracketExpr:
      if eqIdent(body[i][0], ident"df"):
        # refers to a column
        result = fkVector
      else: discard
    of nnkIntLit .. nnkFloat64Lit:
      if result != fkVector:
        # if already a vector, leave it
        result = fkVariable
    else:
      let res = determineFormulaKind(body[i])
      result = if res != fkVariable: res else: result

proc constrFormula(body: NimNode): NimNode =
  result = newNilLit()
  case body.kind
  of nnkCall:
    let fn = body[0]
    result = quote do:
      #let fn = extractFunction(`fn`)
      `fn`
  else: discard

  for i in 0 ..< body.len:
    case body[i].kind
    #of nnkAccQuoted: #nnkStrLit .. nnkTripleStrLit:
    #  # assume this refers to a column, so vector
    #  # (how to diff scalar?)
    #  result = fkVector
    #of nnkIntLit .. nnkFloat64Lit:
    #  if result != fkVector:
    #    # if already a vector, leave it
    #    result = fkVariable
    of nnkCall:
      let fn = body[i][0]
      result = quote do:
        `fn`
        #let fn = extractFunction(`fn`)
    else:
      var res = constrFormula(body[i])
      if res.kind != nnkNilLit:
        result = res

template newTensorHelper(dtype: untyped): untyped =
  newTensor[dtype](0)

template newLiteral(dtype: untyped): untyped =
  when dtype is SomeNumber:
    dtype(0)
  elif dtype is string:
    ""
  elif dtype is Value:
    %~ 0
  else:
    error("BAD TYPE")

proc isValid(n: NimNode): bool =
  result = compiles(n)

proc handleDfNodes(n: NimNode, dtype: NimNode): NimNode =
  expectKind(n, nnkBracketExpr)
  result = n
  case n[0].kind
  of nnkBracketExpr:
    # is of form: `df["colName"][idx]`
    if eqIdent(n[0][0], ident"df"):
      result = getAst(newLiteral(dtype))
  else:
    if eqIdent(n[0], ident"df"):
      result = getAst(newTensorHelper(dtype))

proc extractIdents(n: NimNode, dtype: NimNode,
                   fkKind: FormulaKind): NimNode =
  result = nnkBracket.newTree()
  case n.kind
  of nnkCall:
    let id = n[0]
    var callNode = copyNimTree(n)
    # walk the call and replace `
    for i in 0 ..< callNode.len:
      case callNode[i].kind
      of nnkCallStrLit, nnkAccQuoted:
        callNode[i] = getAst(newTensorHelper(`dtype`))
      of nnkBracketExpr:
        callNode[i] = handleDfNodes(callNode[i], dtype)
      else: discard
    result.add callNode
  of nnkCallStrLit, nnkAccQuoted:
    # call `newDataFrame` to have a DF we can use to later extract `Column` using `n`
    # this way we don't need a locally defined DF of a certain name, since we don't
    # use this type only to deduce the types anyways.
    result.add nnkBracketExpr.newTree(nnkCall.newTree(ident"newDataFrame"), unwrapAccQuote(n))
  else:
    for i in 0 ..< n.len:
      let idents = extractIdents(n[i], dtype, fkKind)
      for el in idents:
        result.add el

proc determineFuncKind(body: NimNode,
                       typeHints: tuple[dtype, resDtype: NimNode],
                       name: NimNode):
     tuple[dtype: NimNode,
           resDtype: NimNode,
           fkKind: FormulaKind] =
  ## checks for certain ... to  determine both the probable
  ## data type for a computation and the `FormulaKind`
  if body.len == 0:
    # a literal or an identifier
    case body.kind
    of nnkIdent:
      result = (newNilLit(), # unknown type since untyped macro
                newNilLit(),
                fkVariable)
    of nnkIntLit .. nnkUInt64Lit:
      result = (ident"int",
                ident"int",
                fkVariable)
    of nnkFloatLit .. nnkFloat64Lit:
      result = (ident"float",
                ident"float",
                fkVariable)
    of nnkCharLit, nnkStrLit, nnkTripleStrLit, nnkRStrLit:
      result = (ident"string",
                ident"string",
                fkVariable)
    else:
      doAssert false, "Weird kind: " & $body.kind & " for body " & $body.repr
    #if typeHint != nnkNilLit:
    #  # override both dtypes
    #  result = (typeHint, typeHint, fk
  else:
    # if more than one element, have to be a bit smarter about it
    # we use the following heuristics
    # - if `+, -, *, /, mod` involved, return as `float`
    #   `TODO:` can we somehow leave pure `int` calcs as `int`?
    # - if `&`, `$` involved, result is string
    # - if `and`, `or`, `xor`, `>`, `<`, `>=`, `<=`, `==`, `!=` involved
    #   result is considered `bool`
    # The priority of these is,
    # - 1. bool
    # - 2. string
    # - 3. float
    # which allows for something like
    # `"10." & "5" == $(val + 0.5)` as a valid bool expression
    # walk tree and check for symbols
    const floatSet = toHashSet(@["+", "-", "*", "/", "mod"])
    const stringSet = toHashSet(@["&", "$"])
    const boolSet = toHashSet(@["and", "or", "xor", ">", "<", ">=", "<=", "==", "!=",
                            "true", "false", "in", "notin"])
    let (isFloat, isString, isBool) = checkDtype(body, floatSet, stringSet, boolSet)
    result[0] = newNilLit()
    result[1] = newNilLit()
    if isFloat:
      result[0] = ident"float"
      result[1] = ident"float"
    if isString:
      # overrides float if it appears
      result[0] = ident"string"
      result[1] = ident"string"
    if isBool:
      # overrides float and string if it appears
      if isString:
        result[0] = ident"string"
      elif isFloat:
        result[0] = ident"float"
      else:
        # is bool tensor
        result[0] = ident"bool"
      # result is definitely bool
      result[1] = ident"bool"

    # apply typeHint if available (overrides above)
    let typeHintDtype = typeHints.dtype
    let typeHintResDtype = typeHints.resDtype
    if typeHintDtype.kind != nnkNilLit:
      if isBool:
        # we don't override bool result type.
        # in cases like:
        # `f{int: x > 4}` the are sure of the result, apply to col only
        result[0] = typeHintDtype
      elif isFloat or isString:
        # override dtype, result still clear
        result[0] = typeHintDtype
      else:
        # set both
        result[0] = typeHintDtype
        result[1] = typeHintDtype
    if typeHintResDtype.kind != nnkNilLit:
      # also assign result type. In this case override regardless of automatic
      # determination
      result[1] = typeHintResDtype
    if result[0].kind == nnkNilLit or result[1].kind == nnkNilLit:
      # attempt via formula
      error("Could not determine data types of tensors in formula:\n" &
        "  name: " & $name.repr & "\n" &
        "  formula: " & $body.repr & "\n" &
        "  data type: " & $result[0].repr & "\n" &
        "  output data type: " & $result[1].repr & "\n" &
        "Consider giving type hints via: `f{T -> U: <theFormula>}`")


    # finally determine the FormulaKind
    # TODO: for now we just assume that raw string literals are supposed
    # to refer to columns. We will provide a different macro entry to
    # explicitly refer to non DF related formulas
    # TODO2: we might want to consider
    result[2] = determineFormulaKind(body)

proc handleCall(id: NimNode): (ReplaceKind, NimNode) =
  let impl = id[0].getTypeImpl
  case impl[0][0].kind
  of nnkSym:
    ## TODO: make sure the symbol actually refers to a valid type?
    case impl[0][0].strVal
    of "Column":
      result = (byValue, id[0])
    else:
      # only `byTensor` if input is also `Tensor`
      case impl[0][1][1].kind
      of nnkBracketExpr:
        doAssert impl[0][1][1][0].strVal in ["seq", "Tensor"]
        result = (byTensor, id[0])
      else:
        result = (byValue, id[0])
  of nnkBracketExpr:
    doAssert eqIdent(impl[0][1][1][0], "Tensor")
    result = (byValue, id[0])
  else: discard

proc extractTypes(idents: NimNode): seq[(ReplaceKind, NimNode)] =
  ##
  expectKind idents, nnkBracket
  for id in idents:
    case id.kind
    of nnkIdent: discard
    of nnkCall: result.add handleCall(id)
    of nnkHiddenDeref: result.add handleCall(id[0])
    else:
      discard

proc parseBool(n: NimNode): bool =
  when (NimMajor, NimMinor, NimPatch) >= (1, 3, 5):
    # change to named tuples on Nim devel
    result = n.eqIdent(ident"true")
  else:
    # stable 1.2
    result = n[1].eqIdent(ident"true")

macro compileFormulaImpl*(rawName, name, body: untyped,
                          bools: untyped,
                          dtype: untyped,
                          resDtype: untyped,
                          funcKind: untyped,
                          typedCalls: varargs[typed]): untyped =
  let typeNodeTuples = extractTypes(typedCalls)
  let
    isAssignment = parseBool(bools[0])
    isVector = parseBool(bools[1])
    isReduce = parseBool(bools[2])
    isRaw = parseBool(bools[3])
    isInplace = parseBool(bools[4])

  # possibly override

  # force `fkVariable` if this is an `<-` assignment
  var allowOverride = false
  var mFuncKind = if isAssignment: fkAssign
                  elif isReduce: fkScalar
                  elif isVector: fkVector
                  else:
                    allowOverride = true
                    FormulaKind(funcKind[1].intVal)
  # possibly override formulaKind yet again due to `typeNodeTuples`
  if typeNodeTuples.len > 0 and allowOverride:
    # if a single `byValue` is involved, the output cannot be a scalar!
    mFuncKind = if typeNodeTuples.allIt(it[0] == byTensor): fkScalar
                else: fkVector

  if not isInplace:
    case mFuncKind
    of fkVariable:
      let val = unwrapAccQuote(body)
      result = quote do:
        FormulaNode(kind: fkVariable,
                    name: `rawName`,
                    val: %~ `val`)
    of fkAssign:
      let rhs = unwrapAccQuote(body)
      result = quote do:
        FormulaNode(kind: fkAssign,
                    name: `rawName`,
                    lhs: `name`,
                    rhs: %~ `rhs`)
    of fkVector:
      result = compileVectorFormula(rawName, name, body, dtype, resDtype,
                                    typeNodeTuples = typeNodeTuples,
                                    isRaw = isRaw)
    of fkScalar:
      result = compileScalarFormula(rawName, name, body, dtype, resDtype,
                                    typeNodeTuples = typeNodeTuples,
                                    isRaw = isRaw)
  else:
    #result = compileInplaceScalar(rawName, name, body, dtype, resDtype,
    #                              typeNodeTuples = typeNodeTuples,
    #                              isRaw = isRaw)
    error("Inplace formulas not yet fully implemented!")

proc compileFormulaTypedMacro(rawName, name, body: NimNode,
                              isAssignment: bool,
                              isVector: bool,
                              isReduce: bool,
                              isRaw: bool,
                              isInplace: bool,
                              typeHints: tuple[dtype, resDtype: NimNode],
                             ): NimNode =
  # essentially:
  # extract every nnkCall
  # for every ident hand it to the typed macro as a sequence.
  # Resolve each ident.
  # If symbol points to a variable, get type:
  # - If scalar, insert directly into body
  #   - if string, assume means a column. Introduce fallback for
  #     real strings? `s( stringIdent )` for example?
  #   - if float, leave as is
  # - If vector, assume same number as idx as DF, iterate over it
  var (dtype, resDtype, funcKind) = determineFuncKind(body, typeHints = typeHints,
                                                      name = name)
  let typedCalls = extractIdents(body, dtype, funcKind)
  let bools = (isAssignment, isVector, isReduce, isRaw, isInplace)
  result = nnkCall.newTree(ident"compileFormulaImpl")
  result.add rawName
  result.add name
  result.add body
  result.add newLit bools
  result.add dtype
  result.add resDtype
  result.add newLit funcKind
  for tk in typedCalls:
    result.add tk
  #result = quote do:
  #    when compiles(`result`):
  #      `result`
  #    else: f{0}

proc parseTypeHints(n: var NimNode): tuple[dtype, resDtype: NimNode] =
  case n.kind
  of nnkExprColonExpr:
    case n[0].kind
    of nnkIdent:
      # simple type hint for tensor input type
      result.dtype = n[0]
      result.resDtype = newNilLit()
    of nnkInfix:
      doAssert n[0].len == 3
      doAssert eqIdent(n[0][0], ident"->")
      # type hint of tensor + result
      result.dtype = n[0][1]
      result.resDtype = n[0][2]
    else: error("Unsupported type hint: " & $n[0].repr)
    n = copyNimTree(n[1])
  else: discard # no type hint

proc compileFormula(n: NimNode, isRaw: bool): NimNode =
  var isAssignment = false
  var isReduce = false
  var isVector = false
  var isInplace = false
  # extract possible type hint
  var node = n
  let typeHints = parseTypeHints(node)
  let tilde = recurseFind(node,
                          cond = ident"~")
  var formulaName = newNilLit()
  var formulaRhs = newNilLit()
  if tilde.kind != nnkNilLit and node[0].ident != toNimIdent"~":
    # only reorder the tree, if it does contain a tilde and the
    # tree is not already ordered (i.e. nnkInfix at top with tilde as
    # LHS)
    let replaced = reorderRawTilde(node, tilde)
    let full = nnkInfix.newTree(tilde[0],
                                tilde[1],
                                replaced)
    node = full
    formulaName = node[1]
    formulaRhs = node[2]
  if tilde.kind == nnkNilLit:
    # check for `<-` assignment
    if node.len > 0 and eqIdent(node[0], ident"<-"):
      # this is an assignment `w/o` access of DF column
      formulaName = unwrapAccQuote(node[1])
      #else: error("Unsupported LHS of type " & $node[1].kind & " : " & $node[1].repr)
      formulaRhs = node[2]
      isAssignment = true
    elif node.len > 0 and eqIdent(node[0], ident"<<"):
      # this is an assignment `w/o` access of DF column
      formulaName = unwrapAccQuote(node[1])
      #else: error("Unsupported LHS of type " & $node[1].kind & " : " & $node[1].repr)
      formulaRhs = node[2]
      isReduce = true
    elif node.len > 0 and eqIdent(node[1], ident"res"):
      isInplace = true
      formulaName = node[1]
      formulaRhs = node
    else:
      #if node.len == 0:
      #  formulaName = node
      #else:
      #  formulaRhs = node
      formulaRhs = node
  else:
    isVector = true
    formulaName = unwrapAccQuote(node[1])
    formulaRhs = node[2]

  let fnName = buildFormula(node)
  let rawName = quote do:
    $(`fnName`)
  result = quote do:
    compileFormula
  result = compileFormulaTypedMacro(rawName, formulaName, formulaRhs,
                                    isAssignment = isAssignment,
                                    isVector = isVector,
                                    isReduce = isReduce,
                                    isRaw = isRaw,
                                    isInplace = isInplace,
                                    typeHints = typeHints)

macro `{}`*(x: untyped{ident}, y: untyped): untyped =
  ## TODO: add some ability to explicitly create formulas of
  ## different kinds more easily! Essentially force the type without
  ## a check to avoid having to rely on heuristics.
  ## Use
  ## - `<-` for assignment
  ## - `<<` for reduce operations, i.e. scalar proc?
  ## - `~` for vector like proc
  ## - formula without any of the above will be considered:
  ##   - `fkVariable` if no column involved
  ##   - `fkVector` else
  ## - `<type>: <actualFormula>`: simple type hint for tensors in closure
  ## - `<type> -> <resDtype>: <actualFormula>`: full type for closure.
  ##   `<type>` is the dtype used for tensors, `<resDtype>` the resulting type
  ## - `df[<someIdent/Sym>]`: to access columns using identifiers / symbols
  ##   defined in the scope
  ## - `idx`: can be used to access the loop iteration index
  if x.strVal == "f":
    result = compileFormula(y, isRaw = false)
  elif x.strVal == "rf":
    ## - `raw:` raw insertion of user input into closure. Still respects
    ##   the logic for above for assignment, vector or scalar
    result = compileFormula(y, isRaw = true)

macro `fn`*(x: untyped): untyped =
  let arg = if x.kind == nnkStmtList: x[0] else: x
  doAssert arg.kind in {nnkCurly, nnkTableConstr}
  result = compileFormula(arg[0], isRaw = false)


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
      result.asgn(id, toColumn( newTensorWith(df.len, idVal) ))
    elif id.len > 0:
      result.asgn(id, result[id].add toColumn( newTensorWith(df.len, idVal) ))
    var lastSize = 0
    for k in keys(df):
      if k notin result:
        # create this new column consisting of `VNull` up to current size
        if result.len > 0:
          result.asgn(k, nullColumn(result.len))
        else:
          result.asgn(k, newColumn(df[k].kind))
      # now add the current vector
      if k != id:
        # TODO: write a test for multiple bind_rows calls in a row!
        result.asgn(k, result[k].add df[k])
      lastSize = max(result[k].len, lastSize)
    result.len = lastSize
  # possibly extend vectors, which have not been filled with `VNull` (e.g. in case
  # the first `df` has a column `k` with `N` entries, but another `M` entries are added to
  # the `df`. Since `k` is not found in another `df`, it won't be extend in the loop above
  for k in keys(result):
    if result[k].len < result.len:
      # extend this by `VNull`
      result.asgn(k, result[k].add nullColumn(result.len - result[k].len))
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
  if df.isNil or df.len == 0:
    df = dfToAdd
  elif dfToAdd.len == 0:
    discard
  else:
    doAssert df.getKeys.sorted == dfToAdd.getKeys.sorted, "all keys must match to add dataframe!"
    df = bind_rows([("", df), ("", dfToAdd)])

proc hashColumn(s: var seq[Hash], c: Column, finish: static bool = false) =
  ## performs a partial hash of a DF. I.e. a single column, where
  ## the hash is added to each index in `s`. The hash is not finalized,
  ## rather the idea is to use this to hash all columns on `s` first.
  withNativeTensor(c, t):
    assert s.len == t.size
    for idx in 0 ..< t.size:
      when not finish:
        s[idx] = s[idx] !& hash(t[idx])
      else:
        s[idx] = !$(s[idx] !& hash(t[idx]))

proc buildColHashes(df: DataFrame, keys: seq[string]): seq[Hash] =
  for i, k in keys:
    if i == 0:
      result = newSeq[Hash](df.len)
    result.hashColumn(df[k])
  # finalize the hashes
  result.applyIt(!$it)

proc arrange*(df: DataFrame, by: seq[string], order = SortOrder.Ascending): DataFrame
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
  let hashes = buildColHashes(dfArranged, keys)
  #[
  Need new approach.
  Again calculate hashes of `keys` columns.
  Walk through DF.
  If hash == lastHash:
    accumulatte
  else:
    yield (seq(key, df[k][idx, Value]), slice of df)
  ]#
  proc buildClassLabel(df: DataFrame, keys: seq[string],
                       idx: int): seq[(string, Value)] =
    result = newSeq[(string, Value)](keys.len)
    for j, key in keys:
      result[j] = (key, df[key][idx, Value])

  var
    currentHash = hashes[0]
    lastHash = hashes[0]
    startIdx, stopIdx: int # indices which indicate from where to where a subgroup is located
  for i in 0 ..< dfArranged.len:
    currentHash = hashes[i]
    if currentHash == lastHash:
      # continue accumulating
      discard
    elif i > 0:
      # found the end of a subgroup or we're at the end of the DataFrame
      stopIdx = i - 1
      # return subgroup of startIdx .. stopIdx
      # build class label seq
      yield (buildClassLabel(dfArranged, keys, stopIdx), dfArranged[startIdx .. stopIdx])
      # set new start and stop idx
      startIdx = i
      lastHash = currentHash
    else:
      # should only happen for i == 0
      doAssert i == 0
      lastHash = currentHash
  # finally yield the last subgroup or the whole group, in case we only
  # have a single key
  yield (buildClassLabel(dfArranged, keys, dfArranged.high), dfArranged[startIdx .. dfArranged.high])

proc filterImpl[T](resCol: var Column, col: Column, filterIdx: Tensor[int]) =
  let t = toTensor(col, T)
  var res = newTensorUninit[T](filterIdx.size)
  if filterIdx.size > 0:
    var i = 0
    for idx in 0 ..< filterIdx.size:
      res[i] = t[filterIdx[idx]]
      inc i
  resCol = res.toColumn

proc filter(col: Column, filterIdx: Tensor[int]): Column =
  ## perform filterting of the given column `key`
  withNativeDtype(col):
    filterImpl[dtype](result, col, filterIdx)

proc countTrue(t: Tensor[bool]): int {.inline.} =
  for el in t:
    if el:
      inc result

proc filteredIdx(t: Tensor[bool]): Tensor[int] {.inline, noinit.} =
  let numNonZero = countTrue(t)
  result = newTensorUninit[int](numNonZero)
  var idx = 0
  var j = 0
  for cond in t:
    if cond:
      result[idx] = j
      inc idx
    inc j

proc filter*(df: DataFrame, conds: varargs[FormulaNode]): DataFrame =
  ## returns the data frame filtered by the conditions given
  result = newDataFrame(df.ncols)
  var fullCondition: FormulaNode
  var filterIdx = newColumn(colInt)
  for c in conds:
    if filterIdx.len > 0:
      # combine two tensors
      let newIdx = c.fnV(df)
      # `filterIdx` must be `bool`
      assert filterIdx.kind == colBool
      filterIdx.bCol.apply2_inline(newIdx.bCol):
        # calculate logic and
        x and y
    else:
      # eval boolean function on DF
      filterIdx = c.fnV(df)
  let nonZeroIdx = filteredIdx(filterIdx.bCol)
  for k in keys(df):
    result.asgn(k, df[k].filter(nonZeroIdx))
    # fill each key with the non zero elements
  result.len = nonZeroIdx.size

proc calcNewColumn*(df: DataFrame, fn: FormulaNode): (string, Column) =
  ## calculates a new column based on the `fn` given
  result = (fn.colName, fn.fnV(df))

proc calcNewConstColumnFromScalar*(df: DataFrame, fn: FormulaNode): (string, Column) =
  ## calculates a new column based on the `fn` given
  assert fn.kind == fkScalar
  result = (fn.valName, constantColumn(fn.fnS(df), df.len))

proc selectInplace*[T: string | FormulaNode](df: var DataFrame, cols: varargs[T]) =
  ## Inplace variant of `select` below.
  var toDrop = toHashSet(df.getKeys)
  for fn in cols:
    when type(T) is string:
      toDrop.excl fn
    else:
      case fn.kind
      of fkVariable: toDrop.excl fn.val.toStr
      of fkAssign:
        df.asgn(fn.lhs, df[fn.rhs])
        toDrop.excl fn.lhs
      else: doAssert false, "function does not make sense for select"
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
    case fn.kind
    of fkVariable:
      if fn.isColumn(df):
        colsToKeep.add fn.val.toStr
      else:
        # create column of value
        df.asgn($fn.val, constantColumn(fn.val, df.len))
        colsToKeep.add $fn.val
    of fkAssign:
      # essentially a rename
      df.asgn(fn.lhs, df[fn.rhs.toStr])
      # colToKeep only relevant for `transmute`, where we only want to keep
      # the LHS
      colsToKeep.add fn.lhs
    of fkVector:
      let (colName, newCol) = df.calcNewColumn(fn)
      df.asgn(colName, newCol)
      colsToKeep.add colName
    of fkScalar:
      let (colName, newCol) = df.calcNewConstColumnFromScalar(fn)
      df.asgn(colName, newCol)
      colsToKeep.add colName
  when dropCols:
    df.selectInplace(colsToKeep)

proc mutateInplace*(df: var DataFrame, fns: varargs[FormulaNode]) =
  ## Inplace variasnt of `mutate` below.
  case df.kind
  of dfGrouped:
    var res = newDataFrame()
    for (tup, subDf) in groups(df):
      var mdf = subDf
      mdf.mutateImpl(fns, dropCols = false)
      res.add mdf
    df = res
  else:
    df.mutateImpl(fns, dropCols = false)

proc mutate*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## Returns the data frame with an additional mutated column, described
  ## by the functions `fns`.
  ## Each formula `fn` given will be used to create a new column in the
  ## dataframe.
  ## We assume that the LHS of the formula corresponds to a fkVariable
  ## that's used to designate the new name.
  ## NOTE: If a given `fn` is a term (`fk`) without an assignment
  ## (using `~`, kind `amDep`) or a function (`fk`), the resulting
  ## column will be named after the stringification of the formula.
  ##
  ## E.g.: `df.mutate(f{"x" * 2})` will add the column `(* x 2)`.
  result = df
  result.mutateInplace(fns)

proc transmuteInplace*(df: var DataFrame, fns: varargs[FormulaNode]) =
  ## Inplace variant of `transmute` below.
  case df.kind
  of dfGrouped:
    var res = newDataFrame()
    for (tup, subDf) in groups(df):
      var mdf = subDf
      mdf.mutateImpl(fns, dropCols = true)
      res.add mdf
    df = res
  else:
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
    doAssert fn.kind == fkAssign
    result[fn.lhs] = df[fn.rhs.toStr]
    # remove the column of the old name
    result.drop(fn.rhs.toStr)

proc arrangeSortImpl[T](toSort: var seq[(int, T)], order: SortOrder) =
  ## sorts the given `(index, Value)` pair according to the `Value`
  toSort.sort(
      cmp = (
        proc(x, y: (int, T)): int =
          result = system.cmp(x[1], y[1])
      ),
      order = order
    )

proc sortBySubset(df: DataFrame, by: string, idx: seq[int], order: SortOrder): seq[int] =
  withNativeDtype(df[by]):
    var res = newSeq[(int, dtype)](idx.len)
    let t = toTensor(df[by], dtype)
    for i, val in idx:
      res[i] = (val, t[val])
    res.arrangeSortImpl(order = order)
    # after sorting here, check duplicate values of `val`, slice
    # of those duplicates, use the next `by` in line and sort
    # the remaining indices. Recursively do this until
    result = res.mapIt(it[0])

proc sortRecurse(df: DataFrame, by: seq[string],
                 startIdx: int,
                 resIdx: seq[int],
                 order: SortOrder): seq[int]

proc sortRecurseImpl[T](result: var seq[int], df: DataFrame, by: seq[string],
                        startIdx: int,
                        resIdx: seq[int],
                        order: SortOrder) =
  var res = newSeq[(int, T)](result.len)
  let t = toTensor(df[by[0]], T)
  for i, val in result:
    res[i] = (val, t[val])

  ## The logic in the following is a bit easy to misunderstand. Here we are
  ## sorting the current key `by[0]` (its data is in `res`) by any additional
  ## keys `by[1 .. ^1]`. It is important to keep in mind that `res` (key `by[0]`)
  ## is already sorted in the proc calling `sortRecurse`.
  ## Then we walk over the sorted data and any time a value of `res` changes,
  ## we have to look at that whole slice and sort it by the second key `by[1]`.
  ## Thus, the while loop below checks for:
  ## - `last != cur`: val changed at index i, need to sort, iff the last search
  ##   was ``not`` done at index `i - 1` (that happens immediately the iteration
  ##   after sorting a slice -> `i > lastSearch + 1`.
  ## - `i == df.high`: In the case of the last element we do ``not`` require
  ##   the value to change, ``but`` here we have to sort not the slice until
  ##   `i - 1` (val changed at current `i`, only want to sort same slice!),
  ##   but until `df.high` -> let topIdx = …
  ## Finally, if there are more keys in `by`, sort the subset itself as subsets.
  let mby = by[1 .. ^1]
  var
    last = res[0][1]
    cur = res[1][1]
    i = startIdx
    lastSearch = 0
  while i < res.len:
    cur = res[i][1]
    if last != cur or i == df.high:
      if i > lastSearch + 1:
        # sort between `lastSearch` and `i`.
        let topIdx = if i == df.high: i else: i - 1
        var subset = sortBySubset(df, mby[0],
                                  res[lastSearch .. topIdx].mapIt(it[0]),
                                  order = order)
        if mby.len > 1:
          # recurse again
          subset = sortRecurse(df, mby, lastSearch,
                               resIdx = subset,
                               order = order)
        result[lastSearch .. topIdx] = subset
      lastSearch = i
    last = res[i][1]
    inc i

proc sortRecurse(df: DataFrame, by: seq[string],
                 startIdx: int,
                 resIdx: seq[int],
                 order: SortOrder): seq[int] =
  result = resIdx
  withNativeDtype(df[by[0]]):
    sortRecurseImpl[dtype](result, df, by, startIdx, resIdx, order)

proc sortBys(df: DataFrame, by: seq[string], order: SortOrder): seq[int] =
  withNativeDtype(df[by[0]]):
    var res = newSeq[(int, dtype)](df.len)
    var idx = 0
    let t = toTensor(df[by[0]], dtype)
    for i in 0 ..< t.size:
      let val = t[i]
      res[idx] = (idx, val)
      inc idx
    res.arrangeSortImpl(order = order)
    # after sorting here, check duplicate values of `val`, slice
    # of those
    # duplicates, use the next `by` in line and sort
    # the remaining indices. Recursively do this until
    var resIdx = res.mapIt(it[0])
    if res.len > 1 and by.len > 1:
      resIdx = sortRecurse(df, by, startIdx = 1, resIdx = resIdx, order = order)
    result = resIdx

proc arrange*(df: DataFrame, by: seq[string], order = SortOrder.Ascending): DataFrame =
  ## sorts the data frame in ascending / descending `order` by key `by`
  # now sort by cols in ascending order of each col, i.e. ties will be broken
  # in ascending order of the columns
  result = newDataFrame(df.ncols)
  let idxCol = sortBys(df, by, order = order)
  result.len = df.len
  var data = newColumn()
  for k in keys(df):
    withNativeDtype(df[k]):
      let col = df[k].toTensor(dtype)
      var res = newTensor[dtype](df.len)
      for i in 0 ..< df.len:
        res[i] = col[idxCol[i]]
      data = toColumn res
    result.asgn(k, data)

proc arrange*(df: DataFrame, by: string, order = SortOrder.Ascending): DataFrame =
  result = df.arrange(@[by], order)

#proc `[]=`*[T](df: var DataFrame, key: string, idx: int, val: T) =
#  ## assign `val` to column `c` at index `idx`
#  ## If the types match, it just calls `[]=` on the tensor.
#  ## If they are compatible, `val` is converted to c's type.
#  ## If they are incompatible, `c` will be rewritten to an object
#  ## column.
#  var rewriteAsValue = false
#  case df[key].kind
#  of colFloat:
#    when T is float:
#      df[key].fCol[idx] = val
#    elif T is SomeNumber:
#      df[key].fCol[idx] = val.float
#  of colInt:
#    when T is int:
#      df[key].iCol[idx] = val
#    else:
#      rewriteAsValue = true
#  of colString:
#    when T is string:
#      df[key].sCol[idx] = val
#    else:
#      rewriteAsValue = true
#  of colBool:
#    when T is bool:
#      df[key].bCol[idx] = val
#    else:
#      rewriteAsValue = true
#  of colObject:
#    df[key].oCol[idx] = %~ val
#  if rewriteAsValue:
#    # rewrite as an object column
#    df = df[key].toObjectColumn()
#    df[key].oCol[idx] = %~ val

proc assign*(df: var DataFrame, key: string, idx1: int, c2: Column, idx2: int) =
  ## checks if the value in `c1` at `idx1` is equal to the
  ## value in `c2` at `idx2`
  withNativeDtype(df[key]):
    df[key, idx1] = c2[idx2, dtype]

proc innerJoin*(df1, df2: DataFrame, by: string): DataFrame =
  ## returns a data frame joined by the given key `by` in such a way as to only keep
  ## rows found in both data frames
  # build sets from both columns and seqs of their corresponding indices
  let
    df1S = df1.arrange(by)
    df2S = df2.arrange(by)
  withNativeDtype(df1S[by]):
    let
      col1 = df1S[by].toTensor(dtype).toRawSeq
      col2 = df2S[by].toTensor(dtype).toRawSeq
    let colSet1 = col1.toHashSet
    let colSet2 = col2.toHashSet
    let intersection = colSet1 * colSet2
    let idxDf1 = toSeq(0 ..< col1.len).filterIt(col1[it] in intersection)
    let idxDf2 = toSeq(0 ..< col2.len).filterIt(col2[it] in intersection)

    var
      i = 0
      j = 0
    let
      # for some reason we can't do toSeq(keys(df1S)) anymore...
      # This is due to https://github.com/nim-lang/Nim/issues/7322. `toSeq` isn't exported for now.
      keys1 = getKeys(df1S).toHashSet
      keys2 = getKeys(df2S).toHashSet
      allKeys = keys1 + keys2
      commonKeys = keys1 * keys2
      restKeys = allKeys - commonKeys
    result = newDataFrame(allKeys.card)
    let resLen = (max(df1S.len, df2S.len))
    for k in allKeys:
      if k in df1S and k in df2S:
        doAssert compatibleColumns(df1S[k], df2S[k]), " Key: " & $k & ", df1: " & $df1S[k].kind & ", df2: " & $df2S[k].kind
        result.asgn(k, newColumn(kind = combinedColKind(@[df1S[k].kind, df2S[k].kind]),
                                 length = resLen))
      elif k in df1S and k notin df2S:
        result.asgn(k, newColumn(kind = df1S[k].kind, length = resLen))
      if k notin df1S and k in df2S:
        result.asgn(k, newColumn(kind = df2S[k].kind, length = resLen))
    var count = 0

    let df1By = df1S[by].toTensor(dtype)
    let df2By = df2S[by].toTensor(dtype)
    while i < idxDf1.len and
          j < idxDf2.len:
      let il = idxDf1[i]
      let jl = idxDf2[j]
      # indices point to same row, merge row
      if df1By[il] == df2By[jl]:
        for k in commonKeys:
          if not equal(df1S[k], il, df2S[k], jl):
            # skip this element
            break
          result.assign(k, count, df1S[k], il)
        for k in restKeys:
          if k in keys1:
            result.assign(k, count, df1S[k], il)
          elif k in keys2:
            result.assign(k, count, df2S[k], jl)
        inc count
      # now increase the indices as required
      if i != idxDf1.high and
         j != idxDf2.high and
         (df1By[idxDf1[i+1]] == df2By[idxDf2[j+1]]):
        inc i
        inc j
      elif i != idxDf1.high and (df1By[idxDf1[i+1]] == df2By[jl]):
        inc i
      elif j != idxDf2.high and (df1By[il] == df2By[idxDf2[j+1]]):
        inc j
      elif i == idxDf1.high and j == idxDf2.high:
        break
      else:
        raise newException(Exception, "This should not happen")
    result.len = count
    # possibly shorten the columns
    if result.len < resLen:
      for k in getKeys(result):
        withNativeTensor(result[k], t):
          result.asgn(k, toColumn(t[_ ..< result.len]))
        result[k].len = result.len

proc toHashSet*[T](t: Tensor[T]): HashSet[T] =
  for el in t:
    result.incl el

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
    result = newDataFrame(df.ncols, kind = dfGrouped)
    result.data = df.data
    result.len = df.len
  for key in by:
    result.groupMap[key] = toHashSet(result[key].toTensor(Value))

proc summarize*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## returns a data frame with the summaries applied given by `fn`. They
  ## are applied in the order in which they are given
  result = newDataFrame(kind = dfNormal)
  var lhsName = ""
  case df.kind
  of dfNormal:
    for fn in fns:
      doAssert fn.kind == fkScalar
      lhsName = fn.valName
      # just apply the function
      withNativeConversion(fn.valKind, get):
        let res = toColumn get(fn.fnS(df))
        result.asgn(lhsName, res)
        result.len = res.len
  of dfGrouped:
    # since `df.len >> fns.len = result.len` the overhead of storing the result
    # in a `Value` first does not matter in practice
    var sumStats = initOrderedTable[string, seq[Value]]()
    var keys = initOrderedTable[string, seq[Value]](df.groupMap.len)
    var idx = 0
    var keyLabelsAdded = false
    for fn in fns:
      doAssert fn.kind == fkScalar
      lhsName = fn.valName
      sumStats[lhsName] = newSeqOfCap[Value](1000) # just start with decent size
      for class, subdf in groups(df):
        if not keyLabelsAdded:
          # keys and labels only have to be added for a single `fn`, since the DF
          # will yield the same subgroups anyways!
          # TODO: we're gonna replace this anyways, but we shouldn't iterate over groups
          # several times for several functions!
          for (key, label) in class:
            if key notin keys: keys[key] = newSeqOfCap[Value](1000)
            keys[key].add label
        sumStats[lhsName].add fn.fnS(subDf)
      # done w/ one subgroup, don't add more keys / labels
      keyLabelsAdded = true
    for k, vals in keys:
      result.asgn(k, toNativeColumn vals)
    for k, vals in sumStats:
      result.asgn(k, toNativeColumn vals)
      result.len = vals.len

proc count*(df: DataFrame, col: string, name = "n"): DataFrame =
  ## counts the number of elements per type in `col` of the data frame.
  ## Basically a shorthand for df.group_by.summarize(f{length(col)}).
  ## TODO: handle already grouped dataframes.
  result = DataFrame()
  let grouped = df.group_by(col, add = true)
  var counts = newSeqOfCap[int](1000) # just start with decent size
  var keys = initOrderedTable[string, seq[Value]](grouped.groupMap.len)
  var idx = 0
  for class, subdf in groups(grouped):
    for (c, val) in class:
      if c notin keys: keys[c] = newSeqOfCap[Value](1000)
      keys[c].add val
    counts.add subDf.len
    inc idx
  for k, vals in keys:
    result.asgn(k, toNativeColumn vals)
  result.asgn(name, toColumn counts)
  result.len = idx

proc setDiff*(df1, df2: DataFrame, symmetric = false): DataFrame =
  ## returns a `DataFrame` with all elements in `df1` that are not found in
  ## `df2`. If `symmetric` is true, the symmetric difference of the dataset is
  ## returned, i.e. elements which are either not in `df1` ``or`` not in `df2`.
  result = newDataFrame(df1.ncols)
  #[
  Calculate custom hash for each row in each table.
  Keep var h1, h2 = seq[Hashes] where seq[Hashes] is hash of of row.
  Calculate hashes by column! Get df1 column 1, start hash, column 2, add to hash etc.
  Same for df2.
  Compare hashes either symmetric, or asymmetric.
  Use indices of allowed hashes to rebuild final DF via columns again. Should be fast
  ]#
  if getKeys(df1) != getKeys(df2):
    # if not all keys same, all rows different by definition!
    return df1

  let keys = getKeys(df1)
  let h1 = buildColHashes(df1, keys)
  let h2 = buildColHashes(df2, keys)
  # given hashes apply set difference
  var diff: HashSet[Hash]
  if symmetric:
    diff = symmetricDifference(toHashSet(h1), toHashSet(h2))
    var idxToKeep1 = newSeqOfCap[int](diff.card)
    var idxToKeep2 = newSeqOfCap[int](diff.card)
    for idx, h in h1:
      if h in diff:
        # keep this row
        idxToKeep1.add idx
    for idx, h in h2:
      if h in diff:
        # keep this row
        idxToKeep2.add idx
    # rebuild those from df1, then those from idx2
    for k in keys:
      result.asgn(k, df1[k].filter(toTensor(idxToKeep1)))
      # fill each key with the non zero elements
    result.len = idxToKeep1.len
    var df2Res = newDataFrame()
    for k in keys:
      df2Res.asgn(k, df2[k].filter(toTensor(idxToKeep2)))
      # fill each key with the non zero elements
    df2Res.len = idxToKeep2.len
    # stack the two data frames
    result.add df2Res
  else:
    diff = toHashSet(h1) - toHashSet(h2)
    # easy
    var idxToKeep = newTensor[int](diff.card)
    var i = 0
    for idx, h in h1:
      if h in diff:
        # keep this row
        idxToKeep[i] = idx
        inc i
    # rebuild the idxToKeep columns
    for k in keys:
      result.asgn(k, df1[k].filter(idxToKeep))
      # fill each key with the non zero elements
    result.len = idxToKeep.size

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
  result = newDataFrame(df.ncols)
  let remainCols = getKeys(df).toHashSet.difference(cols.toHashSet)
  let newLen = cols.len * df.len
  # assert all columns same type
  # TODO: relax this restriction, auto convert to `colObject` if non matching
  var keyTensor = newTensorUninit[string](newLen)
  withCombinedType(df, @cols):
    var valTensor = newTensorUninit[dtype](newLen)
    for i in 0 ..< cols.len:
      # for each column, clone the `col` tensor once to the correct position
      let col = cols[i]
      keyTensor[i * df.len ..< (i + 1) * df.len] = col #.clone()
      # TODO: make sure we don't have to clone the given tensor!
      valTensor[i * df.len ..< (i + 1) * df.len] = df[col].toTensor(dtype)
    # now create result
    result.asgn(key, toColumn keyTensor)
    result.asgn(value, toColumn valTensor)
  # For remainder of columns, just use something like `repeat`!, `stack`, `concat`
  for rem in remainCols:
    withNativeDtype(df[rem]):
      let col = df[rem].toTensor(dtype)
      var fullCol = newTensorUninit[dtype](newLen)
      for i in 0 ..< cols.len:
        # for each column, clone the `col` tensor once to the correct position
        fullCol[i * df.len ..< (i + 1) * df.len] = col #.clone()
      result[rem] = toColumn(fullCol)
  result.len = newLen


proc unique*(c: Column): Column =
  ## returns a seq of all unique values in `v`
  var hashes = newSeq[Hash](c.len)
  hashes.hashColumn(c, finish = true)
  # finalize the hashes
  var hSet = toHashSet(hashes)
  var idxToKeep = newTensor[int](hSet.card)
  var idx = 0
  for i in 0 ..< c.len:
    if hashes[i] in hSet:
      idxToKeep[idx] = i
      # remove from set to not get duplicates!
      hSet.excl hashes[i]
      inc idx
  # apply idxToKeep as filter
  result = c.filter(idxToKeep)
  result.len = idxToKeep.size

proc unique*(df: DataFrame, cols: varargs[string],
             keepAll = true): DataFrame =
  ## returns a DF with only distinct rows. If one or more `cols` are given
  ## the uniqueness of a row is only determined based on those columns. By
  ## default all columns are considered.
  ##
  ## If not all columns are considered and `keepAll` is true the resulting
  ## DF contains all other columns. Of those the first duplicated row
  ## is kept!
  ## NOTE: The corresponding `dplyr` function is `distinct`. The choice for
  ## `unique` was made, since `distinct` is a keyword in Nim!
  result = newDataFrame(df.ncols)
  var mcols = @cols
  if mcols.len == 0:
    mcols = getKeys(df)
  let hashes = buildColHashes(df, mcols)
  var hSet = toHashSet(hashes)
  # walk df, build indices from `hashes` which differ
  var idxToKeep = newTensor[int](hSet.card)
  var idx = 0
  for i in 0 ..< df.len:
    if hashes[i] in hSet:
      idxToKeep[idx] = i
      # remove from set to not get duplicates!
      hSet.excl hashes[i]
      inc idx
  # apply idxToKeep as filter
  let resCols = if keepAll: getKeys(df) else: mcols
  for k in resCols:
    result.asgn(k, df[k].filter(idxToKeep))
    # fill each key with the non zero elements
  result.len = idxToKeep.size

proc drop_null*(df: DataFrame, cols: varargs[string],
                convertColumnKind = false,
                failIfConversionFails: bool = false): DataFrame =
  ## returns a DF with only those rows left, which contain no null values.
  ## By default this includes all columns in the data frame. If one or more
  ## args are given, only those columns will be considered.
  ##
  ## By default no attempt is made to convert the new columns to a unified
  ## data type, since it introduces another walk over the data. If `convertColumnKind`
  ## is true, conversion is attempted. Whether that throws an assertion error
  ## if the conversion is not possible to a single native type is controlled
  ## by the static `failIfConversionFails`.
  ##
  ## Note that in general this is not a particularly fast proc, since each column
  ## which should drop null values causes a filter of the DF, i.e. a full run over
  ## the lenght of the DF.
  # NOTE: `failIfConversionFails` can't be a static bool right now, because that
  # results in a weird overload resolution bug in the `filter` line below
  # TODO: we could use `column.toTensor` / `column.valueTo` with the `dropNull`
  # argument too. Unify? :/ Which way though?
  var mcols = @cols
  if mcols.len == 0:
    mcols = getKeys(df)
  var colsNeedPruning = newSeq[string]()
  for col in mcols:
    if df[col].kind == colObject: # cols which aren't object cannot contain null
      colsNeedPruning.add col
  # now have to check all those cols for null, advantage: all cols use Value
  # -> can read all
  result = df
  for col in colsNeedPruning:
    ## TODO: avoid filtering several times somehow?
    ## can read all cols first and then iterate over them? Not necessarily faster
    let localCol = col # ref: https://github.com/nim-lang/Nim/pull/14447
    result = result.filter(f{Value: isNull(df[localCol][idx]).toBool == false})
    if convertColumnKind:
      if failIfConversionFails: # ugly workaround
        result[col] = result[col].toNativeColumn(failIfImpossible = true)
      else:
        result[col] = result[col].toNativeColumn(failIfImpossible = false)

func evaluate*(node: FormulaNode): Value =
  ## tries to return a single `Value` from a FormulaNode.
  ## Works either if formula is `fkNone` or `fkVariable`.
  ## Raises for `fkVector` and `fkScalar`.
  case node.kind
  of fkVariable: result = node.val
  of fkAssign: result = node.rhs # ?? TODO: should this be allowed?
  of fkScalar: result = %~ node.valName
  of fkVector: result = %~ node.colName

proc evaluate*(node: FormulaNode, df: DataFrame): Column =
  ## tries to return a Column from a FormulaNode with an input
  ## DataFrame `df`.
  ## Works either if formula is `fkNone` or `fkVariable`.
  ## Raises for `fkVector` and `fkScalar`.
  # TODO: Handle cases if a value is not a column!
  case node.kind
  of fkVariable:
    if node.isColumn(df):
      result = df[node.val.toStr]
    else:
      # create constant column
      result = constantColumn(node.val, df.len)
  of fkAssign: result = df[node.rhs.toStr]
  of fkVector: result = node.fnV(df)
  of fkScalar: result = constantColumn(node.fnS(df), df.len)

proc reduce*(node: FormulaNode, df: DataFrame): Value =
  ## tries to return a Column from a FormulaNode with an input
  ## DataFrame `df`.
  ## Works either if formula is `fkNone` or `fkVariable`.
  ## Raises for `fkVector` and `fkScalar`.
  # TODO: Handle cases if a value is not a column!
  case node.kind
  of fkScalar:
    result = node.fnS(df)
  else:
    raise newException(ValueError, "Cannot reduce a data frame using a formula " &
      "of kind " & $node.kind & "!")
