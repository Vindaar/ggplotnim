#import ggplotnim

import macros, tables, strutils, options, fenv, sets, hashes, sugar, math

import sequtils, stats, strformat, algorithm, parseutils

# for error messages to print types
import typetraits

from ginger import Scale

import arraymancer except io_csv
export arraymancer

#import df_io
#export df_io

import value
export value

import column
export column

import formulaClosure
export formulaClosure

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
    data*: Table[string, Column]
    case kind: DataFrameKind
    of dfGrouped:
      # a grouped data frame stores the keys of the groups and maps them to
      # a set of the categories
      groupMap: OrderedTable[string, HashSet[Value]]
    else: discard

const ValueNull* = Value(kind: VNull)

proc newDataFrame*(size = 8,
                    kind = dfNormal): DataFrame =
  ## initialize a DataFrame, which initializes the table for `size` number
  ## of columns. Given size will be rounded up to the next power of 2!
  result = DataFrame(kind: kind,
                     data: initTable[string, Column](nextPowerOfTwo(size)),
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

proc `[]`*(df: DataFrame, k: string): Column {.inline.} =
  result = df.data[k]

proc `[]`*(df: DataFrame, k: Value): Column {.inline.} =
  result = df.data[k.toStr]

func isColumn*(fn: FormulaNode, df: DataFrame): bool =
  result = $fn in df

proc `[]`*(df: DataFrame, k: string, idx: int): Value {.inline.} =
  ## returns the element at index `idx` in column `k` directly, without
  ## returning the whole vector first
  result = df.data[k][idx, Value]

proc `[]`*[T](df: DataFrame, k: string, idx: int, dtype: typedesc[T]): T {.inline.} =
  ## returns the element at index `idx` in column `k` directly, without
  ## returning the whole vector first
  result = df.data[k][idx, dtype]

proc `[]`*[T](df: DataFrame, k: string, slice: Slice[int], dtype: typedesc[T]): Tensor[T] {.inline.} =
  ## returns the elements in `slice` in column `k` directly, without
  ## returning the whole vector first as a tensor of type `dtype`
  result = df.data[k][slice.a .. slice.b, dtype]

proc `[]`*(df: DataFrame, k: string, slice: Slice[int]): Column {.inline.} =
  ## returns the elements in `slice` in column `k` directly, without
  ## returning the whole vector first
  result = df.data[k][slice.a .. slice.b]

proc `[]=`*(df: var DataFrame, k: string, col: Column) {.inline.} =
  ## Assigns a full column to the DF. In debug mode it checks that the size of
  ## the input column matches the DF size, unless the DF is empty.
  df.data[k] = col
  assert df.len == col.len or df.len == 0
  df.len = col.len

proc asgn(df: var DataFrame, k: string, col: Column) {.inline.} =
  # low level assign, which does not care about sizes of column. Used in `toTab`.
  # Shorter columns are extended afterwards.
  df.data[k] = col

proc clone*(df: DataFrame): DataFrame =
  ## returns a cloned version of `df` so that the tensors don't share
  ## data.
  # NOTE: This should actually just use `clone` on each tensor, but if
  # we do that, we get random GC segfaults later
  result = DataFrame(kind: df.kind)
  result.len = df.len
  result.data = deepCopy(df.data)
  case df.kind
  of dfGrouped:
    result.groupMap = df.groupMap
  else: discard

proc `[]=`*[T: Tensor | seq | array](df: var DataFrame, k: string, t: T) {.inline.} =
  df.data[k] = toColumn t

proc `[]=`*[T](df: var DataFrame, k: string, idx: int, val: T) {.inline.} =
  ## WARNING: only use this if you know that `T` is the correct data type!
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
    echo "Dataframe with ", df.getKeys.len, " columns and ", df.len, " rows:"
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
                col = data.toColumn
              except ValueError:
                try:
                  data[i] = %~ x.parseFloat
                  col = data.toColumn
                except ValueError:
                  data[i] = %~ x
                  col = data.toColumn
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
              col = data.toColumn
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
  ## the `seq[Value]` to a `PersistentVector[Value]` and add to DF.
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
      if found.kind != nnkNilLIt:
        result = found

proc replaceColumns(body: NimNode, idents: var seq[NimNode],
                    fkKind: FormulaKind): NimNode =
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
    of nnkAccQuoted, nnkCallStrLit:
      case fkKind
      of fkVector:
        let idIdx = ident"idx"
        result[i] = nnkBracketExpr.newTree(idents.pop,
                                           idIdx)
        #nnkCall.newTree(ident"get", ident"df", body[i])
      of fkScalar:
        # just the full column
        # TODO: change such that we determine which ident needs to be
        # scalar treated and which vector, based on determining
        # the formula kind the way we do in default backend!
        result[i] = idents.pop
      of fkVariable:
        result[i] = result[i][0].toStrLit
        error("fix me")
      else: discard
    of nnkBracketExpr:
      if eqIdent(result[i][0], ident"df"):
        # replace whole node
        let idIdx = ident"idx"
        result[i] = nnkBracketExpr.newTree(idents.pop,
                                           idIdx)
      else:
        result[i] = replaceColumns(body[i], idents, fkKind = fkKind)
    else:
      result[i] = replaceColumns(body[i], idents, fkKind = fkKind)

proc unwrapAccQuote(n: NimNode): NimNode =
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
  else: result = n

proc collectColumns(body: NimNode): seq[NimNode] =
  result = newSeq[NimNode]()
  for i in 0 ..< body.len:
    case body[i].kind
    of nnkAccQuoted, nnkCallStrLit:
      result.add unwrapAccQuote(body[i])
    #of nnkStrLit:
    #  result.add body[i]
    #of nnkIdent:
    #  echo "IDENT ", body[i].repr
    of nnkBracketExpr:
      if eqIdent(body[i][0], ident"df"):
        # refers to a column
        result.add body[i][1]
      else:
        result.add collectColumns(body[i])
    else:
      result.add collectColumns(body[i])

proc genIdentsFromColumns(columns: seq[NimNode]): seq[NimNode] =
  result = newSeq[NimNode]()
  var cols = initTable[string, string]()
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
                          isRaw: bool): NimNode =
  let columns = collectColumns(body)
  var idents = genIdentsFromColumns(columns)
  let resSym = genSym(nskVar, ident = "res")
  let colDefs = genColDefsFromIdents(idents, columns, fkVector,
                                     resSym,
                                     dtype, resDtype)
  # reverse the idents, since we use `pop`
  idents.reverse()
  let forLoopBody = replaceColumns(body, idents, fkKind = fkVector)
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
                          isRaw: bool): NimNode =
  let columns = collectColumns(body)
  var idents = genIdentsFromColumns(columns)
  let resSym = genSym(nskVar, ident = "res")
  let colDefs = genColDefsFromIdents(idents, columns, fkScalar,
                                     resSym,
                                     dtype, resDtype)
  # reverse the idents, since we use `pop`
  idents.reverse()
  let scalarBody = replaceColumns(body, idents, fkKind = fkScalar)
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
  doAssert name.kind != nnkNilLit, " there must be a LHS for fkScalar!"
  result = quote do:
    FormulaNode(name: `rawName`,
                valName: `name`, kind: fkScalar,
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
  echo body.treerepr
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

proc extractIdents(n: NimNode): seq[NimNode] =
  case n.kind
  of nnkIdent:
    result.add n
  else:
    for i in 0 ..< n.len:
      result.add extractIdents(n[i])

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
    const floatSet = toSet(@["+", "-", "*", "/", "mod"])
    const stringSet = toSet(@["&", "$"])
    const boolSet = toSet(@["and", "or", "xor", ">", "<", ">=", "<=", "==", "!=",
                            "true", "false", "in", "notin"])
    let (isFloat, isString, isBool) = checkDtype(body, floatSet, stringSet, boolSet)
    debugecho "AUTO IS FLOAT ", isFloat
    debugecho "AUTO IS string ", isstring
    debugecho "AUTO IS bool ", isbool
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
      discard
      #error("Could not determine data types of tensors in formula:\n" &
      #  "  name: " & $name & "\n" &
      #  "  formula: " & $body.repr & "\n" &
      #  "  data type: " & $result[0].repr & "\n" &
      #  "  output data type: " & $result[1].repr)


    # finally determine the FormulaKind
    # TODO: for now we just assume that raw string literals are supposed
    # to refer to columns. We will provide a different macro entry to
    # explicitly refer to non DF related formulas
    # TODO2: we might want to consider
    result[2] = determineFormulaKind(body)

proc isValidFunc(fn: NimNode): bool =
  ## Checks if the given `fn` sym node represents a valid function
  ## of either `VectorValuedFunc` or `ScalarValuedFunc`.
  # TODO: this is essentialy the IMPL of `getFuncKind` too!!!
  case fn.kind
  of nnkClosedSymChoice, nnkOpenSymChoice:
    # if a generic, check if there exists a valid choice
    for ch in fn:
      if isValidFunc(ch):
        #result = ch
        return true
  else:
    let impl = fn.getTypeImpl
    result = false
    case impl.kind
    of nnkProcTy:
      let argType = impl[0][1][1]
      #let resType = impl[0][1][1]
      #echo impl.treeRepr
      #echo argType.repr
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

proc extractTypes(idents: NimNode): seq[(NimNode, NimNode)] =
  ## Checks if the given `fn` sym node represents a valid function
  ## of either `VectorValuedFunc` or `ScalarValuedFunc`.
  # TODO: this is essentialy the IMPL of `getFuncKind` too!!!
  expectKind idents, nnkBracket
  for id in idents:
    case id.kind
    of nnkIdent: discard
    else:
      let impl = id.getTypeImpl

macro cpTyped(name, body: untyped,
              idents: typed,
              typeHints: typed): untyped =
  echo name.treerepr
  echo body.treerepr
  #echo fns.treeRepr
  echo idents.treeRepr
  echo "\n\n\n\n"
  #let fnKind = isValidFunc(fns)
  let typeNodeTuples = extractTypes(idents)
  #echo "fn kind ", fnKind
  result = quote do:
    FormulaNode(kind: fkVariable, name: "test")
  echo "res cpTyped ", result.repr

proc compileFormulaTypedMacro(body: NimNode,
                              typeHints: tuple[dtype, resDtype: NimNode],
                              name: NimNode): NimNode =
  # essentially:
  # extract every ident
  # for every ident hand it to the typed macro as a sequence.
  # Resolve each ident.
  # If symbol points to a variable, get type:
  # - If scalar, insert directly into body
  #   - if string, assume means a column. Introduce fallback for
  #     real strings? `s( stringIdent )` for example?
  #   - if float, leave as is
  # - If vector, assume same number as idx as DF, iterate over it
  let columns = collectColumns(body)
  var idents = genIdentsFromColumns(columns)
  # reverse the idents, since we use `pop`
  idents.reverse()
  let scalarBody = replaceColumns(body, idents, fkKind = fkVariable)
  echo scalarBody.treeRepr
  #let fnss = constrFormula(scalarBody)
  let fnss = extractIdents(scalarBody)
  result = quote do:
    cpTyped(`name`, `scalarBody`,
            `fnss`,
            `typeHints`)

proc compileFormulaImpl(rawName, name, body: NimNode,
                        isAssignment: bool,
                        isVector: bool,
                        isReduce: bool,
                        isRaw: bool,
                        typeHints: tuple[dtype, resDtype: NimNode]): NimNode =
  echo "IMPL, name: ", name.repr, " body: ", body.repr
  var (dtype, resDtype, funcKind) = determineFuncKind(body, typeHints = typeHints,
                                                      name = name)
  # force `fkVariable` if this is an `<-` assignment
  echo resDtype.repr
  echo dtype.repr
  echo "FUNCKIND ", funcKind
  echo "isAssignment ", isAssignment
  echo "isVector ", isVector
  echo "isReduce ", isReduce
  funcKind = if isAssignment: fkAssign
             elif isReduce: fkScalar
             elif isVector: fkVector
             else: funcKind

  if false: #dtype.kind == nnkNilLit and resDtype.kind == nnkNilLit:
    result = compileFormulaTypedMacro(body, typeHints = typeHints,
                                      name = name)
  else:
    case funcKind
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
      result = compileVectorFormula(rawName, name, body, dtype, resDtype, isRaw = isRaw)
    of fkScalar:
      result = compileScalarFormula(rawName, name, body, dtype, resDtype, isRaw = isRaw)

    #var mhs = @[ident"hwy"]
    #let fns = constrFormula(replaceColumns(body, mhs, fkKind = fkNone))
    #result = quote do:
    #  block:
    #    `fns`
    #    `result`

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
    elif node.len > 0 and  eqIdent(node[0], ident"<<"):
      # this is an assignment `w/o` access of DF column
      formulaName = unwrapAccQuote(node[1])
      #else: error("Unsupported LHS of type " & $node[1].kind & " : " & $node[1].repr)
      formulaRhs = node[2]
      isReduce = true
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
  result = compileFormulaImpl(rawName, formulaName, formulaRhs,
                              isAssignment = isAssignment,
                              isVector = isVector,
                              isReduce = isReduce,
                              isRaw = isRaw,
                              typeHints = typeHints)
  echo result.repr

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
  expectKind arg, nnkCurly
  result = compileFormula(arg[0], isRaw = false)

#iterator pairs*(df: DataFrame): (int, Value) =
#  # returns each row of the dataframe as a Value of kind VObject
#  for i in 0 ..< df.len:
#    yield (i, df.row(i))
#
#proc toSeq(v: PersistentVector[Value]): seq[Value] =
#  result = v[0 ..< v.len]
#
#proc toSeq(df: DataFrame, key: string): seq[Value] =
#  result = df[key].toSeq
#
#proc vToSeq*(v: PersistentVector[Value]): seq[Value] = toSeq(v)
#proc vToSeq*(df: DataFrame, key: string): seq[Value] = toSeq(df, key)
#
#proc toFloat*(s: string): float =
#  # TODO: replace by `toFloat(v: Value)`!
#  result = s.parseFloat
#
#proc nearlyEqual(x, y: float, eps = 1e-10): bool =
#  ## equality check for floats which tries to work around floating point
#  ## errors
#  ## Taken from: https://floating-point-gui.de/errors/comparison/
#  let absX = abs(x)
#  let absY = abs(y)
#  let diff = abs(x - y)
#  if x == y:
#    # shortcut, handles infinities
#    result = true
#  elif x == 0 or
#       y == 0 or
#       diff < minimumPositiveValue(system.float):
#    # a or b is zero or both are extremely close to it
#    # relative error is less meaningful here
#    result =  diff < (eps * minimumPositiveValue(system.float))
#  else:
#    # use relative error
#    result = diff / min((absX + absY), maximumPositiveValue(system.float)) < eps
#
#proc isValidVal(v: Value, f: FormulaNode): bool =
#  doAssert v.kind != VObject
#  doAssert f.kind == fkTerm
#  doAssert f.op in {amEqual, amGreater, amLess, amGeq, amLeq, amAnd, amOr, amXor}
#  case v.kind
#  of VInt, VFloat:
#    case f.op
#    of amEqual:
#      result = v.toFloat.nearlyEqual(f.rhs.val.toFloat)
#    of amUnequal:
#      result = not v.toFloat.nearlyEqual(f.rhs.val.toFloat)
#    of amGreater:
#      result = v > f.rhs.val
#    of amLess:
#      result = v < f.rhs.val
#    of amGeq:
#      result = v >= f.rhs.val
#    of amLeq:
#      result = v <= f.rhs.val
#    else:
#      raise newException(Exception, "comparison of kind " & $f.op & " does " &
#        "not make sense for value kind of " & $v.kind & "!")
#  of VString:
#    doAssert not f.rhs.val.isNumber, "comparison must be with another string!"
#    case f.op
#    of amEqual:
#      result = v == f.rhs.val
#    of amUnequal:
#      result = v != f.rhs.val
#    of amGreater:
#      result = v > f.rhs.val
#    of amLess:
#      result = v < f.rhs.val
#    else:
#      raise newException(Exception, "comparison of kind " & $f.op & " does " &
#        "not make sense for value kind of " & $v.kind & "!")
#  of VBool:
#    doAssert f.rhs.val.kind == VBool, "comparison must be with another bool!"
#    case f.op
#    of amEqual:
#      result = v == f.rhs.val
#    of amUnequal:
#      result = v != f.rhs.val
#    of amGreater:
#      result = v > f.rhs.val
#    of amLess:
#      result = v < f.rhs.val
#    of amGeq:
#      result = v >= f.rhs.val
#    of amLeq:
#      result = v <= f.rhs.val
#    of amAnd:
#      result = v.toBool and f.rhs.val.toBool
#    of amOr:
#      result = v.toBool or f.rhs.val.toBool
#    of amXor:
#      result = v.toBool xor f.rhs.val.toBool
#    else:
#      raise newException(Exception, "comparison of kind " & $f.op & " does " &
#        "not make sense for value kind of " & $v.kind & "!")
#  else:
#    raise newException(Exception, "comparison for kind " & $v.kind &
#      " not yet implemented!")
#
#proc isValidRow(v: Value, f: FormulaNode): bool =
#  doAssert v.kind == VObject
#  doAssert f.kind == fkTerm
#  doAssert f.op in {amEqual, amUnequal, amGreater, amLess, amGeq, amLeq}
#  let lhsKey = f.lhs.val
#  doAssert f.lhs.val.kind == VString
#  result = v[lhsKey.str].isValidVal(f)
#
#proc delete(df: DataFrame, rowIdx: int): DataFrame =
#  result = df
#  for k in keys(df):
#    var s = df[k][0 ..< df.len]
#    s.delete(rowIdx)
#    #result[k] = s
#    result[k] = toPersistentVector(s)
#  result.len = result.len - 1

#proc add(df: var DataFrame, row: Value) =
#  for k in keys(row):
#    if not df.hasKey(k):
#      df[k] = newColumn(toColKind(row[k]))
#    df[k] = df[k].add row[k]
#    doAssert df.len + 1 == df[k].len
#  df.len = df.len + 1

proc filter(col: Column, filterIdx: Tensor[int]): Column =
  ## perform filterting of the given column `key`
  withNativeDtype(col):
    let t = toTensor(col, dtype)
    var res = newTensorUninit[dtype](filterIdx.size)
    if filterIdx.size > 0:
      var i = 0
      for idx in filterIdx:
        res[i] = t[idx]
        inc i
    result = res.toColumn

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
    of fkScalar: discard
  when dropCols:
    df.selectInplace(colsToKeep)

proc mutateInplace*(df: var DataFrame, fns: varargs[FormulaNode]) =
  ## Inplace variasnt of `mutate` below.
  df.mutateImpl(fns, dropCols = false)

proc mutate*(df: DataFrame, fns: varargs[FormulaNode]): DataFrame =
  ## Returns the data frame with an additional mutated column, described
  ## by the functions `fns`.
  ## Each formula `fn` given will be used to create a new column in the
  ## dataframe.
  ## We assume that the LHS of the formula corresponds to a fkVariable
  ## that's used to designate the new name.
  ## TODO: UPDATE!!!
  ## NOTE: If a given `fn` is a term (`fk`) without an assignment
  ## (using `~`, kind `amDep`) or a function (`fk`), the resulting
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
                 order: SortOrder): seq[int] =
  result = resIdx
  withNativeDtype(df[by[0]]):
    var res = newSeq[(int, dtype)](result.len)
    let t = toTensor(df[by[0]], dtype)
    for i, val in result:
      res[i] = (val, t[val])

    var mby = by
    mby.delete(0)
    var last = res[0][1]
    var cur = res[1][1]
    var i = startIdx
    var lastSearch = 0
    while i < res.len:
      cur = res[i][1]
      if last != cur or i == df.high:
        if i > lastSearch + 1:
          # sort between `lastSearch` and `i - 1`
          var subset = sortBySubset(df, mby[0], res[lastSearch ..< i].mapIt(it[0]), order = order)
          if mby.len > 1:
            # recurse again
            subset = sortRecurse(df, mby, lastSearch,
                                 resIdx = subset,
                                 order = order)

          result[lastSearch ..< i] = subset
        lastSearch = i
      last = res[i][1]
      inc i

proc sortBys(df: DataFrame, by: seq[string], order: SortOrder): seq[int] =
  withNativeDtype(df[by[0]]):
    var res = newSeq[(int, dtype)](df.len)
    var idx = 0
    for val in toTensor(df[by[0]], dtype):
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
    result = newDataFrame(allKeys.card)
    let resLen = (max(df1S.len, df2S.len))
    for k in allKeys:
      if k in df1S and k in df2S:
        doAssert df1S[k].kind == df2S[k].kind
        result.asgn(k, newColumn(kind = df1S[k].kind, length = resLen))
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
        for k in allKeys:
          if k in keys1 and k in keys2:
            doAssert equal(df1S[k], il, df2S[k], jl)
            result.assign(k, count, df1S[k], il)
          elif k in keys1:
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
    #for k in keys(seqTab):
    #  result[k] = seqTab[k].toPersistentVector

proc toSet[T](t: Tensor[T]): HashSet[T] =
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
    result.groupMap[key] = toSet(result[key].toTensor(Value))

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
    var sumStats = initTable[string, seq[Value]]()
    var keys = initTable[string, seq[Value]](df.groupMap.len)
    var idx = 0
    for fn in fns:
      doAssert fn.kind == fkScalar
      lhsName = fn.valName
      sumStats[lhsName] = newSeqOfCap[Value](1000) # just start with decent size
      for class, subdf in groups(df):
        for (key, label) in class:
          if key notin keys: keys[key] = newSeqOfCap[Value](1000)
          keys[key].add label
        sumStats[lhsName].add fn.fnS(subDf)
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
  var keys = initTable[string, seq[Value]](grouped.groupMap.len)
  var idx = 0
  for class, subdf in groups(grouped):
    #echo "!!! ", subdf
    for (c, val) in class:
      if c notin keys: keys[c] = newSeqOfCap[Value](1000)
      keys[c].add val
    counts.add subDf.len
    inc idx
  for k, vals in keys:
    result.asgn(k, toNativeColumn vals)
  result.asgn(name, toColumn counts)
  result.len = idx

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
  if df.len == 0:
    df = dfToAdd
  else:
    doAssert df.getKeys == dfToAdd.getKeys, "all keys must match to add dataframe!"
    df = bind_rows([("", df), ("", dfToAdd)])

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
    diff = symmetricDifference(toSet(h1), toSet(h2))
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
    diff = toSet(h1) - toSet(h2)
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
  let remainCols = getKeys(df).toSet.difference(cols.toSet)
  let newLen = cols.len * df.len
  # assert all columns same type
  # TODO: relax this restriction, auto convert to `colObject` if non matching
  assert cols.mapIt(df[it].kind).deduplicate.len == 1, "all gathered columns must be of the same type!"
  var keyTensor = newTensorUninit[string](newLen)
  withNativeDtype(df[cols[0]]):
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
  hashes.hashColumn(c)
  # finalize the hashes
  hashes.applyIt(!$it)
  var hSet = toSet(hashes)
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

proc unique*(df: DataFrame, cols: varargs[string]): DataFrame =
  ## returns a DF with only distinct rows. If one or more `cols` are given
  ## the uniqueness of a row is only determined based on those columns. By
  ## default all columns are considered.
  ## NOTE: The corresponding `dplyr` function is `distinct`. The choice for
  ## `unique` was made, since `distinct` is a keyword in Nim!
  result = newDataFrame(df.ncols)
  var mcols = @cols
  if mcols.len == 0:
    mcols = getKeys(df)
  let hashes = buildColHashes(df, mcols)
  var hSet = toSet(hashes)
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
  for k in mcols:
    result.asgn(k, df[k].filter(idxToKeep))
    # fill each key with the non zero elements
  result.len = idxToKeep.size

func evaluate*(node: FormulaNode): Value =
  ## tries to return a single `Value` from a FormulaNode.
  ## Works either if formula is `fkNone` or `fkVariable`.
  ## Raises for `fkVector` and `fkScalar`.
  case node.kind
  of fkVariable: result = node.val
  of fkAssign: result = node.rhs # ?? TODO: should this be allowed?
  of fkScalar, fkVector:
    raise newException(ValueError, "Cannot evaluate a formula of kind " &
      $node.kind & " without a data frame as input!")

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
  of fkScalar:
    raise newException(ValueError, "Cannot evaluate a formula of kind " &
      $node.kind & " without a data frame as input!")

#proc evaluate*[T](node: FormulaNode, df: DataFrame,
#                  idx: int,
#                  dtype: typedesc[T]): T =
#  case node.kind
#  of fkVector:
#    # filter to indices
#    withNativeDtype(df[
#    result = node.fnV(df)
#  else:
#    raise newException(ValueError, "Cannot evaluate a formula of kind " &
#      $node.kind & " without a data frame as input!")


#
#proc evaluate*(node: FormulaNode, data: DataFrame): PersistentVector[Value] =
#  ## evaluation of a data frame under a given `FormulaNode`. This is a non-reducing
#  ## operation. It returns a `PersitentVector[Value]` from a whole data frame (by working on
#  ## a single column) and applying `node` to each element.
#  case node.kind
#  of fkVariable:
#    case node.val.kind
#    of VString:
#      # the given node corresponds to a key of the data frame
#      # TODO: maybe extend this so that if `node.val` is ``not`` a key of the dataframe
#      # we take the literal string value instead?
#      if node.val.str in data:
#        result = data[node.val.str]
#      else:
#        # if it's not a key, we use the literal
#        result = toPersistentVector(toSeq(0 ..< data.len).mapIt(node.val))
#    of VFloat, VInt, VBool:
#      # take the literal value of the node
#      result = toPersistentVector(toSeq(0 ..< data.len).mapIt(node.val))
#    else:
#      raise newException(Exception, "Node kind of " & $node.kind & " does not " &
#        "make sense for evaluation!")
#  of fkTerm:
#    let lhs = evaluate(node.lhs, data)
#    let rhs = evaluate(node.rhs, data)
#    doAssert lhs.len == rhs.len
#    var res = newSeq[Value](lhs.len)
#    for i in 0 ..< lhs.len:
#      res[i] = evaluate FormulaNode(kind: fkTerm, op: node.op, lhs: f{lhs[i]}, rhs: f{rhs[i]})
#    result = toPersistentVector(res)
#  of fkFunction:
#    case node.fnKind
#    of funcScalar:
#      # just a function taking a scalar. Apply to current `idx`
#      var res = newSeq[Value](data.len)
#      for i in 0 ..< data.len:
#        res[i] = node.evaluate(data, i)
#      result = toPersistentVector(res)
#    of funcVector:
#      raise newException(Exception, "Reductive vector like proc cannot be evaluated to " &
#        "return a vector!")
