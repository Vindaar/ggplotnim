import arraymancer
import value, sugar, math, strformat

type
  ColKind* = enum
    colNone, colFloat, colInt, colBool, colString, colObject
  Column* = ref object
    len*: int
    case kind*: ColKind
    of colFloat: fCol*: Tensor[float]
    of colInt: iCol*: Tensor[int]
    of colBool: bCol*: Tensor[bool]
    of colString: sCol*: Tensor[string]
    of colObject: oCol*: Tensor[Value]
    of colNone: discard

# just a no-op
template toColumn*(c: Column): Column = c

func high*(c: Column): int = c.len - 1

proc toColumn*[T: float | int | string | bool | Value](t: Tensor[T]): Column =
  when T is int:
    result = Column(kind: colInt,
                    iCol: t,
                    len: t.size)
  elif T is float:
    result = Column(kind: colFloat,
                    fCol: t,
                    len: t.size)
  elif T is bool:
    result = Column(kind: colBool,
                    bCol: t,
                    len: t.size)
  elif T is string:
    result = Column(kind: colString,
                    sCol: t,
                    len: t.size)
  elif T is Value:
    result = Column(kind: colObject,
                    oCol: t,
                    len: t.size)

proc constantColumn*[T](val: T, len: int): Column =
  ## creates a constant column based on `val` and its type
  when T is Value:
    withNative(val, x):
      result = toColumn newTensorWith[type(x)](len, x)
  else:
    result = toColumn newTensorWith[T](len, val)

proc `[]`*(c: Column, slice: Slice[int]): Column =
  case c.kind
  of colInt: result = toColumn c.iCol[slice.a .. slice.b]
  of colFloat: result = toColumn c.fCol[slice.a .. slice.b]
  of colString: result = toColumn c.sCol[slice.a .. slice.b]
  of colBool: result = toColumn c.bCol[slice.a .. slice.b]
  of colObject: result = toColumn c.oCol[slice.a .. slice.b]
  of colNone: raise newException(IndexError, "Accessed column is empty!")

proc newColumn*(kind = colNone, length = 0): Column =
  case kind
  of colFloat: result = toColumn newTensor[float](length)
  of colInt: result = toColumn newTensor[int](length)
  of colString: result = toColumn newTensor[string](length)
  of colBool: result = toColumn newTensor[bool](length)
  of colObject: result = toColumn newTensor[Value](length)
  of colNone: result = Column(kind: colNone, len: 0)


proc toColKind*[T](dtype: typedesc[T]): ColKind =
  when T is float:
    result = colFloat
  elif T is int:
    result = colInt
  elif T is bool:
    result = colBool
  elif T is string:
    result = colString
  elif T is Value:
    result = colObject

proc toColKind*(vKind: ValueKind): ColKind =
  case vKind
  of VFloat: result = colFloat
  of VInt: result = colInt
  of VString: result = colString
  of VBool: result = colBool
  of VObject: result = colObject
  of VNull: result = colObject

proc toValueKind*(colKind: ColKind): ValueKind =
  case colKind
  of colFloat: result = VFloat
  of colInt: result = VInt
  of colString: result = VString
  of colBool: result = VBool
  of colObject: result = VObject
  of colNone: result = VNull

proc toNimType*(colKind: ColKind): string =
  ## returns the string name of the underlying data type of the column kind
  case colKind
  of colFloat: result = "float"
  of colInt: result = "int"
  of colString: result = "string"
  of colBool: result = "bool"
  of colObject: result = "object"
  of colNone: result = "null"

proc asValue*[T](t: Tensor[T]): Tensor[Value] {.noInit.} =
  ## Apply type conversion on the whole tensor
  result = t.map(x => (%~ x))

proc valueTo*[T](t: Tensor[Value], dtype: typedesc[T],
                 dropNulls: static bool = false): Tensor[T] =
  when not dropNulls:
    when T is string:
      result = t.map(x => x.toStr)
    elif T is float:
      result = t.map(x => x.toFloat)
    elif T is int:
      result = t.map(x => x.toInt)
    elif T is bool:
      result = t.map(x => x.toBool)
    elif T is Value:
      result = t
  else:
    # filter tensor to non Null values
    var outputIdx = newSeqOfCap[int](t.size)
    for idx, x in t:
      if x.kind != VNull:
        outputIdx.add idx[0]
    result = newTensor[T](outputIdx.len)
    when T is string:
      for i, idx in outputIdx:
        result[i] = t[idx].toStr
    elif T is float:
      for i, idx in outputIdx:
        result[i] = t[idx].toFloat
    elif T is int:
      for i, idx in outputIdx:
        result[i] = t[idx].toInt
    elif T is bool:
      for i, idx in outputIdx:
        result[i] = t[idx].toBool
    elif T is Value:
      for i, idx in outputIdx:
        result[i] = t[idx]

proc toTensor*[T](c: Column, dtype: typedesc[T],
                  dropNulls: static bool = false): Tensor[T] =
  ## `dropNulls` only has an effect on `colObject` columns. It allows to
  ## drop Null values to get (hopefully) a valid raw Tensor
  case c.kind
  of colInt:
    when T is int:
      result = c.iCol
    elif T is SomeNumber:
      result = c.iCol.asType(T)
    elif T is Value:
      result = c.iCol.asValue
  of colFloat:
    when T is float:
      result = c.fCol
    elif T is SomeNumber:
      result = c.fCol.asType(T)
    elif T is Value:
      result = c.fCol.asValue
  of colString:
    when T is string:
      result = c.sCol
    elif T is Value:
      result = c.sCol.asValue
  of colBool:
    when T is bool:
      result = c.bCol
    elif T is Value:
      result = c.bCol.asValue
  of colObject:
    result = c.oCol.valueTo(T, dropNulls = dropNulls)
  of colNone: raise newException(ValueError, "Accessed column is empty!")

proc toTensor*[T](c: Column, slice: Slice[int], dtype: typedesc[T]): Tensor[T] =
  case c.kind
  of colInt:
    when T is int:
      result = c.iCol[slice.a .. slice.b]
    elif T is SomeNumber:
      result = c.iCol[slice.a .. slice.b].asType(T)
  of colFloat:
    when T is float:
      result = c.fCol[slice.a .. slice.b]
    elif T is SomeNumber:
      result = c.fCol[slice.a .. slice.b].asType(T)
  of colString:
    when T is string:
      result = c.sCol[slice.a .. slice.b]
  of colBool:
    when T is bool:
      result = c.bCol[slice.a .. slice.b]
  of colObject:
    result = c.oCol[slice.a .. slice.b].valueTo(T)
  of colNone: raise newException(ValueError, "Accessed column is empty!")

proc `[]`*[T](c: Column, idx: int, dtype: typedesc[T]): T =
  when T isnot Value:
    case c.kind
    of colInt:
      when T is int:
        result = c.iCol[idx]
      elif T is SomeNumber:
        result = c.iCol[idx].T
      elif T is string:
        result = $c.iCol[idx]
    of colFloat:
      when T is float:
        result = c.fCol[idx]
      elif T is SomeNumber:
        result = c.fCol[idx].T
      elif T is string:
        # convert to Value and then string so that we use one single
        # formatting function. This is slow anyways
        result = pretty(%~ c.fCol[idx])
    of colString:
      when T is string:
        result = c.sCol[idx]
    of colBool:
      when T is bool:
        result = c.bCol[idx]
    of colObject:
      when T is string:
        result = c.oCol[idx].toStr
      elif T is float:
        result = c.oCol[idx].toFloat
      elif T is int:
        result = c.oCol[idx].toInt
      elif T is bool:
        result = c.oCol[idx].toBool
    of colNone: raise newException(ValueError, "Accessed column is empty!")
  else:
    case c.kind
    of colInt: result = %~ c.iCol[idx]
    of colFloat: result = %~ c.fCol[idx]
    of colString: result = %~ c.sCol[idx]
    of colBool: result = %~ c.bCol[idx]
    of colObject: result = c.oCol[idx]
    of colNone: raise newException(ValueError, "Accessed column is empty!")

proc `[]=`*[T](c: var Column, idx: int, val: T) =
  ## assign `val` to column `c` at index `idx`
  ## If the types match, it just calls `[]=` on the tensor.
  ## If they are compatible, `val` is converted to c's type.
  ## If they are incompatible, `c` will be rewritten to an object
  ## column.
  var rewriteAsValue = false
  case c.kind
  of colFloat:
    when T is float:
      c.fCol[idx] = val
    elif T is SomeNumber:
      c.fCol[idx] = val.float
  of colInt:
    when T is int:
      c.iCol[idx] = val
    else:
      rewriteAsValue = true
  of colString:
    when T is string:
      c.sCol[idx] = val
    else:
      rewriteAsValue = true
  of colBool:
    when T is bool:
      c.bCol[idx] = val
    else:
      rewriteAsValue = true
  of colObject:
    c.oCol[idx] = %~ val
  of colNone: raise newException(ValueError, "Accessed column is empty!")
  if rewriteAsValue:
    # rewrite as an object column
    c = c.toObjectColumn()
    c.oCol[idx] = %~ val

template withNativeTensor*(c: Column,
                           valName: untyped,
                           body: untyped): untyped =
  case c.kind
  of colInt:
    let `valName` {.inject.} =  c.iCol
    body
  of colFloat:
    let `valName` {.inject.} =  c.fCol
    body
  of colString:
    let `valName` {.inject.} =  c.sCol
    body
  of colBool:
    let `valName` {.inject.} =  c.bCol
    body
  of colObject:
    let `valName` {.inject.} =  c.oCol
    body
  of colNone: raise newException(ValueError, "Accessed column is empty!")

template `%~`*(v: Value): Value = v

proc toObjectColumn*(c: Column): Column =
  ## returns `c` as an object column
  var res = newTensor[Value](c.len)
  withNativeTensor(c, t):
    for idx in 0 ..< c.len:
      res[idx] = %~ (t[idx])

template withNative*(c: Column, idx: int,
                     valName: untyped,
                     body: untyped): untyped =
  case c.kind
  of colInt:
    let `valName` {.inject.} =  c[idx, int]
    body
  of colFloat:
    let `valName` {.inject.} =  c[idx, float]
    body
  of colString:
    let `valName` {.inject.} =  c[idx, string]
    body
  of colBool:
    let `valName` {.inject.} =  c[idx, bool]
    body
  of colObject:
    let `valName` {.inject.} =  c[idx, Value]
    body
  of colNone: raise newException(ValueError, "Accessed column is empty!")

template withNativeDtype*(c: Column, body: untyped): untyped =
  case c.kind
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
  of colNone: raise newException(ValueError, "Accessed column is empty!")

proc equal*(c1: Column, idx1: int, c2: Column, idx2: int): bool =
  ## checks if the value in `c1` at `idx1` is equal to the
  ## value in `c2` at `idx2`
  if c1.kind != c2.kind: return false
  withNativeDtype(c1):
    result = c1[idx1, dtype] == c2[idx2, dtype]

template withNative2*(c1, c2: Column, idx1, idx2: int,
                      valName1, valName2: untyped,
                      body: untyped): untyped =
  assert c1.kind == c2.kind
  case c1.kind
  of colInt:
    let `valName1` {.inject.} =  c1[idx1, int]
    let `valName2` {.inject.} =  c2[idx2, int]
    body
  of colFloat:
    let `valName1` {.inject.} =  c1[idx1, float]
    let `valName2` {.inject.} =  c2[idx2, float]
    body
  of colString:
    let `valName1` {.inject.} =  c1[idx1, string]
    let `valName2` {.inject.} =  c2[idx2, string]
    body
  of colBool:
    let `valName1` {.inject.} =  c1[idx1, bool]
    let `valName2` {.inject.} =  c2[idx2, bool]
    body
  of colObject:
    let `valName1` {.inject.} =  c1[idx1, Value]
    let `valName2` {.inject.} =  c2[idx2, Value]
    body
  of colNone: raise newException(ValueError, "Accessed column is empty!")

proc compatibleColumns*(c1, c2: Column): bool {.inline.} =
  if c1.kind == c2.kind: result = true
  elif c1.kind in {colInt, colFloat} and
       c2.kind in {colInt, colFloat}:
    result = true
  else: result = false

proc toObject*(c: Column): Column {.inline.} =
  case c.kind
  of colObject: result = c
  of colInt: result = toColumn c.iCol.asValue
  of colFloat: result = toColumn c.fCol.asValue
  of colString: result = toColumn c.sCol.asValue
  of colBool: result = toColumn c.bCol.asValue
  of colNone: raise newException(ValueError, "Accessed column is empty!")

proc add*(c1, c2: Column): Column =
  ## adds column `c2` to `c1`. Uses `concat` internally.
  if c2.len == 0: return c1
  elif c1.len == 0: return c2
  if c1.kind == c2.kind:
    # just concat directly
    case c1.kind
    of colInt: result = toColumn concat(c1.iCol, c2.iCol, axis = 0)
    of colFloat: result = toColumn concat(c1.fCol, c2.fCol, axis = 0)
    of colBool: result = toColumn concat(c1.bCol, c2.bCol, axis = 0)
    of colString: result = toColumn concat(c1.sCol, c2.sCol, axis = 0)
    of colObject: result = toColumn concat(c1.oCol, c2.oCol, axis = 0)
    of colNone: doAssert false, "Both columns are empty!"
  elif compatibleColumns(c1, c2):
    # convert both to float
    case c1.kind
    of colInt:
      # c1 is int, c2 is float
      assert c2.kind == colFloat
      result = toColumn concat(c1.iCol.asType(float), c2.fCol, axis = 0)
    of colFloat:
      # c1 is float, c2 is int
      assert c2.kind == colInt
      result = toColumn concat(c1.fCol, c2.iCol.asType(float), axis = 0)
    else: doAssert false, "cannot happen, since not compatible!"
  else:
    # convert both columns to Value
    result = toColumn concat(c1.toObject.oCol, c2.toObject.oCol, axis = 0)
  result.len = c1.len + c2.len

proc toColumn*[T: float | string | int | bool | Value](s: openArray[T]): Column =
  var vals = newTensor[T](s.len)
  for i, x in s:
    vals[i] = x
  result = toColumn(vals)

proc toColumn*[T: float | string | int | bool | Value](x: T): Column =
  # also possible to create single row column, but inefficient
  # for `summarize` though there's no way around
  let vals = newTensorWith[T](1, x)
  result = toColumn(vals)

proc toNativeColumn*(s: openArray[Value]): Column =
  ## given input as `Value`, will attempt to return the column as native
  ## data type.
  ## NOTE: this is unsafe and assumes the values are indeed all one type!
  if s.len > 0:
    withNativeConversion(s[0].kind, get):
      var data = newTensor[dtype](s.len)
      for i, x in s:
        data[i] = get(x)
      result = toColumn data

proc nullColumn*(num: int): Column =
  ## returns an object `Column` with `N` values, which are
  ## all `VNull`
  var nullseq = newSeq[Value](num)
  for i in 0 ..< num:
    nullseq[i] = Value(kind: VNull)
  result = toColumn(nullseq)

#proc `*`[T: SomeNumber]*(c: Column, x: T)
proc contains*[T: float | string | int | bool | Value](c: Column, val: T): bool =
  let t = toTensor(c, T)
  result = false
  for x in t:
    if val == x:
      return true

template liftScalarToColumn*(name: untyped): untyped =
  proc `name`*(c: Column): Value =
    withNativeDtype(c):
      result = %~ `name`(c.toTensor(dtype))
liftScalarToColumn(max)

proc pretty*(c: Column): string =
  ## pretty prints a Column
  result = &"Column of type: {toNimType(c.kind)} with length: {c.len}\n"
  withNativeTensor(c, t):
    result.add &"  contained Tensor: {t}"
template `$`*(c: Column): string = pretty(c)
