import tables, strutils, math, fenv, parseutils, strformat, hashes

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
      num*: int #BiggestInt
    of VFloat:
      fnum*: float
    of VBool:
      bval*: bool
    of VObject:
      fields*: OrderedTable[string, Value] # alternative: `seq[(string, Value)]` pairs?
    of VNull:
      discard

proc pretty*(v: Value, precision = 4, emphStrNumber = true): string

func toValKind*[T](dtype: typedesc[T]): ValueKind =
  when T is float:
    result = VFloat
  elif T is int:
    result = VInt
  elif T is bool:
    result = VBool
  elif T is string:
    result = VString
  elif T is Value:
    result = VObject

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

proc contains*(v: Value, key: string): bool =
  doAssert v.kind == VObject
  result = v.fields.hasKey(key)

proc `[]`*(v: Value, key: string): Value {.inline.} =
  doAssert v.kind == VObject
  result = v.fields[key]

proc `[]=`*(v: var Value, key: string, val: Value) {.inline.} =
  doAssert v.kind == VObject
  v.fields[key] = val

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

proc newVObject*(length = 8): Value =
  result = Value(kind: VObject)
  result.fields = initOrderedTable[string, Value](nextPowerOfTwo(length))

proc `%~`*[T: not Value](s: openArray[T]): seq[Value] =
  ## converts a `seq[T]` to a `seq[Value]`
  result = newSeq[Value](s.len)
  for i, x in s:
    result[i] = %~ x

template `%~`*(s: openArray[Value]): seq[Value] = @s

proc toObject*(s: seq[(string, Value)]): Value =
  ## converts the given sequence to an object
  ## This is only used to store the result of the `groups` iterator as a
  ## `Value`.
  result = Value(kind: VObject)
  result.fields = initOrderedTable[string, Value]()
  for (key, val) in s:
    result.fields[key] = val

proc toObject*(s: (string, Value)): Value = toObject(@[s])

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

proc toInt*(v: Value): int = #BiggestInt =
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
  of VInt, VFloat, VBool: result = pretty(v)
  of VString: result = v.str
  else:
    raise newException(ValueError, "Will not convert a Value of kind " &
      $v.kind & " to string! Use `$` for that!")

proc to*[T: int | float | string | bool](v: Value, dtype: typedesc[T]): T =
  when T is int:
    result = v.toInt
  elif T is float:
    result = v.toFloat
  elif T is string:
    result = v.toStr
  elif T is bool:
    result = v.toBool
  else:
    doAssert false, "Impossible branch!"

template withNative*(v: Value,
                     valName: untyped,
                     body: untyped): untyped =
  case v.kind
  of VInt:
    let `valName` {.inject.} =  v.num
    body
  of VFloat:
    let `valName` {.inject.} =  v.fnum
    body
  of VString:
    let `valName` {.inject.} =  v.str
    body
  of VBool:
    let `valName` {.inject.} =  v.bval
    body
  of VObject, VNull:
    doAssert false, "not implemented / makes no sense for current usage"

template withNativeConversion*(kind: ValueKind,
                               procName: untyped,
                               body: untyped): untyped =
  ## generates an environment, in which the correct `to*` proc
  ## is named `procName` for `kind`
  case kind
  of VInt:
    template `procName`(v: Value): untyped = v.toInt
    type dtype {.inject.} = int
    body
  of VFloat:
    template `procName`(v: Value): untyped = v.toFloat
    type dtype {.inject.} = float
    body
  of VString:
    template `procName`(v: Value): untyped = v.toStr
    type dtype {.inject.} = string
    body
  of VBool:
    template `procName`(v: Value): untyped = v.toBool
    type dtype {.inject.} = bool
    body
  of VObject, VNull:
    doAssert false, "not implemented / makes no sense for current usage"


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
  elif v.kind != w.kind and
     v.kind in {VFloat, VInt, VString} and
     w.kind in {VFloat, VInt, VString}:
    # compare as strings
    result = $v < $w
  elif v.kind == w.kind:
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

proc smallerOrFalse*(v: Value, f: float): bool {.inline.} =
  ## extension of `<` for `Value` to return `false` if `v` is
  ## not a valid VInt/VFloat.
  case v.kind
  of VInt, VFloat: result = v.toFloat < f
  else: result = false

proc largerOrFalse*(v: Value, f: float): bool {.inline.} =
  ## extension of `<` for `Value` to return `false` if `v` is
  ## not a valid VInt/VFloat.
  case v.kind
  of VInt, VFloat: result = v.toFloat > f
  else: result = false

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
    var idx = 0
    for k, x in v.fields:
      if idx == v.fields.len - 1:
        result.add (&"{k}: {pretty(x)}")
      else:
        result.add (&"{k}: {pretty(x)}, ")
      inc idx
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

proc contains*(v: Value, has: Value): bool =
  ## checks whether `has` is a subset of `v` if both are `VObject`.
  ## A subset means that all keys of `has` are in `v` and their values match.
  ## There may be more fields in `v` than in `has`
  doAssert v.kind == VObject
  doAssert has.kind == VObject
  result = true
  for key, val in has:
    if key in v: result = result and val == v[key]
    else: result = false
