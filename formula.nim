import macros, tables, strutils, options

import persvector, sequtils, seqmath, stats, strformat

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

  ArithmeticKind* = enum
    amPlus = "+"
    amMinus = "-"
    amMul = "*"
    amDiv = "/"
    amDep = "~"
    amEqual = "=="
    amGreater = ">"
    amLess = ">"
    amGeq = ">="
    amLeq = "<="

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
      val*: string # TODO: replace by `Value`!
    of fkFunction:
      # storing a function to be applied to the data
      fnName*: string
      fn*: proc(s: PersistentVector[Value]): Value
      arg*: FormulaNode
      res: Option[Value] # the result of fn(arg), so that we can cache it
                         # instead of recalculating it for every index potentially
iterator keys(row: Value): string =
  doAssert row.kind == VObject
  for k in keys(row.fields):
    yield k

proc `[]`*(v: Value, key: string): Value =
  doAssert v.kind == VObject
  result = v.fields[key]

proc `[]=`*(v: var Value, key: string, val: Value) =
  doAssert v.kind == VObject
  v.fields[key] = val

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
    result = v.str
  of VObject:
    for k, x in v.fields:
      result.add (&"{k} : {x}")
  of VNull:
    result = "null"

proc toSeq(v: PersistentVector[Value]): seq[Value] =
  result = v[0 ..< v.len]

proc toFloat*(v: Value): float =
  doAssert v.kind in {VInt, VFloat}
  case v.kind
  of VInt: result = v.num.float
  of VFloat: result = v.fnum
  else: discard

template liftScalarFloatProc(name: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): Value =
    result = Value(kind: VFloat, fnum: `name`(v[0 ..< v.len].mapIt(it.toFloat)))

template liftScalarIntProc(name: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): Value =
    result = Value(kind: VInt, num: `name`(v[0 ..< v.len].mapIt(it.toInt)))

template liftScalarStringProc(name: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): Value =
    result = Value(kind: VString, str: `name`(v[0 ..< v.len].mapIt(it.toInt)))

liftScalarFloatProc(mean)

template liftVectorProcToPersVec(name: untyped, outType: untyped): untyped =
  proc `name`*(v: PersistentVector[Value]): `outType` =
    result = v[0 ..< v.len].mapIt(`name`(it.toFloat))

liftVectorProcToPersVec(ln, seq[float])

#template liftProcToString(name: untyped, outType: untyped): untyped =
#  proc `name`(df: DataFrame, x: string): `outType` =
#    result = `name`(df[x])
#
#liftProcToString(mean, float)

proc serialize*[T](node: var FormulaNode, data: T, idx: int): float
proc constructVariable*(n: NimNode): NimNode
proc constructFunction*(n: NimNode): NimNode
proc buildFormula(n: NimNode): NimNode
proc handleSide(n: NimNode): NimNode =
  case n.kind
  of nnkInfix:
    result = buildFormula(n)
  of nnkStrLit:
    result = constructVariable(n)
  of nnkCall:
    result = constructFunction(n)
  else:
    raise newException(Exception, "Not implemented!")

proc buildFormula(n: NimNode): NimNode =
  expectKind(n, nnkInfix)
  let opid = n[0].strVal
  let op = quote do:
    parseEnum[ArithmeticKind](`opid`)
  let lhs = handleSide(n[1])
  let rhs = handleSide(n[2])
  result = quote do:
    FormulaNode(kind: fkTerm, lhs: `lhs`, rhs: `rhs`, op: `op`)

macro `{}`*(x, y: untyped): untyped =
  if x.repr == "f":
    result = buildFormula(y)


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
    echo n.treeRepr
    error("Unsupported kind " & $n.kind)

proc constructVariable(n: NimNode): NimNode =
  var val = ""
  if n.kind != nnkNilLit:
    val = n.strVal
  else:
    # empty value meaning no comparison. Only allowed for something like
    # ~ x
    val = ""
  result = quote do:
    FormulaNode(kind: fkVariable, val: `val`)

proc constructFunction*(n: NimNode): NimNode =
  let fname = n[0].strVal
  let fn = n[0]
  let arg = constructVariable(n[1])
  result = quote do:
    FormulaNode(kind: fkFunction, fnName: `fname`, fn: `fn`, arg: `arg`)

proc isSingle(x, y: NimNode, op: ArithmeticKind): NimNode =
  var
    lhs: NimNode
    rhs: NimNode
  if x.len == 0:
    # is single
    lhs = constructVariable(x)
  else:
    lhs = expand(x)
  if y.len == 0:
    # is single
    rhs = constructVariable(y)
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

template `+`*(x, y: untyped): FormulaNode =
  deconstruct(x, y, amPlus)

template `-`*(x, y: untyped): FormulaNode =
  deconstruct(x, y, amMinus)

template `*`*(x, y: untyped): FormulaNode =
  deconstruct(x, y, amMinus)

template `/`*(x, y: untyped): FormulaNode =
  deconstruct(x, y, amDiv)

proc initVariable(x: string): FormulaNode =
  result = FormulaNode(kind: fkVariable,
                       val: x)

template makeMathProc(operator, opKind: untyped): untyped =
  proc `operator`*(x, y: string): FormulaNode =
    let
      lhs = initVariable(x)
      rhs = initVariable(y)
    result = FormulaNode(kind: fkTerm, lhs: lhs, rhs: rhs,
                         op: opKind)
  proc `operator`*(lhs: FormulaNode, y: string): FormulaNode =
    let rhs = initVariable(y)
    result = FormulaNode(kind: fkTerm, lhs: lhs, rhs: rhs,
                         op: opKind)
  proc `operator`*(x: string, rhs: FormulaNode): FormulaNode =
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
    result.add node.val
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

proc serialize*(node: FormulaNode, data: Table[string, seq[string]], idx: int): float =
  case node.kind
  of fkVariable:
    result = data[node.val][idx].parseFloat
  of fkTerm:
    case node.op
    of amPlus:
      result = node.lhs.serialize(data, idx) + node.rhs.serialize(data, idx)
    of amMinus:
      result = node.lhs.serialize(data, idx) - node.rhs.serialize(data, idx)
    of amMul:
      result = node.lhs.serialize(data, idx) * node.rhs.serialize(data, idx)
    of amDiv:
      result = node.lhs.serialize(data, idx) / node.rhs.serialize(data, idx)
    of amDep:
      raise newException(Exception, "Cannot serialize a term still containing a dependency!")
