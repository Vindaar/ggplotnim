import macros, sequtils, strformat
import formulaNameMacro

import arraymancer_backend, column, value, df_types

type
  AssignKind = enum
    byIndex, byTensor
  ## either: `t in df["foo", int]`
  ## or: `t = df["foo", int]`
  Assign = object
    asgnKind: AssignKind
    element: NimNode # e.g. `t`
    tensor: NimNode # either `t` or `t_T` if `elmenent` used
    col: NimNode # name of the column
    colType: NimNode # e.g. `float`
  Preface = object
    args: seq[Assign]
  FormulaCT = object
    preface: Preface
    resType: NimNode # e.g. `ident"float"`
    name: string # name of the formula body as lisp
    loop: NimNode # loop needs to be patched to remove accented quotes etc

const dtypes = ["float", "int", "string", "bool", "Value"]

proc checkIdent(n: NimNode, s: string): bool =
  result = n.len > 0 and n[0].kind == nnkIdent and n[0].strVal == s

proc extractCall(stmts: NimNode, id: string): NimNode =
  expectKind(stmts, nnkStmtList)
  for ch in stmts:
    case ch.kind
    of nnkCall:
      if checkIdent(ch, id):
        return ch
    else: continue

proc parsePreface(n: NimNode): Preface =
  proc addInfixAssign(ch: NimNode): Assign =
    doAssert checkIdent(ch, "in")
    doAssert ch[1].kind == nnkIdent, "First element before `in` needs to be an ident!"
    doAssert ch[2].kind == nnkBracketExpr, "`in` must refer to a `df[<col>, <type>]`!"
    doAssert ch[2][0].strVal == "df", "`in` must refer to a `df[<col>, <type>]`!"
    let elId = ch[1].strVal
    let dtype = ch[2][2].strVal
    doAssert dtype in dtypes, "Column dtype " & $dtype & " not in " & $dtypes & "!"
    result = Assign(asgnKind: byIndex,
                    element: ident(elId),
                    tensor: ident(elId & "T"),
                    col: ch[2][1],
                    colType: ident(dtype))
  proc addAsgnAssign(ch: NimNode): Assign =
    doAssert ch[0].kind == nnkIdent, "First element before `=` needs to be an ident!"
    doAssert ch[1].kind == nnkBracketExpr, "`=` must assign from a `df[<col>, <type>]`!"
    doAssert ch[1][0].strVal == "df", "`=` must assign from a `df[<col>, <type>]`!"
    let tId = ch[0].strVal
    let dtype = ch[1][2].strVal
    doAssert dtype in dtypes, "Column dtype " & $dtype & " not in " & $dtypes & "!"
    result = Assign(asgnKind: byTensor,
                    element: ident(tId & "Idx"),
                    tensor: ident(tId),
                    col: ch[1][1],
                    colType: ident(dtype))

  expectKind(n, nnkCall)
  expectKind(n[1], nnkStmtList)
  for ch in n[1]:
    case ch.kind
    of nnkInfix: result.args.add addInfixAssign(ch)
    of nnkAsgn: result.args.add addAsgnAssign(ch)
    else: error("Invalid node kind " & $ch.kind & " in `preface`: " & (ch.repr))

proc parseSingle(n: NimNode): NimNode =
  expectKind(n[1], nnkStmtList)
  result = n[1][0]

proc parseLoop(n: NimNode): NimNode =
  expectKind(n[1], nnkStmtList)
  result = n[1]

proc convertPreface(p: Preface): NimNode =
  proc toLet(a: Assign): NimNode =
    result = nnkIdentDefs.newTree(
      a.tensor,
      newEmptyNode(),
      nnkBracketExpr.newTree(ident"df", a.col, a.colType)
    )
  result = nnkLetSection.newTree()
  for arg in p.args:
    result.add toLet(arg)

proc convertDtype(d: NimNode): NimNode =
  result = nnkVarSection.newTree(
    nnkIdentDefs.newTree(
      ident"res",
      newEmptyNode(),
      nnkCall.newTree(
        nnkBracketExpr.newTree(ident"newTensor",
                               d),
        nnkDotExpr.newTree(ident"df",
                           ident"len"))
    )
  )

proc `$`(p: Preface): string =
  $p.args.mapIt(&"Assign(element: {it.element.strVal}, tensor: {it.tensor.strVal}))")

proc contains(p: Preface, s: string): bool =
  for arg in p.args:
    if arg.element.strVal == s:
      return true
    if arg.tensor.strVal == s:
      return true

proc `[]`(p: Preface, s: string): Assign =
  for arg in p.args:
    if arg.element.strVal == s:
      return arg
    if arg.tensor.strVal == s:
      return arg
  error("Could not find " & s & " in preface containing " & $p)

proc fixupTensorIndices(loopStmts: NimNode, preface: Preface, toElements: bool): NimNode =
  ## If `toElements` is true, we rewrite everything by `t` (where `t` is an
  ## element of `tT` (Tensor). This includes
  expectKind(loopStmts, nnkStmtList)
  if toElements:
    proc replaceByIdx(n: NimNode, preface: Preface): NimNode =
      ## recurses the node `n` and replaces all occurences by `t[idx]` for each
      ## tensor in the loop

      # first check if an ident that is in preface we have to replace or if
      # an `nnkBracketExpr` which contains an ident from `preface`. In those cases
      # return early
      case n.kind
      of nnkIdent, nnkSym:
        if n.strVal in preface:
          return nnkBracketExpr.newTree(
            preface[n.strVal].tensor,
            ident"idx"
          )
        else: return n
      of nnkBracketExpr:
        if n[0].kind == nnkIdent and n[0].strVal in preface:
          return n
      else: result = n
      if n.len > 0:
        result = newTree(n.kind)
        for ch in n:
          result.add replaceByIdx(ch, preface)

    let loop = loopStmts[0].replaceByIdx(preface)
    echo loop.treeRepr, " is this::: ", loop.repr
    case loop.kind
    of nnkAsgn:
      doAssert loop[0].kind == nnkBracketExpr and
        loop[0][0].kind == nnkIdent and loop[0][0].strVal == "r" and
        loop[0][1].kind == nnkIdent and loop[0][1].strVal == "idx"
      ## TODO: make this prettier / fix this
    else:
      # turn this into an nnkAsgn node with `res` as LHS and `nnkAsgn` on RHS
      result = nnkAsgn.newTree(
        nnkBracketExpr.newTree(ident"res", ident"idx"),
        loop)
  else:
    proc replaceByElement(n: NimNode, preface: Preface): NimNode =
      ## recurses the node `n` and replaces all occurences by `t` for each
      ## tensor in the loop
      # first check if an ident that is in preface we have to replace or if
      # an `nnkBracketExpr` which contains an ident from `preface`. In those cases
      # return early
      case n.kind
      of nnkIdent, nnkSym:
        if n.strVal in preface: return preface[n.strVal].element
        else: return n
      of nnkBracketExpr:
        if n[0].kind == nnkIdent and n[0].strVal in preface:
          return preface[n[0].strVal].element
      else: result = n
      if n.len > 0:
        result = newTree(n.kind)
        for ch in n:
          result.add replaceByElement(ch, preface)

    let loop = loopStmts[0].replaceByElement(preface)
    echo loop.treeRepr
    case loop.kind
    of nnkAsgn: doAssert loop[0].kind == nnkIdent and loop[0].strVal == "r"
    else:
      # turn this into an nnkAsgn node with `res` as LHS and `nnkAsgn` on RHS
      result = nnkAsgn.newTree(ident"r", loop)

proc convertLoop(p: Preface, dtype, loop: NimNode): NimNode =
  let idx = ident"idx"
  let df = ident"df"
  let memCopyable = ["float", "int", "bool"]
  let isMemCopyable = dtype.strVal in memCopyable and
    p.args.allIt(it.colType.strVal in memCopyable)
  if not isMemCopyable:
    let loopIndexed = fixupTensorIndices(loop, p, toElements = true)
    echo "1 ", loopIndexed.repr
    result = quote do:
      for `idx` in 0 ..< `df`.len:
        `loopIndexed`
  else:
    let loopElements = fixupTensorIndices(loop, p, toElements = false)
    result = nnkCommand.newTree(ident"forEach")
    let resId = ident"res"
    let rId = ident"r"
    result.add nnkInfix.newTree(ident"in", rId, resId)
    for arg in p.args:
      result.add nnkInfix.newTree(ident"in", arg.element, arg.tensor)
    result.add nnkStmtList.newTree(loopElements)

proc compileFormula(stmts: NimNode): NimNode =
  let preface = parsePreface(extractCall(stmts, "preface"))
  ## TODO: if `dtype` not given: auto determine
  let dtype = parseSingle(extractCall(stmts, "dtype"))
  ## TODO: if `name` not given, use formulaNameMacro
  let name = parseSingle(extractCall(stmts, "name"))
  let loop = parseLoop(extractCall(stmts, "loop"))

  var procBody = newStmtList()
  procBody.add convertPreface(preface)
  procBody.add convertDtype(dtype)
  procBody.add convertLoop(preface, dtype, loop)
  let resultId = ident"result"
  let resId = ident"res"
  procBody.add quote do:
    `resultId` = toColumn `resId`
  result = procBody
  let params = [ident"Column",
                nnkIdentDefs.newTree(ident"df",
                                     ident"DataFrame",
                                     newEmptyNode())]
  result = newProc(newEmptyNode(),
                   params = params,
                   body = procBody,
                   procType = nnkLambda)

  echo result.repr


macro fn*(y: untyped): untyped =
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
  result = compileFormula(y)

import math
import arraymancer / laser / strided_iteration / foreach
let f1 = fn:
  preface:
    t in df["foo", int] # t refers to each element of `foo` in the loop
    u in df["bar", float]
    v = df["baz", int] # v refers to the ``Tensor`` `baz`
  dtype: float
  name: "fooBar"
  loop:
    t.float * u + v[idx].float

let f2 = fn:
  preface:
    t in df["foo", int] # t refers to each element of `foo` in the loop
    u in df["bar", float]
    v = df["baz", int] # v refers to the ``Tensor`` `baz`
    #r in result
  dtype: bool
  name: "filterme"
  loop:
    t.float > u and v[idx].float < 2.2

let f3 = fn:
  preface:
    t in df["foo", float] # t refers to each element of `foo` in the loop
  dtype: bool
  name: "noNan"
  loop:
    not (classify(t) == fcNan)
