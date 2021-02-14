import value, column
import macros

#type
#  FormulaKind = enum
#    funcVector, funcScalar
#
#  FormulaNode* = object
#    name*: string
#    case kind*: FormulaKind
#    of funcVector:
#      fnV: proc(df: DataFrame): Column
#      #case dtKindV*: ColKind
#      #of colFloat: fnFloatV*: proc(df: DataFrame): float
#      #of colInt: fnIntV*: proc(df: DataFrame): int
#      #of colString: fnStringV*: proc(df: DataFrame): string
#      #of colBool: fnBoolV*: proc(df: DataFrame): bool
#      #of colObject: fnValueV*: proc(df: DataFrame): Value
#    of funcScalar:
#      fnS*: proc(c: Column): Column
#      #case dtKindS: ColKind
#      #of cFloat: fnFloatS: proc(x: float): float
#      #of cInt: fnIntS: proc(x: int): int
#      #of cString: fnStringS: proc(x: string): string
#      #of cBool: fnBoolS: proc(x: bool): bool
#      #of cObject: fnValueS: proc(x: Value): Value
type
  FormulaKind = enum
    fkTerm, fkVariable
  FormulaName* = ref FormulaNameObj
  FormulaNameObj = object
    # FormulaNode is either a Term, meaning it has a LHS and RHS
    # or a variable. The operator (function) is given as an enum for
    # the Term connecting the two sides
    case kind*: FormulaKind
    of fkTerm:
      lhs*: FormulaName
      rhs*: FormulaName
      op*: string
    of fkVariable:
      val*: string

  #let lhs = buildFormula(node[1])
  #let rhs = buildFormula(node[2])

func initTerm*(lhs: FormulaName, rhs: FormulaName,
               op: string): FormulaName =
  result = FormulaName(kind: fkTerm,
                       lhs: lhs,
                       rhs: rhs,
                       op: op)

func initVariable*[T: Value | string](val: T): FormulaName =
  result = FormulaName(kind: fkVariable,
                       val: $val)

proc toUgly*(result: var string, node: FormulaName) =
  var comma = false
  case node.kind:
  of fkTerm:
    result.add "(" & $node.op
    var lhs = ""
    lhs.toUgly(node.lhs)
    if lhs.len > 0:
      result.add " " & lhs
    var rhs = ""
    rhs.toUgly(node.rhs)
    if rhs.len > 0:
      result.add " " & rhs
    result.add ")"
  of fkVariable:
    result.add $node.val

proc `$`*(node: FormulaName): string =
  ## Converts `node` to its string representation
  result = newStringOfCap(1024)
  toUgly(result, node)

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
  of nnkStrLit, nnkRStrLit:
    val = ident(n.strVal)
  of nnkCallStrLit:
    val = ident(n[1].strVal)
  of nnkIntLit .. nnkFloat64Lit:
    val = n
  of nnkDotExpr, nnkBracketExpr:
    # probably field access of some object
    val = n
  of nnkPrefix:
    val = n
  of nnkCurly:
    val = n
  else:
    error("Unsupported kind to construct variable " & $n.kind)
  let name = val.toStrLit
  result = quote do:
    FormulaName(kind: fkVariable, val: `name`)

proc buildFormula*(n: NimNode): NimNode
proc constructFunction*(n: NimNode): NimNode =
  let fn = buildFormula(n[0])
  let arg = buildFormula(n[1])
  let rhs = buildFormula(newLit "")
  result = quote do:
    initTerm(lhs = `arg`,
             rhs = `rhs`,
             op = $`fn`)

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

proc handleInfix(n: NimNode): NimNode =
  ## Builds the formula given by `f{}`
  ## If it is infix, a `fkTerm` is created. If it's a literal a `fkVariable` is
  ## created.
  expectKind(n, nnkInfix)
  let tilde = recurseFind(n,
                          cond = ident"~")
  var node = n
  if tilde.kind != nnkNilLit and n[0] != ident"~":
    # only reorder the tree, if it does contain a tilde and the
    # tree is not already ordered (i.e. nnkInfix at top with tilde as
    # LHS)
    let replaced = reorderRawTilde(n, tilde)
    let full = nnkInfix.newTree(tilde[0],
                                tilde[1],
                                replaced)
    node = full

  let opid = node[0].repr
  let lhs = buildFormula(node[1])
  let rhs = buildFormula(node[2])
  result = quote do:
    initTerm(lhs = `lhs`, rhs = `rhs`, op = `opid`)

proc handlePrefix(n: NimNode): NimNode =
  if n[0].eqIdent(ident"-"):
    case n[1].kind
    of nnkStrLit, nnkRStrLit:
      let str = "-" & n[1].strVal
      result = quote do:
        initVariable(`str`)
    of nnkCallStrLit:
      let str = "-" & n[1][1].strVal
      result = quote do:
        initVariable(`str`)
    of nnkIntLit .. nnkFloat64Lit:
      result = constructVariable(n)
    else:
      raise newException(Exception, "Not implemented `nnkPrefix` for " & $n[1].kind)
  else:
    result = quote do:
      initVariable(`n`)

import strutils
proc buildFormula*(n: NimNode): NimNode =
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
    result = buildFormula(n[0])
  of nnkDotExpr, nnkBracketExpr:
    result = constructVariable(n)
  of nnkPrefix:
    result = handlePrefix(n)
  of nnkAccQuoted:
    result = constructVariable(n[0].toStrLit)
  of nnkCallStrLit:
    # do some hacky things
    let node = n[1].toStrLit.repr.unescape[2 .. ^2]
    result = constructVariable(ident(node))
  of nnkCurly:
    result = constructVariable(n)
  else:
    raise newException(Exception, "Not implemented! " & $n.kind)
  # echo "Result is ", result.repr, " for kind ", n.kind
