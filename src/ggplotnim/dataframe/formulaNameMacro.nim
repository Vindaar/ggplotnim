import strformat, macros, strutils

proc build(n: NimNode): string
proc buildArgs(n: NimNode, head = ""): string =
  if result.len == 0 and head.len > 0:
    result = &"{head}"
  for i in 0 ..< n.len:
    if result.len == 0:
      result = &"({build(n[i])}"
    else:
      result.add &" {build(n[i])}"
  result.add ")"

proc build(n: NimNode): string =
  # convert to lisp representation
  case n.kind
  of nnkInfix:
    result = &"({n[0].strVal} {build(n[1])} {build(n[2])})"
  of nnkIntLit .. nnkFloat64Lit, nnkStrLit:
    result = n.repr
  of nnkIdent, nnkSym:
    # should correspond to a known identifier in the calling scope
    result = n.strVal
  of nnkPar, nnkCall:
    result = buildArgs(n)
  of nnkDotExpr, nnkBracketExpr:
    result = n.repr
  of nnkPrefix:
    result = &"({n[0].strVal} {build(n[1])})"
  of nnkAccQuoted:
    result = build(n[0])
  of nnkCallStrLit:
    result = n[1].strVal
  of nnkCurly:
    result = node
  of nnkIfExpr:
    result = "(if"
    for arg in n:
      result.add &" {build(arg)}"
    result.add ")"
  of nnkElifExpr:
    result = buildArgs(n, head = "(elif")
  of nnkElseExpr:
    result = buildArgs(n, head = "(else")
  of nnkStmtList:
    for ch in n:
      if result.len == 0 and n.len > 1:
        result = &"({buildArgs(ch)}"
      elif result.len == 0:
        result = &"{buildArgs(ch)}"
      else:
        result.add &" {buildArgs(ch)}"
    if n.len > 1:
      result.add ")"
  else:
    error("Node kind " & $n.kind & " not implemented " &
      "for FormulaNode string representation")

proc buildFormula*(n: NimNode): NimNode =
  result = newLit(build(n))
