import macros, tables, sequtils, sets, algorithm, options, strutils
import value, column, df_types
# formulaNameMacro contains a macro and type based on the fallback `FormulaNode`,
# which is used to generate the names of each `FormulaNode` in lisp representation
import formulaNameMacro
export formulaNameMacro

import formulaExp
export formulaExp

import arraymancer / laser / strided_iteration / foreach
export foreach

type
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

  FormulaMismatchError* = object of CatchableError

type
  ## These are internal types used in the macro
  TypeKind = enum
    tkNone, tkExpression, tkProcedure

  PossibleTypes = object
    isGeneric: bool
    asgnKind: Option[AssignKind]
    case kind: TypeKind
    of tkExpression:
      types: seq[NimNode]
    of tkProcedure:
      ## procedure types encountered are separate
      procTypes: seq[ProcType]
    else: discard

  ProcType = object
    argId: int # argument number of the "input" type
    isGeneric: bool
    inputTypes: seq[NimNode] # types of all arguments
    resType: Option[NimNode]

proc add(p: var PossibleTypes, pt: ProcType) =
  doAssert p.kind in {tkProcedure, tkNone}
  if p.kind == tkNone:
    p = PossibleTypes(kind: tkProcedure)
  p.procTypes.add pt

proc add(p: var PossibleTypes, p2: PossibleTypes) =
  doAssert p.kind == p2.kind
  case p.kind
  of tkExpression: p.types.add p2.types
  of tkProcedure: p.procTypes.add p2.procTypes
  else: discard

func isColIdxCall(n: NimNode): bool =
  (n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal in ["idx", "col"])
func isColCall(n: NimNode): bool =
  (n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal == "col")
func isIdxCall(n: NimNode): bool =
  (n.kind == nnkCall and n[0].kind == nnkIdent and n[0].strVal == "idx")

proc isGeneric(n: NimNode): bool =
  ## given a node that represents a type, check if it's generic by checking
  ## if the symbol or bracket[symbol] is notin `Dtypes`
  case n.kind
  of nnkSym, nnkIdent: result = n.strVal notin DtypesAll
  of nnkBracketExpr: result = n[1].strVal notin DtypesAll
  of nnkEmpty: result = true # sort of generic...
  else: error("Invalid call to `isGeneric` for non-type like node " &
    $(n.treeRepr) & "!")

func isLiteral(n: NimNode): bool = n.kind in {nnkIntLit .. nnkFloat64Lit, nnkStrLit}

func isScalar(n: NimNode): bool = n.strVal in DtypesAll

proc reorderRawTilde(n: NimNode, tilde: NimNode): NimNode =
  ## a helper proc to reorder an nnkInfix tree according to the
  ## `~` contained in it, so that `~` is at the top tree.
  ## (the actual result is simply the tree reordered, but without
  ## the tilde. Reassembly must happen outside this proc)
  ##
  ## TODO: For a formula like:
  ##       let fn = fn2 { "Test" ~ max idx("a"), "hello", 5.5, someInt() }
  ## we reconstruct:
  ## Curly
  ##   Infix
  ##     Ident "~"
  ##     StrLit "Test"
  ##     Command
  ##       Ident "max"
  ##       Call
  ##         Ident "idx"
  ##         StrLit "a"
  ##   StrLit "hello"
  ##   FloatLit 5.5
  ##   Call
  ##     Ident "someInt"
  ## so the tilde is still nested one level too deep.
  ## However it seems to be rather an issue of `recurseFind`?
  result = copyNimTree(n)
  for i, ch in n:
    case ch.kind
    of nnkIdent, nnkStrLit, nnkIntLit .. nnkFloat64Lit, nnkPar, nnkCall, nnkCommand,
       nnkAccQuoted, nnkCallStrLit, nnkBracketExpr:
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

proc compileVectorFormula(fct: FormulaCT): NimNode =
  let fnClosure = generateClosure(fct)
  # given columns
  let rawName = newLit(fct.rawName)
  var colName = if fct.name.kind == nnkNilLit: rawName else: fct.name
  let dtype = fct.resType
  result = quote do:
    FormulaNode(name: `rawName`,
                colName: `colName`, kind: fkVector,
                resType: toColKind(type(`dtype`)),
                fnV: `fnClosure`)
  echo result.repr

proc compileScalarFormula(fct: FormulaCT): NimNode =
  let fnClosure = generateClosure(fct)
  let rawName = newLit(fct.rawName)
  let valName = if fct.name.kind == nnkNilLit: rawName else: fct.name
  let dtype = fct.resType
  result = quote do:
    FormulaNode(name: `rawName`,
                valName: `valName`, kind: fkScalar,
                valKind: toValKind(`dtype`),
                fnS: `fnClosure`)
  echo result.repr

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
    of nnkCallStrLit, nnkAccQuoted, nnkCall:
      # skip this node completely, don't traverse further, since it (might) represent
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

var TypedSymbols {.compileTime.}: Table[string, Table[string, NimNode]]
var Formulas {.compileTime.}: Table[string, FormulaCT]

macro addSymbols(tabName, nodeName: string, n: typed): untyped =
  let nStr = tabName.strVal
  let nodeStr = nodeName.strVal
  if nStr notin TypedSymbols:
    TypedSymbols[nStr] = initTable[string, NimNode]()
  TypedSymbols[nStr][nodeStr] = n

proc isPureTree(n: NimNode): bool
proc extractSymbols(n: NimNode): seq[NimNode] =
  if n.isPureTree:
    return @[n]
  case n.kind
  of nnkIdent, nnkSym:
    # take any identifier or symbol
    if n.strVal notin ["df", "idx"]: # these are reserved identifiers
      result.add n
  of nnkIntLit .. nnkFloat64Lit, nnkStrLit:
    result.add n
  of nnkBracketExpr:
    # iff this is a pure (no column access) node, add as symbol
    if n.isPureTree:
      result.add n
  of nnkDotExpr:
    ## If `DotExpr` consists only of Idents during the untyped pass,
    ## it's either field access or multiple calls taking no arguments.
    ## In that case we can just keep the chain and pass it to the typed
    ## macro. In case other things are contained (possibly `df[<...>]` or
    ## a regular call) take the individual fields.
    ## For something like `ms.trans` in ggplotnim (`trans` field of a scale)
    ## we need to pass `ms.trans` to typed macro!
    proc isAllIdent(n: NimNode): bool =
      result = true
      case n.kind
      of nnkIdent: discard
      of nnkDotExpr:
        if n[1].kind != nnkIdent: return false
        result = isAllIdent(n[0])
      else: return false
    let allIdent = isAllIdent(n)
    if allIdent:
      result.add n
    else:
      # add all identifiers found
      for ch in n:
        result.add extractSymbols(ch)
  of nnkCall:
    # check if it's a call of `idx(someCol)` or `col(someCol)`. Else recurse.
    if n.isColIdxCall():
      return
    for i in 0 ..< n.len:
      result.add extractSymbols(n[i])
  of nnkAccQuoted, nnkCallStrLit:
    # do not look at these, since they are untyped identifiers referring to
    # DF columns
    return
  else:
    for i in 0 ..< n.len:
      result.add extractSymbols(n[i])

const FloatSet = toHashSet(@["+", "-", "*", "/", "mod"])
const StringSet = toHashSet(@["&", "$"])
const BoolSet = toHashSet(@["and", "or", "xor", ">", "<", ">=", "<=", "==", "!=",
                            "true", "false", "in", "notin", "not"])

func inFloatSet(n: string): bool = n in FloatSet
func inStringSet(n: string): bool = n in StringSet
func inBoolSet(n: string): bool = n in BoolSet

proc determineHeuristicTypes(body: NimNode,
                             typeHint: TypeHint,
                             name: string): TypeHint =
  ## checks for certain ... to  determine both the probable
  ## data type for a computation and the `FormulaKind`
  doAssert body.len > 0, "Empty body unexpected in `determineFuncKind`!"
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
  let (isFloat, isString, isBool) = checkDtype(body, FloatSet, StringSet, BoolSet)
  var typ: TypeHint
  if isFloat:
    typ.inputType = some(ident"float")
    typ.resType = some(ident"float")
  if isString:
    # overrides float if it appears
    typ.inputType = some(ident"string")
    typ.resType = some(ident"string")
  if isBool:
    # overrides float and string if it appears
    if isString:
      typ.inputType = some(ident"string")
    elif isFloat:
      typ.inputType = some(ident"float")
    else:
      # is bool tensor
      typ.inputType = some(ident"bool")
    # result is definitely bool
    typ.resType = some(ident"bool")

  # apply typeHint if available (overrides above)
  if typeHint.inputType.isSome:
    let dtype = typeHint.inputType
    if isBool:
      # we don't override bool result type.
      # in cases like:
      # `f{int: x > 4}` the are sure of the result, apply to col only
      typ.inputType = dtype
    elif isFloat or isString:
      # override dtype, result still clear
      typ.inputType = dtype
    else:
      # set both
      typ.inputType = dtype
      typ.resType = dtype
  if typeHint.resType.isSome:
    # also assign result type. In this case override regardless of automatic
    # determination
    typ.resType = typeHint.resType
  if typ.inputType.isNone or typ.resType.isNone:
    # attempt via formula
    warning("Could not determine data types of tensors in formula:\n" &
      "  name: " & $name & "\n" &
      "  formula: " & $body.repr & "\n" &
      "  data type: " & $typ.inputType.repr & "\n" &
      "  output data type: " & $typ.resType.repr & "\n" &
      "Consider giving type hints via: `f{T -> U: <theFormula>}`")
  result = typ

proc removeAll(s: string, chars: set[char]): string =
  result = newStringOfCap(s.len)
  for c in s:
    if c notin chars:
      result.add c
  if result.len == 0:
    result = "col"

proc genColSym(name, s: string): NimNode =
  ## custom symbol generation from `name` (may contain characters that are
  ## invalid Nim symbols) and `s`
  let toRemove = AllChars - IdentStartChars
  var res = removeAll(name, toRemove)
  res &= s
  result = ident(res)

proc addColRef(n: NimNode, typeHint: FormulaTypes, asgnKind: AssignKind): seq[Assign] =
  let (dtype, resType) = (typeHint.inputType, typeHint.resType)
  case n.kind
  of nnkAccQuoted:
    let name = n[0].strVal
    result.add Assign(asgnKind: asgnKind,
                      node: n,
                      element: ident(name & "Idx"),
                      tensor: ident(name),
                      col: newLit(name),
                      colType: dtype,
                      resType: resType)
  of nnkCallStrLit:
    # call str lit needs to be handled indendently, because it may contain
    # symbols that are invalid for a Nim identifier
    let name = buildName(n)
    let colName = genColSym(name, "T")
    let colIdxName = genColSym(name, "Idx")
    let nameCol = newLit(name)
    result.add Assign(asgnKind: asgnKind,
                      node: n,
                      element: colIdxName,
                      tensor: colName,
                      col: nameCol,
                      colType: dtype,
                      resType: resType)
  of nnkBracketExpr:
    if nodeIsDf(n):
      # `df["someCol"]`
      let name = n[1]
      let colName = genColSym(buildName(name), "T")
      let colIdxName = genColSym(buildName(name), "Idx")
      result.add Assign(asgnKind: byTensor,
                        node: n,
                        element: colIdxName,
                        tensor: colName,
                        col: n[1],
                        colType: dtype,
                        resType: resType)
    elif nodeIsDfIdx(n):
      # `df["someCol"][idx]`
      let name = n[0][1]
      let colName = genColSym(buildName(name), "T")
      let colIdxName = genColSym(buildName(name), "Idx")
      result.add Assign(asgnKind: byIndex,
                        node: n,
                        element: colIdxName,
                        tensor: colName,
                        col: n[0][1],
                        colType: dtype,
                        resType: resType)
    else:
      error("Invalid nnkBracketNode. Might contain a column reference, but " &
        "is not a raw colunm reference!")
  of nnkCall:
    # - `col(someCol)` referring to full column access
    # - `idx(someCol)` referring to column index access
    let name = buildName(n[1])
    let colName = genColSym(name, "T")
    let colIdxName = genColSym(name, "Idx")
    result.add Assign(asgnKind: asgnKind,
                      node: n,
                      element: colIdxName,
                      tensor: colName,
                      col: n[1],
                      colType: dtype,
                      resType: resType)
  else:
    discard

proc countArgs(n: NimNode): tuple[args, optArgs: int] =
  ## counts the number of arguments this procedure has as well
  ## as the number of default arguments
  ## Arguments are a `nnkFormalParams`. The first child node refers
  ## to the return type.
  ## After that follows a bunch of `nnkIdentDefs`, with typically
  ## 3 child nodes. However if we have a proc
  ## `proc foo(a, b: int): int`
  ## the formal params only have 2 child nodes and a `nnkIdentDefs` with
  ## 4 instead of 3 children (due to the `,`).
  ## An optional value is stored in the last node. If no optional parameters
  ## that node is empty.
  expectKind(n, nnkFormalParams)
  # skip the first return type node
  for idx in 1 ..< n.len:
    let ch = n[idx]
    let chLen = ch.len
    inc result.args, chLen - 2 # add len - 2, since 3 by default.
                               #Any more is same type arg
    if ch[ch.len - 1].kind != nnkEmpty:
      inc result.optArgs, chLen - 2

proc typeToAsgnKind(n: NimNode): AssignKind =
  ## NOTE: only use this function if you know that `n` represents a
  ## `type` and it is either `Tensor[T]` or `T` where `T` has to be
  ## in `Dtypes` (DF allowed types)
  case n.kind
  of nnkBracketExpr: result = byTensor
  of nnkIdent, nnkSym: result = byIndex
  else: error("Invalid call to `typeToAsgnKind` for non-type like node " &
    $(n.treeRepr) & "!")

func isTensorType(n: NimNode): bool =
  n[0].kind in {nnkSym, nnkIdent} and n[0].strVal == "Tensor"

proc typeAcceptableOrNone(n: NimNode): Option[NimNode] =
  ## Returns a type that either matches `Dtypes` (everything storable
  ## in a DF) or is a `Tensor[T]`. Returns that type.
  ## Otherwise returns an empty node.
  case n.kind
  of nnkIdent, nnkSym:
    if n.strVal in DtypesAll:
      result = some(n)
  of nnkBracketExpr:
    if isTensorType(n):
      result = some(n)
  of nnkCall:
    # sometimes the type shows up as something like (nnkCall (openSymChoice openArray T))
    doAssert n.len == 3
    if n[1].kind == nnkSym and n[1].strVal == "Tensor":
      result = some(n)
  of nnkRefTy, nnkPtrTy, nnkInfix: discard
  of nnkEmpty:
    # no return type
    discard
  else:
    error("Invalid type `" & $(n.treeRepr) & "`!")

import sugar
proc typeAcceptable(n: NimNode): bool =
  case n.kind
  of nnkIdent, nnkSym:
    let nStr = n.strVal
    if nStr in DtypesAll:
      result = true
    elif nStr.startsWith("Tensor") and
         nStr.dup(removePrefix("Tensor["))[0 ..< ^1] in DtypesAll:
      # stringified type `Tensor[int, float, ...]`. Check is a bit of a hack
      result = true
  of nnkBracketExpr:
    if n.isTensorType() and not n.isGeneric:
      result = true
  else: discard

proc determineTypeFromProc(n: NimNode, numArgs: int): Option[ProcType] =
  # check if args matches our args
  var res = ProcType()
  let params = if n.kind == nnkProcTy: n[0]
               else: n.params
  let (hasNumArgs, optArgs) = countArgs(params)
  if (hasNumArgs - numArgs) <= optArgs and numArgs <= hasNumArgs:
    res.isGeneric = (not (n.kind == nnkProcTy)) and n[2].kind != nnkEmpty
    res.resType = some(params[0])
    for idx in 1 ..< params.len:
      # skip index 0, cause that's the output type
      let pArg = params[idx]
      let numP = pArg.len - 2
      for j in 0 ..< pArg.len - 2:
        res.inputTypes.add pArg[pArg.len - 2]
    if res.resType.isSome or res.inputTypes.len > 0:
      result = some(res)

proc toType(n: NimNode): NimNode =
  case n.kind
  of nnkIntLit .. nnkUInt64Lit: result = ident "int"
  of nnkFloatLit .. nnkFloat128Lit: result = ident "float"
  of nnkStrLit: result = ident "string"
  of nnkIdent, nnkSym:
    if n.strVal in ["true", "false"]: result = ident "bool"
    elif n.strVal in DtypesAll: result = n
    else: error("Invalid type " & $n.repr & " of kind " & $n.kind)
  of nnkBracketExpr:
    if n.isTensorType():
      result = n
    elif n[0].kind in {nnkSym, nnkIdent} and n[0].strVal == "typeDesc":
      result = n[1]
  else:
    error("Invalid node " & $n.kind & " : " & n.repr)

proc maybeAddSpecialTypes(possibleTypes: var PossibleTypes, n: NimNode) =
  ## These don't appear as overloads sometimes?
  if n.kind in {nnkSym, nnkIdent}:
    if n.strVal in ["<", ">", ">=", "<=", "==", "!="]:
      for dtype in Dtypes:
        possibleTypes.add ProcType(inputTypes: @[ident(dtype),
                                                 ident(dtype)],
                                   isGeneric: false,
                                   resType: some(ident("bool")))

proc findType(n: NimNode, numArgs: int): PossibleTypes =
  ## This procedure tries to find type information about a given NimNode.
  ## It must be a symbol (or contain one) of some kind. It should not be used for
  ## literals, as they have fixed type information.
  ## NOTE: this may be changed in the future! Currently this is used to
  var possibleTypes = PossibleTypes()
  case n.kind
  of nnkIntLit .. nnkFloat64Lit, nnkStrLit:
    return PossibleTypes(isGeneric: false, kind: tkExpression, types: @[n.toType], asgnKind: some(byIndex))
  of nnkSym:
    ## TODO: chck if a node referring to our types
    if n.strVal in DtypesAll:
      return PossibleTypes(isGeneric: false, kind: tkExpression, types: @[n], asgnKind: some(byIndex))
    else:
      ## TODO: check if a proc by using `getImpl`
      let tImpl = n.getImpl
      case tImpl.kind
      of nnkProcDef, nnkFuncDef:
        let pt = determineTypeFromProc(tImpl, numArgs)
        if pt.isSome:
          possibleTypes.add pt.get
      of nnkTemplateDef:
        # cannot deduce from template
        return
      else:
        # should be a symbol of a pure tree. Should have a well defined type
        ## TODO: is this branch likely?
        ## Should happen, yes. E.g. just a variable defined in local scope?
        ##
        let typ = n.getType
        if typ.kind != nnkNilLit:
          return PossibleTypes(isGeneric: false, kind: tkExpression,
                               types: @[typ.toType], asgnKind: some(byIndex))
        warning("How did we stumble over " & $(n.treeRepr) & " with type " &
          $(tImpl.treeRepr))
        #return
  of nnkCheckedFieldExpr:
    let impl = n.getTypeImpl
    expectKind(impl, nnkProcTy)
    ## TODO: fix to actually use proc type!
    let inputType = impl[0][1][1]
    let resType = impl[0][0]
    let pt = ProcType(inputTypes: @[inputType], resType: some(resType))
    possibleTypes.add pt
  of nnkClosedSymChoice, nnkOpenSymChoice:
    for ch in n:
      let tImpl = ch.getImpl
      case tImpl.kind
      of nnkProcDef, nnkFuncDef:
        let pt = determineTypeFromProc(tImpl, numArgs)
        if pt.isSome:
          possibleTypes.add pt.get
      else:
        error("How did we stumble over " & $(ch.treeRepr) & " with type " &
          $(tImpl.treeRepr))
  else:
    ## Else we deal with a pure node from which we can extract the type
    ## TODO: Clarify what this is for better. Write down somewhere that we try to
    ## determine types up to pure nodes, which means we may end up with arbitrary
    ## nim nodes that have a type
    let typ = n.getTypeInst
    case typ.kind
    of nnkProcDef, nnkFuncDef, nnkProcTy:
      let pt = determineTypeFromProc(typ, numArgs)
      if pt.isSome:
        possibleTypes.add pt.get
    else:
      if typ.toType.kind != nnkNilLit:
        return PossibleTypes(isGeneric: false, kind: tkExpression,
                             types: @[typ.toType], asgnKind: some(byIndex))

  ## possibly add special types
  possibleTypes.maybeAddSpecialTypes(n)
  result = possibleTypes

proc isPureTree(n: NimNode): bool =
  ## checks if this node tree is a "pure" tree. That means it does ``not``
  ## contain any column references
  result = true
  if n.nodeIsDf or n.nodeIsDfIdx:
    return false
  for ch in n:
    result = isPureTree(ch)
    if not result:
      return

proc getTypeIfPureTree(tab: Table[string, NimNode], n: NimNode, numArgs: int): PossibleTypes =
  let lSym = buildName(n)
  if n.isPureTree and lSym in tab:
    let nSym = tab[lSym]
    result = findType(nSym, numArgs = numArgs)
  else:
    ## "hack" to get the correct type of the last node in case this is impure.
    ## Needed in some cases like dotExpressions from an infix so that we know
    ## the result of the full expression
    if n.len > 0:
      result = tab.getTypeIfPureTree(n[^1], numArgs)

  ## TODO: there are cases where one can extract type information from impure trees.
  ## `idx("a").float` is a nnkDotExpr that is impure, but let's us know that the output
  ## type will be `float`.
  ## The thing is this information will appear on the next recursion anyway. But for
  ## certain cases (e.g. infix before such a impure dotExpr) we could use the information
  ## already.

proc typesMatch(n, m: NimNode): bool =
  result = if n.kind in {nnkSym, nnkIdent, nnkBracketExpr} and
              m.kind in {nnkSym, nnkIdent, nnkBracketExpr}:
             n.repr == m.repr
           else: false

proc toTypeSet(s: seq[NimNode]): HashSet[string] =
  result = initHashSet[string]()
  for x in s:
    result.incl x.repr

proc toTypeSet(p: seq[ProcType], inputs: bool): HashSet[string] =
  result = initHashSet[string]()
  for pt in p:
    if inputs:
      for it in pt.inputTypes:
        result.incl it.repr
    else:
      if pt.resType.isSome:
        result.incl pt.resType.get.repr

proc toTypeSet(p: PossibleTypes, inputs: bool): HashSet[string] =
  case p.kind
  of tkExpression:
    result = p.types.toTypeSet
  of tkProcedure:
    result = p.procTypes.toTypeSet(inputs = inputs)
  else:
    discard

proc matchingTypes(t, u: PossibleTypes): seq[NimNode] =
  ## Checks if the types match.
  ## If considering a chain of expressions, `t` is considered on the LHS so that
  ## it's output will be the input of `u`.
  var ts: HashSet[string]
  var us: HashSet[string]
  if t.kind == tkExpression:
    ts = t.types.toTypeSet
  elif t.kind == tkProcedure:
    ts = t.procTypes.toTypeSet(false)
  if u.kind == tkExpression:
    us = u.types.toTypeSet
  elif u.kind == tkProcedure:
    us = u.procTypes.toTypeSet(true)
  if ts.card > 0 and us.card > 0:
    result = intersection(ts, us).toSeq.mapIt(ident(it))

proc assignType(heuristicType: FormulaTypes, types: seq[NimNode], resType = newEmptyNode()): FormulaTypes =
  if types.len == 1:
    result = FormulaTypes(inputType: types[0],
                          resType: heuristicType.resType)
    if result.resType.kind == nnkEmpty:
      result.resType = resType
  elif types.len > 1:
    ## TODO: apply heuristic rules to pick "most likely"? Done in `assignType` below
    result = heuristicType
  else:
    result = heuristicType

proc assignType(heuristicType: FormulaTypes, typ: PossibleTypes, arg = 0): FormulaTypes =
  case typ.kind
  of tkExpression:
    if typ.types.len > 0:
      # take the type with the highest priority as the input type
      let typs = typ.types.sortTypes()
      result = FormulaTypes(inputType: ident(typs[^1]),
                            resType: heuristicType.resType)
    else:
      result = heuristicType
  of tkProcedure:
    if typ.procTypes.len > 0:
      # access the `arg` (by default 0) argument to the command / ... to get its type
      # filter out all possible input types and use it
      var inTypes = newSeq[NimNode]()
      for el in typ.procTypes:
        if el.inputTypes.len > arg:
          inTypes.add el.inputTypes[arg]
      let inTypsSorted = inTypes.sortTypes()
      result = FormulaTypes(inputType: ident(inTypsSorted[^1]),
                            resType: heuristicType.resType)
      if result.resType.kind == nnkEmpty:
        # only use output type if not set and pick highest priority one
        var outTyps = newSeq[NimNode]()
        for el in typ.procTypes:
          if el.resType.isSome:
            outTyps.add el.resType.get
        let outTypsSorted = outTyps.sortTypes()
        if outTypsSorted.len > 0:
          result.resType = ident(outTypsSorted[^1])
    else:
      result = heuristicType
  else:
    result = heuristicType

proc toTypeSeq(t: PossibleTypes, inputs: bool): seq[NimNode] =
  case t.kind
  of tkExpression:
    result = t.types
  of tkProcedure:
    for pt in t.procTypes:
      if inputs:
        result.add pt.inputTypes
      elif pt.resType.isSome:
        result.add pt.resType.get
  else: discard

proc detNumArgs(n: NimNode): int =
  case n.kind
  of nnkCall, nnkCommand, nnkPrefix:
    # the `"command"` is 1 we need to subtract
    result = n.len - 1
  of nnkInfix:
    result = 2
  of nnkDotExpr:
    result = 1
  else:
    result = 1
    warning("What kind? " & $n.kind & " in node " & n.repr)

proc argsValid(pt: ProcType, args: seq[PossibleTypes], impureIdxs: seq[int]): bool =
  ## This procedure removes all `ProcTypes` from the input `pt` that do not match the given
  ## `args` (excluding the impure indices!)
  ## The `ProcTypes` need to contain all input types (even ones we do not support as DFs)
  ## and we simply check if the args (possibly not of allowed types in DF due to being local vars)
  ## match these types. If a mismatch is found, the entry is deleted.
  ## Note: The arguments may be procedures themselves. In that case we check if the output types
  ## of these procedures match the inputs.
  ## If something has multiple overloads in the args (shouldn't be possible *I think*), we simply
  ## check for `any` match.
  result = true
  if pt.inputTypes.len < args.len: return false
  for i, inArg in pt.inputTypes:
    if i in impureIdxs: continue
    let arg = args[i]
    case args[i].kind
    of tkExpression:
      var anyTyp = false
      for t in arg.types:
        if typesMatch(inArg, t):
          anyTyp = true
          break
      if not anyTyp:
        return false
    of tkProcedure:
      for argPt in arg.procTypes:
        var anyTyp = false
        if argPt.resType.isSome and inArg == argPt.resType.get:
          anyTyp = true
        if not anyTyp:
          return false
    else:
      discard

proc determineTypesImpl(n: NimNode, tab: Table[string, NimNode], heuristicType: FormulaTypes): seq[Assign] =
  ## This procedure tries to determine type information from the typed symbols stored in `TypedSymbols`,
  ## so that we can override the `heuristicType`, which was determined in a first pass. This is to then
  ## create `Assign` objects, which store the column / index references and give them the type information
  ## that is required. That way we can automatically determine types for certain operations. For example:
  ##
  ## .. code-block:: nim
  ##         ## ```
  ##    proc max(x: int, y: string, z: float, b: int)
  ##    f{ max(idx("a"), "hello", 5.5, someInt()) }
  ##
  ## will automatically determine that column `a` needs to be read as an integer due to the placement
  ## as first argument of the procedure `max`. `max` is chosen because it is a common overload.
  if n.isPureTree:
    return
  case n.kind
  of nnkCall, nnkCommand, nnkPrefix:
    if n.isColCall:
      result.add addColRef(n, heuristicType, byTensor)
    elif n.isIdxCall:
      result.add addColRef(n, heuristicType, byIndex)
    else:
      ## in this case is a regular call
      ## determine type information from the procedure / w/e
      var cmdTyp = tab.getTypeIfPureTree(n[0], detNumArgs(n))
      doAssert n[0].isPureTree, "If this wasn't a pure tree, it would be a col reference!"
      ## for each argument to the call / cmd get the type of the argument.
      ## find then find the procedure / cmd /... that satisfies all requirements
      ## e.g.
      ## ```
      ## proc max(x: int, y: string, z: float, b: int)
      ## f{ max(idx("a"), "hello", 5.5, someInt()) }
      ## ```
      ## needs to restrict to this specific `max` thanks to the arguments `y, z, b`
      ## Arguments are only looked at for their *output* type, because that is the input to
      ## the command / call / ...
      doAssert cmdTyp.kind == tkProcedure, "In a call the first argument needs to " &
        "have procedure signature, i.e. have an input and output type!"
      # first extract all possible types for the call/cmd/... arguments
      var impureIdxs = newSeq[int]()
      var chTyps = newSeq[PossibleTypes]()
      for i in 1 ..< n.len:
        chTyps.add tab.getTypeIfPureTree(n[i], detNumArgs(n[i]))
        if not n[i].isPureTree:
          # i - 1 is impure idx, because i == 0 is return type of procedure
          impureIdxs.add i - 1
      # remove all mismatching proc types
      var idx = 0
      while idx < cmdTyp.procTypes.len:
        let pt = cmdTyp.procTypes[idx]
        if not pt.argsValid(chTyps, impureIdxs):
          cmdTyp.procTypes.delete(idx)
          continue
        inc idx
      # can use the type for the impure argument
      for idx in impureIdxs:
        result.add determineTypesImpl(
          # idx + 1 because we shift it down by 1 when adding to `impureIdxs`
          n[idx + 1], tab,
          assignType(heuristicType,
                     cmdTyp,
                     arg = idx))
  of nnkAccQuoted, nnkCallStrLit, nnkBracketExpr:
    if n.nodeIsDf or n.nodeIsDfIdx:
      if n.nodeIsDf and not n.nodeIsDfIdx:
        result.add addColRef(n, heuristicType, byTensor)
      elif heuristicType.inputType.isColumnType():
        result.add addColRef(n, heuristicType, byTensor)
      else:
        result.add addColRef(n, heuristicType, byIndex)
    else:
      for ch in n:
        result.add determineTypesImpl(ch, tab, heuristicType)
    ## TODO: need to handle regular `nnkBracketExpr`. Can this appear? If pure we wouldn't be here
  of nnkInfix:
    let lSym = buildName(n[0])
    let nSym = tab[lSym]
    let typ1 = tab.getTypeIfPureTree(n[1], detNumArgs(n))
    let typ2 = tab.getTypeIfPureTree(n[2], detNumArgs(n))
    doAssert not (n[1].isPureTree and n[2].isPureTree), "Both infix subtrees cannot be pure. We wouldn't have entered"
    # infix has 2 arguments
    let infixType = nSym.findType(numArgs = detNumArgs(n))
    var reqType: NimNode
    let matching1 = matchingTypes(typ1, infixType)
    let matching2 = matchingTypes(typ2, infixType)
    ## Now check if there is only a single result type of the infix type choices. If so
    ## use that as the result (if none set already)
    let resType = infixType.toTypeSet(inputs = false).toSeq.sortTypes()
    ## TODO: think about applying similar logic to `nnkCall` here (i.e. don't use sets)
    if resType.len > 0:
      result.add determineTypesImpl(n[1], tab,
                                    assignType(heuristicType, matching2, resType = ident(resType[^1])))
      result.add determineTypesImpl(n[2], tab,
                                    assignType(heuristicType, matching1, resType = ident(resType[^1])))
    else:
      result.add determineTypesImpl(n[1], tab,
                                    assignType(heuristicType, matching2))
      result.add determineTypesImpl(n[2], tab,
                                    assignType(heuristicType, matching1))
  of nnkDotExpr:
    ## dot expression is similar to infix, but only has "one argument".
    ## index 0 is our "operator" and index 1 our "argument"
    ## In essence the impure node can never be the last node, right? That would mean something like
    ## `a.myCall().b.idx("a")`
    ## which does not really make sense.
    doAssert not (n[0].isPureTree and n[1].isPureTree), "Not both trees can be pure"
    let typ0 = tab.getTypeIfPureTree(n[0], detNumArgs(n))
    let typ1 = tab.getTypeIfPureTree(n[1], detNumArgs(n))
    doAssert n[1].isPureTree, "Impure tree as second child to `nnkDotExpr` does not make sense"
    # extract types from `typ1`. Since `typ1` receives `n[0]` as its input, we can use
    # it's type or input (argument 0 technically only!) as the type for the impure branch
    result.add determineTypesImpl(n[0], tab,
                                  assignType(heuristicType,
                                             typ1))
  #of nnkIfExpr:
  #  Could add `ifExpr` because there we know that result type is type of argument. But better to rely on
  #  type hints here (at least for now).
  else:
    for ch in n:
      result.add determineTypesImpl(ch, tab, heuristicType)


proc determineTypes(loop: NimNode, tab: Table[string, NimNode]): Preface =
  ## we give an empty node as return to fill it in a top down way. Result is
  ## determined at top level. We go down until we find a result type, if any.
  ## otherwise we will use the heuristics in the main code below.
  let args = determineTypesImpl(loop, tab, FormulaTypes(inputType: newEmptyNode(),
                                                        resType: newEmptyNode()))
  result = Preface(args: args)

proc parseOptionValue(n: NimNode): Option[FormulaKind] =
  ## parses the AST of a `FormulaKind` into an `Option[T]` at CT
  ## Note: shouldn't there be an easier way?...
  expectKind n, nnkObjConstr
  doAssert n[0][0].kind == nnkSym and n[0][0].strVal == "Option"
  doAssert n[0][1].kind == nnkSym and n[0][1].strVal == "FormulaKind"
  doAssert n.len == 3
  if n[2].kind == nnkExprColonExpr and n[2][1].kind == nnkSym:
    if n[2][1].strVal == "true":
      # parse the number
      doAssert n[1][1].kind == nnkCall
      doAssert n[1][1][0].kind == nnkSym and n[1][1][0].strVal == "FormulaKind"
      result = some(FormulaKind(n[1][1][1].intVal))
    else:
      result = none(FormulaKind)
  else:
    error("Bad input node " & $n.repr & " in `parseOptionValue`.")

macro compileFormulaImpl*(rawName: untyped,
                          funcKindAst: untyped): untyped =
  ## Second stage of formula macro. In a sense the "typed" stage (even if it's an untyped macro).
  ## Extracts the typed symbols from `TypedSymbols` CT table and uses it to determine possible
  ## types for column references.
  let funcKind = parseOptionValue(funcKindAst)
  var fct = Formulas[rawName.strVal]
  var typeTab = initTable[string, NimNode]()
  if rawName.strVal in TypedSymbols:
    typeTab = TypedSymbols[rawName.strVal]
  # generate the `preface`
  ## generating the preface is done by extracting all references to columns,
  ## using their names as `tensor` names (not element, since we in the general
  ## formula syntax can only refer to full columns)
  ## Explicit `df` usage (`df["col"][idx]` needs to be put into a temp variable,
  ## `genSym("col", nskVar)`)
  fct.preface = determineTypes(fct.loop, typeTab)
  # compute the `resType`
  # use heuristics to determine a possible input / output type
  ## TODO: the type hint isn't really needed here anymore, is it?
  var typ = determineHeuristicTypes(fct.loop, typeHint = fct.typeHint,
                                    name = fct.rawName)

  var resTypeFromSymbols = newEmptyNode()
  var allScalar = true
  var allAgree = true
  ## Modify the result type in case all Assigns agree on one type
  ## TODO: need to think the assignment of `resTypeFromSymbols` over again. Idea is simply to
  ## use the `Assign` result types in case they all agree. Make sure this is correct.

  for arg in mitems(fct.preface.args):
    if typ.inputType.isSome and not arg.colType.typeAcceptable:
      arg.colType = typ.inputType.get
    if typ.resType.isSome and not arg.resType.typeAcceptable:
      arg.resType = typ.resType.get
    if resTypeFromSymbols.kind == nnkEmpty:
      resTypeFromSymbols = arg.resType
    elif resTypeFromSymbols.repr != arg.resType.repr:
      allAgree = false
    if arg.asgnKind == byIndex:
      allScalar = false
    # apply user given type hints rigorously
    if fct.typeHint.inputType.isSome:
      arg.colType = fct.typeHint.inputType.get
    if fct.typeHint.resType.isSome:
      arg.resType = fct.typeHint.resType.get

    # check if any is `Empty` or column type not acceptable, if so error out at CT
    if arg.colType.kind == nnkEmpty or arg.resType.kind == nnkEmpty:
      error("Could not determine data types of tensors in formula:\n" &
        "  name: " & $fct.name & "\n" &
        "  formula: " & $fct.loop.repr & "\n" &
        "  data type: " & $arg.colType.repr & "\n" &
        "  output data type: " & $arg.resType.repr & "\n" &
        "Consider giving type hints via: `f{T -> U: <theFormula>}`")
    elif not arg.colType.typeAcceptable:
      error("Input type for column " & $arg.node.repr & " in formula " &
        $fct.rawName & " is ambiguous.\n" &
        "Column type determined to be: " & $arg.colType.repr & "\n" &
        "Consider giving type hints via: `f{T -> U: <theFormula>}` where " &
        "`T` is the input type (`U` might not be required).")

  fct.resType = if fct.typeHint.resType.isSome:
                  fct.typeHint.resType.get
                elif resTypeFromSymbols.kind != nnkEmpty and allAgree:
                  resTypeFromSymbols
                else: typ.resType.get
  # check if result type still not acceptable, if so error out
  if not fct.resType.typeAcceptable:
    error("Output type for formula " & $fct.rawName & " is ambiguous.\n" &
      "Result type determined to be: " & $fct.resType.repr & "\n" &
      "Consider giving type hints via: `f{T -> U: <theFormula>}` where" &
      "`U` is the output type (`T`, input type, is required as well).")

  # possibly overwrite funcKind
  if funcKind.isSome:
    ## raise error in case given function kind does not match what we expect
    let fnk = funcKind.get
    if allScalar and fnk != fkScalar:
      warning("Formula " & $fct.rawName & " has a mismatch between given formula " &
        "kind:\n\t`" & $fnk & "` (mapping)\nand automatically determined formula kind:\n\t" &
        "<< (reducing)\nPlease adjust the given kind to `<<`.")
    elif not allScalar and fnk == fkScalar:
      error("Formula " & $fct.rawName & " has a mismatch between given formula " &
        "kind:\n\t`" & $fnk & "` (reducing)\nand automatically determined formula kind:\n\t" &
        "`~` (mapping)\nPlease adjust the given kind to `~`.")
    # use the user given formula kind
    fct.funcKind = fnk
  else:
    fct.funcKind = if allScalar: fkScalar else: fkVector

  case fct.funcKind
  of fkVector: result = compileVectorFormula(fct)
  of fkScalar: result = compileScalarFormula(fct)
  else: error("Unreachable branch. `fkAssign` and `fkVariable` are already handled!")

proc parseTypeHint(n: var NimNode): TypeHint =
  ## extracts possible type hints from the node `T -> U: ...`
  case n.kind
  of nnkExprColonExpr:
    case n[0].kind
    of nnkIdent:
      # simple type hint for tensor input type
      result = TypeHint(inputType: some(n[0]))
      # resType is `None`
    of nnkInfix:
      doAssert n[0].len == 3
      doAssert eqIdent(n[0][0], ident"->")
      # type hint of tensor + result
      result = TypeHint(inputType: some(n[0][1]),
                        resType: some(n[0][2]))
    else: error("Unsupported type hint: " & $n[0].repr)
    n = copyNimTree(n[1])
  else: discard # no type hint

proc isPureFormula(n: NimNode): bool =
  ## Checks if the input tree `n` is a pure formula. A pure formula is any Nim AST
  ## that does not contain a column reference.
  result = true
  if n.len > 0:
    for ch in n:
      result = result and isPureFormula(ch)
      if not result:
        return result
  case n.kind
  of nnkAccQuoted, nnkCallStrLit: result = false
  of nnkBracketExpr:
    if nodeIsDf(n) or nodeIsDfIdx(n):
      result = false
  of nnkCall:
    if nodeIsDf(n) or nodeIsDfIdx(n):
      result = false
  else: discard

proc compileFormula(n: NimNode): NimNode =
  ## Preprocessing stage of formula macro. Performs preprocessing steps and extracts
  ## basic information:
  ## - possible type hints
  ## - possible AST reordering if `~` involved
  ## - extracts possible `~`, `<<`, `<-` symbols
  ## - generates result column name (from possible LHS)
  ## - generates formula name
  ## - extracts pure subtrees and adds them to `TypedSymbols` CT table
  ## - calls second stage `compileFormulaImpl`
  var isAssignment = false
  var isReduce = false
  var isVector = false
  # extract possible type hint
  var node = n
  let typeHint = parseTypeHint(node)
  let tilde = recurseFind(node,
                          cond = ident"~")
  var formulaName = ""
  var formulaRhs = newNilLit()
  if tilde.kind != nnkNilLit and node[0].ident != toNimIdent"~":
    # only reorder the tree, if it does contain a tilde and the
    # tree is not already ordered (i.e. nnkInfix at top with tilde as
    # LHS)
    let replaced = reorderRawTilde(node, tilde)
    formulaName = buildFormula(tilde[1])
    formulaRhs = replaced
    isVector = true
  elif tilde.kind != nnkNilLit:
    # already tilde at level 0: infix(~, arg1, arg2)
    formulaName = buildFormula(node[1])
    formulaRhs = node[2]
    isVector = true
  else:
    # no tilde in node
    # check for `<-` assignment
    if node.len > 0 and eqIdent(node[0], ident"<-"):
      formulaName = buildFormula(node[1])
      formulaRhs = node[2]
      isAssignment = true
    # check for `<<` reduction
    elif node.len > 0 and eqIdent(node[0], ident"<<"):
      formulaName = buildFormula(node[1])
      formulaRhs = node[2]
      isReduce = true
    else:
      formulaRhs = node

  let fnName = buildName(node)
  let rawName = newLit fnName
  if isPureFormula(formulaRhs):
    # simply output a pure formula node
    if not isAssignment:
      ## TODO: allow in formulaExp.nim
      result = quote do:
        FormulaNode(kind: fkVariable,
                    name: `rawName`,
                    val: %~ `formulaRhs`)
    else:
      ## TODO: allow in formulaExp.nim
      result = quote do:
        FormulaNode(kind: fkAssign,
                    name: `rawName`,
                    lhs: `formulaName`,
                    rhs: %~ `formulaRhs`)
  elif isAssignment:
    error("Assignment of unpure formulas (column reference in formula body) is " &
      "unsupported. Use a reducing `<<` or mapping `~` formula.")
  else:
    ## The `funcKind` here is the ``preliminary`` determination of our
    ## FunctionKind. It may be overwritten at a later time in case the
    ## formula does not use explicit `~`, `<<` or `<-`, i.e. `f{<someOperation>}`
    ## without LHS.
    ## We have 2 pieces of information to go by
    ## -`df["<someCol>"]` in the body, refers to an operation on an explicit column,
    ##   -> should imply `fkScalar` (and has to be an arg of a proc call)
    ## - type information of all symbols that are not column references, which
    ##   might be reducing operations (`mean(df["someCol"])` etc.).
    let funcKind = if isReduce: some(fkScalar)
                   elif isVector: some(fkVector)
                   else: none(FormulaKind)

    ## Generate a preliminary `FormulaCT` with the information we have so far
    var fct = FormulaCT()
    # assign the name
    fct.name = if formulaName.len == 0: newLit(fnName) else: newLit(formulaName)
    fct.rawName = fnName
    # assign the loop
    fct.loop = if formulaRhs.kind == nnkStmtList: formulaRhs
               else: newStmtList(formulaRhs)
    fct.typeHint = typeHint
    ## assign to global formula CT table
    Formulas[fct.rawName] = fct

    result = newStmtList()
    let syms = extractSymbols(formulaRhs)
    for s in syms:
      let sName = buildName(s)
      result.add quote do:
        addSymbols(`rawName`, `sName`, `s`)
    var cpCall = nnkCall.newTree(ident"compileFormulaImpl",
                                 rawName,
                                 newLit funcKind)
    result.add cpCall

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
  ## - `<type> -> <resType>: <actualFormula>`: full type for closure.
  ##   `<type>` is the dtype used for tensors, `<resType>` the resulting type
  ## - `df[<someIdent/Sym>]`: to access columns using identifiers / symbols
  ##   defined in the scope
  ## - `idx`: can be used to access the loop iteration index
  if x.strVal == "f":
    result = compileFormula(y)

macro `fn`*(x: untyped): untyped =
  let arg = if x.kind == nnkStmtList: x[0] else: x
  doAssert arg.kind in {nnkCurly, nnkTableConstr}
  result = compileFormula(arg[0])
