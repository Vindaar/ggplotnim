import macros, tables, sequtils, sets, algorithm
import value, column, df_types
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

var TypedSymbols {.compileTime.}: Table[string, seq[NimNode]]

macro addSymbols(name: string, n: typed): untyped =
  if name.repr notin TypedSymbols:
    TypedSymbols[name.repr] = newSeq[NimNode]()
  TypedSymbols[name.repr].add n

proc extractSymbols(n: NimNode): seq[NimNode] =
  case n.kind
  of nnkIdent, nnkSym:
    # take any identifier or symbol
    if n.strVal notin ["df", "idx"]: # these are reserved identifiers
      result.add n
  of nnkBracketExpr:
    # check if contains df[<something>], df[<something>][idx]
    if not ((n[0].kind == nnkIdent and n[0].strVal == "df") or
            (n[0].kind == nnkBracketExpr and
             n[0][0].kind == nnkIdent and n[0][0].strVal == "df" and
             n[1].kind == nnkIdent and n[1].strVal == "idx")):
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
  of nnkAccQuoted, nnkCallStrLit:
    # do not look at these, since they are untyped identifiers referring to
    # DF columns
    return
  else:
    for i in 0 ..< n.len:
      result.add extractSymbols(n[i])

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

  when false:
    for key, val in TypedSymbols:
      for ch in val:
        case ch.kind
        of nnkSym:
          echo ch.getImpl.treeRepr
        of nnkClosedSymChoice, nnkOpenSymChoice:
          for chch in ch:
            echo chch.getImpl.treeRepr
        else:
          echo "Found node of kind ", ch.kind, "?"
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
  echo typedCalls.repr
  echo rawName.repr
  let bools = (isAssignment, isVector, isReduce, isRaw, isInplace)

  result = newStmtList()
  let syms = extractSymbols(body)
  for s in syms:
    result.add quote do:
      addSymbols(`rawName`, `s`)

  var cpCall = nnkCall.newTree(ident"compileFormulaImpl")
  cpCall.add rawName
  cpCall.add name
  cpCall.add body
  cpCall.add newLit bools
  cpCall.add dtype
  cpCall.add resDtype
  cpCall.add newLit funcKind
  for tk in typedCalls:
    cpCall.add tk
  result.add cpCall
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
      formulaName = unwrapAccQuote(node[1])
      formulaRhs = node[2]
      isAssignment = true
    elif node.len > 0 and eqIdent(node[0], ident"<<"):
      formulaName = unwrapAccQuote(node[1])
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
