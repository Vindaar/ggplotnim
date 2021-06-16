import dataframe/[arraymancer_backend, value, column]

import memfiles, streams, strutils, tables, parsecsv, sequtils
# for `showBrowser`
import browsers, strformat, os

proc countLines(s: var FileStream): int =
  ## quickly counts the number of lines and then resets stream to beginning
  ## of file
  var buf = newString(500)
  while s.readLine(buf):
    inc result
  s.setPosition(0)

proc checkHeader(s: Stream, fname, header: string, colNames: seq[string]): bool =
  ## checks whether the given file contains the header `header`
  result = true
  if header.len > 0:
    var headerBuf: string
    if s.peekLine(headerBuf):
      result = headerBuf.startsWith(header)
    else:
      raise newException(IOError, "The input file " & $fname & " seems to be empty.")
  elif colNames.len > 0:
    # given some column names and a "header" without a symbol means we assume
    # there is no real header. If there is a real header in addition, user has
    # to use `skipLines = N` to skip it.
    result = false

proc readCsv*(s: Stream,
              sep = ',',
              header = "",
              skipLines = 0,
              colNames: seq[string] = @[],
              fname = "<unknown>"): OrderedTable[string, seq[string]] =
  ## returns a `Stream` with CSV like data as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  ## The `header` field can be used to designate the symbol used to
  ## differentiate the `header`. By default `#`.
  ## `colNames` can be used to provide custom names for the columns.
  ## If any are given and a header is present with a character indiciating
  ## the header, it is automatically skipped. ``However``, if custom names are
  ## desired and there is a real header without any starting symbol (i.e.
  ## `header.len == 0`), please use `skipLines = N` to skip it manually!
  # first check if the file even has a header of type `header`
  let hasHeader = checkHeader(s, fname, header, colNames)

  var parser: CsvParser
  open(parser, s, fname, separator = sep, skipInitialSpace = true)

  if colNames.len > 0:
    # if `colNames` available, use as header
    parser.headers = colNames
    if hasHeader:
      # and skip the real header
      discard parser.readRow()
  elif hasHeader:
    # read the header and use it
    parser.readHeaderRow()
  else:
    # file has no header nor user gave column names, raise
    raise newException(IOError, "Input neither has header starting with " &
      $header & " nor were column names provided!")

  result = initOrderedTable[string, seq[string]]()
  # filter out the header, delimiter, if any
  parser.headers.keepItIf(it != header)

  # possibly strip the headers and create the result table of columns
  var colHeaders: seq[string]
  for colUnstripped in items(parser.headers):
    let col = colUnstripped.strip
    colHeaders.add col
    result[col] = newSeqOfCap[string](5000) # start with a reasonable default cap

  # parse the actual file using the headers
  var lnCount = 0
  while readRow(parser):
    if lnCount < skipLines:
      inc lnCount
      continue
    for i, col in parser.headers:
      parser.rowEntry(col).removePrefix({' '})
      parser.rowEntry(col).removeSuffix({' '})
      result[colHeaders[i]].add parser.rowEntry(col)
  parser.close()

template copyBuf(data: ptr UncheckedArray[char], buf: var string,
                 idx, colStart: int): untyped =
  let nIdx = idx - colStart
  if nIdx > 0:
    buf = newString(nIdx)
    copyMem(buf[0].addr, data[colStart].addr, nIdx)
    buf.setLen(nIdx)

template parseHeaderCol(data: ptr UncheckedArray[char], buf: var string,
                        colNames: var seq[string],
                        header: string,
                        idx, colStart): untyped =
  copyBuf(data, buf, idx, colStart)
  if col == 0:
    if not buf.startsWith(header):
      raise newException(IOError, "Unexpected column name at column 0, missing " &
        "expected header `" & header & "`. Found " & buf)
    else:
      buf.removePrefix(header)
      # and remove possible whitespace
      buf = buf.strip
  colNames.add buf

template guessType(data: ptr UncheckedArray[char], buf: var string,
                   colTypes: var seq[ColKind],
                   col, idx, colStart, numCols: untyped): untyped =
  # only determine types for as many cols as in header
  if col < numCols:
    copyBuf(data, buf, idx, colStart)
    if buf.isInt:
      colTypes[col] = colInt
    elif buf.isNumber:
      colTypes[col] = colFloat
    elif buf.isBool:
      colTypes[col] = colBool
    else:
      colTypes[col] = colString

proc i64(c: char): int {.inline.} = int(ord(c) - ord('0'))

proc pow10(e: int): float {.inline.} =
  const p10 = [1e-22, 1e-21, 1e-20, 1e-19, 1e-18, 1e-17, 1e-16, 1e-15, 1e-14,
               1e-13, 1e-12, 1e-11, 1e-10, 1e-09, 1e-08, 1e-07, 1e-06, 1e-05,
               1e-4, 1e-3, 1e-2, 1e-1, 1.0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7,
               1e8, 1e9]                        # 4*64B cache lines = 32 slots
  if -22 <= e and e <= 9:
    return p10[e + 22]                          # common case=small table lookup
  result = 1.0
  var base = 10.0
  var e = e
  if e < 0:
    e = -e
    base = 0.1
  while e != 0:
    if (e and 1) != 0:
      result *= base
    e = e shr 1
    base *= base

type
  RetType = enum
    rtInt, rtFloat, rtNaN, rtError

proc parseNumber(data: ptr UncheckedArray[char],
                 sep: char, # if this sep is found parsing ends
                 idxIn: int,
                 intVal: var int, floatVal: var float): RetType {.inline, noInit.} =
  ## this code is taken and adapted from @c-blake's code in Nim PR #16055.
  # Parse/validate/classify all at once, returning the type we parsed into
  # and if not `rtError` the `intVal/floatVal` will store the parsed number
  const Sign = {'+', '-'} # NOTE: `parseFloat` can generalize this to INF/NAN.
  var idx = idxIn
  var noDot = false
  var exp = 0
  var p10 = 0
  var pnt = -1                                  # find '.' (point); do digits
  var nD = 0
  var giant = false
  intVal = 0                                    # build intVal up from zero..
  if data[idx] in Sign:
    idx.inc                                     # skip optional sign
  while data[idx] != '\0':                      # ..and track scale/pow10.
    if data[idx] notin Digits:
      if data[idx] != '.' or pnt >= 0:
        break                                   # a second '.' is forbidden
      pnt = nD                                  # save location of '.' (point)
      nD.dec                                    # undo loop's nD.inc
    elif nD < 18:                               # 2**63==9.2e18 => 18 digits ok
      intVal = 10 * intVal + data[idx].i64      # core ASCII->binary transform
    else:                                       # 20+ digits before decimal
      giant = true #XXX condition should be more precise than "18 digits"
      p10.inc                                   # any digit moves implicit '.'
    idx.inc
    nD.inc
  if data[idxIn] == '-':
    intVal = -intVal                            # adjust sign

  if pnt < 0:                                   # never saw '.'
    if nD == 0 and data[idx] == sep:            # empty field in CSV
      return rtNaN
    pnt = nD; noDot = true                      # so set to number of digits
  elif nD == 1:
    return rtError                              # ONLY "[+-]*\.*"

  if data[idx] notin {sep, '\n', '\r', '\l', 'e', 'E'}: ## TODO: generalize this?
    return rtError

  if data[idx] in {'E', 'e'}:                   # optional exponent
    idx.inc
    let i0 = idx
    if data[idx] in Sign:
      idx.inc                                   # skip optional sign
    while data[idx] in Digits:                  # build exponent
      exp = 10 * exp + data[idx].i64
      idx.inc
    if data[i0] == '-':
      exp = -exp                                # adjust sign
  elif noDot: # and intVal < (1'i64 shl 53'i64) ? # No '.' & No [Ee]xponent
    ## TODO: handle giant?
    #if giant:
    #  return rtError
    #  #copyBuf(data, strVal, idx, idxIn)
    return rtInt                                # mark as integer
  exp += pnt - nD + p10                         # combine explicit&implicit exp
  floatVal = intVal.float * pow10(exp)          # has round-off vs. 80-bit
  ## TODO: handle giant?
  #if giant:
  #  return rtError
  #  #copyBuf(data, strVal, idx, idxIn)
  result = rtFloat                                # mark as float

template parseCol(data: ptr UncheckedArray[char], buf: var string,
                  col: var Column,
                  sep: char,
                  colTypes: seq[ColKind], colIdx, idx, colStart, row, numCols: int,
                  intVal: var int, floatVal: var float, rtType: var RetType): untyped =
  ## if there are more `,` in a row than in the header, skip it
  if likely(colIdx < numCols):
    case colTypes[colIdx]
    of colInt:
      retType = parseNumber(data, sep, colStart, intVal, floatVal)
      case retType
      of rtInt: col.iCol[row] = intVal
      of rtFloat, rtNaN:
        # before we copy everything check if can be parsed to float, this branch will only
        # be called a single time
        col = toColumn col.iCol.asType(float)
        if retType != rtNaN:
          col.fCol[row] = floatVal
        else:
          col.fCol[row] = NaN
        colTypes[colIdx] = colFloat
      of rtError:
        # object column
        copyBuf(data, buf, idx, colStart)
        col = toObjectColumn col
        colTypes[colIdx] = colObject
        col.oCol[row] = %~ buf
    of colFloat:
      retType = parseNumber(data, sep, colStart, intVal, floatVal)
      case retType
      of rtInt: col.fCol[row] = intVal.float
      of rtFloat: col.fCol[row] = floatVal
      of rtNaN: col.fCol[row] = NaN
      of rtError:
        # object column
        copyBuf(data, buf, idx, colStart)
        col = toObjectColumn col
        colTypes[colIdx] = colObject
        col.oCol[row] = %~ buf
    of colBool:
      copyBuf(data, buf, idx, colStart)
      try:
        col.bCol[row] = parseBool buf
      except ValueError:
        # object column
        col = toObjectColumn col
        colTypes[colIdx] = colObject
        col.oCol[row] = %~ buf
    of colString:
      copyBuf(data, buf, idx, colStart)
      col.sCol[row] = buf
    of colObject:
      # try to parse as number
      retType = parseNumber(data, sep, colStart, intVal, floatVal)
      case retType
      of rtInt: col.oCol[row] = %~ intVal
      of rtFloat: col.oCol[row] = %~ floatVal
      of rtNaN: col.oCol[row] = Value(kind: VNull)
      of rtError:
        copyBuf(data, buf, idx, colStart)
        col.oCol[row] = %~ buf
    of colConstant: discard # already set
    of colNone:
      raise newException(IOError, "Invalid column type to parse into: `colNone`. " &
        "This shouldn't have happened! row = " & $row & ", col = " & $col)

template parseLine(data: ptr UncheckedArray[char], buf: var string,
                   sep: char,
                   col, idx, colStart, row: var int,
                   toBreak: static bool,
                   fnToCall: untyped): untyped =
  if unlikely(data[idx] in {'\n', '\r', '\l'}):
    fnToCall
    inc row
    col = 0
    if data[idx] == '\r' and data[idx + 1] == '\l':
      inc idx
    colStart = idx + 1
    inc idx
    when toBreak:
      break
  elif unlikely(data[idx] == sep):
    # convert last col to data
    fnToCall
    inc col
    colStart = idx + 1
  elif unlikely(data[idx] in toSkip):
    colStart = idx + 1
  else:
    discard
  inc idx

proc readCsvTyped*(fname: string,
                   sep: char = ',',
                   header: string = "",
                   skipLines = 0,
                   toSkip: set[char] = {}): DataFrame =
  ## Reads a DF from a CSV file using the separator character `sep`.
  ##
  ## `toSkip` can be used to skip optional characters that may be present
  ## in the data. For instance if a CSV file is separated by `,`, but contains
  ## additional whitespace (`5, 10, 8` instead of `5,10,8`) this can be
  ## parsed correctly by setting `toSkip = {' '}`.
  ##
  ## `header` designates the symbol that defines the header of the CSV file.
  ## By default it's empty meaning that the first line will be treated as
  ## the header. If a header is given, e.g. `"#"`, this means we will determine
  ## the column names from the first line (which has to start with `#`) and
  ## skip every line until the first line starting without `#`.
  ##
  ## `skipLines` is used to skip `N` number of lines at the beginning of the
  ## file.
  result = newDataFrame()
  var ff = memfiles.open(fname)
  var lineCnt = 0
  for slice in memSlices(ff):
    if slice.size > 0:
      inc lineCnt

  var
    idx = 0
    row = -1 # to skip header
    ## we're dealing with ASCII files, thus each byte can be interpreted as a char
    data = cast[ptr UncheckedArray[char]](ff.mem)
    col = 0
    colStart = 0
    buf = newString(80)

  # 1. first parse the header
  var colNames: seq[string]
  while idx < ff.size:
    parseLine(data, buf, sep, col, idx, colStart, row, toBreak = true):
      parseHeaderCol(data, buf, colNames, header, idx, colStart)

  # 1a. if `header` is set, skip all additional lines starting with header
  var cnt = 0 # column counter, used to determine skipped lines
  if header.len > 0:
    while idx < ff.size:
      parseLine(data, buf, sep, col, idx, colStart, row, toBreak = false):
        if col == 0 and data[colStart] != header[0]:
          break
        inc cnt
  let numCols = colNames.len
  # 1b. skip `skipLines`
  while idx < ff.size:
    parseLine(data, buf, sep, col, idx, colStart, row, toBreak = false):
      if cnt div numCols == skipLines:
        break
      inc cnt
  # reset row to 0
  row = 0
  # compute the number of skipped lines in total
  let skippedLines = cnt div numCols + 1 # + 1 for header

  # 2. peek the first line to determine the data types
  var colTypes = newSeq[ColKind](numCols)
  var lastIdx = idx
  var lastColStart = colStart
  while idx < ff.size:
    parseLine(data, buf, sep, col, idx, colStart, row, toBreak = true):
      guessType(data, buf, colTypes, col, idx, colStart, numCols)
  # 2a. revert the indices (make it a peek)
  idx = lastIdx
  dec row
  colStart = lastColStart
  # 3. create the starting columns
  var cols = newSeq[Column](numCols)
  let dataLines = lineCnt - skippedLines
  for i in 0 ..< colTypes.len:
    # create column of length:
    # lines in file - header - skipLines
    cols[i] = newColumn(colTypes[i], dataLines)
  # 4. parse the actual data
  doAssert row >= 0, "Parsing the header failed"
  var
    retType: RetType
    intVal: int
    floatVal: float
  while idx < ff.size:
    parseLine(data, buf, sep, col, idx, colStart, row, toBreak = false):
      parseCol(data, buf, cols[col], sep, colTypes, col, idx, colStart, row, numCols,
               intVal, floatVal, retType)
  for i, col in colNames:
    result[col] = cols[i]
  result.len = dataLines

  ff.close()

proc readCsv*(fname: string,
              sep = ',',
              header = "",
              skipLines = 0,
              colNames: seq[string] = @[]): OrderedTable[string, seq[string]] =
  ## returns a CSV file as a table of `header` keys vs. `seq[string]`
  ## values, where idx 0 corresponds to the first data value
  ## The `header` field can be used to designate the symbol used to
  ## differentiate the `header`. By default `#`.
  ## `colNames` can be used to provide custom names for the columns.
  ## If any are given and a header is present with a character indiciating
  ## the header, it is automatically skipped. ``However``, if custom names are
  ## desired and there is a real header without any starting symbol (i.e.
  ## `header.len == 0`), please use `skipLines = N` to skip it manually!
  var s = newFileStream(fname, fmRead)
  if s == nil:
    raise newException(IOError, "Input file " & $fname & " does not exist! " &
     "`readCsv` failed.")
  result = s.readCsv(sep, header, skipLines, colNames, fname = fname)
  s.close()

proc writeCsv*(df: DataFrame, filename: string, sep = ',', header = "",
               precision = 4) =
  ## writes a DataFrame to a "CSV" (separator can be changed) file.
  ## `sep` is the actual separator to be used. `header` indicates a potential
  ## symbol marking the header line, e.g. `#`
  var data = newStringOfCap(df.len * 8) # for some reserved space
  # add header symbol to first line
  data.add header
  let keys = getKeys(df)
  data.add join(keys, $sep) & "\n"
  var idx = 0
  for row in df:
    idx = 0
    for x in row:
      if idx > 0:
        data.add $sep
      data.add pretty(x, precision = precision)
      inc idx
    data.add "\n"
  writeFile(filename, data)

proc showBrowser*(df: DataFrame, toRemove = false) =
  ## Displays the given DataFrame as a table in the default browser.
  ##
  ## Note: the HTML generation is not written for speed at this time. For very large
  ## dataframes expect bad performance.
  const tmpl = """
<!DOCTYPE html>
<html>
<head>
<style>
table {
  font-family: arial, sans-serif;
  border-collapse: collapse;
  width: 100%;
}

td, th {
  border: 1px solid #dddddd;
  text-align: left;
  padding: 8px;
}

tr:nth-child(even) {
  background-color: #dddddd;
}
</style>
</head>
<body>

<table>
  $#
</table>

</body>
</html>
"""
  var
    header: string
    body: string
  header = "<thead>\n<tr>"
  for k in df.getKeys:
    header.add &"<th> {k} <br><br> {df[k].kind.toNimType} </th>"
  header.add "</tr>\n</thead>"
  body = "<tbody>"
  for row in df:
    body.add "<tr>\n"
    for x in row:
      body.add &"<td>{pretty(x)}</td>"
    body.add "\n</tr>"
  body.add "</tbody>"
  let fname = getTempDir() / "df.html"
  writeFile(fname, tmpl % [header & body])
  openDefaultBrowser(fname)
  if toRemove:
    # opening browsers may be slow, so wait a long time before we delete (file still needs to
    # be there when the browser is finally open. Thus default is to keep the file
    sleep(1000)
    removeFile(fname)
