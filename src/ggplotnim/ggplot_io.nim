import dataframe/[arraymancer_backend, value, column]

import memfiles, streams, strutils, tables, parsecsv, sequtils

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
  copyMem(buf[0].addr, data[colStart].addr, idx - colStart)
  buf.setLen(idx - colStart)

template parseHeaderCol(data: ptr UncheckedArray[char], buf: var string,
                        colNames: var seq[string],
                        idx, colStart): untyped =
  copyBuf(data, buf, idx, colStart)
  colNames.add buf

template guessType(data: ptr UncheckedArray[char], buf: var string,
                   colTypes: var seq[ColKind],
                   col, idx, colStart): untyped =
  copyBuf(data, buf, idx, colStart)
  if buf.isInt:
    colTypes[col] = colInt
  elif buf.isNumber:
    colTypes[col] = colFloat
  elif buf.isBool:
    colTypes[col] = colBool
  else:
    colTypes[col] = colString

template parseCol(data: ptr UncheckedArray[char], buf: var string, col: var Column,
                  colTypes: seq[ColKind], colIdx, idx, colStart, row: int): untyped =
  copyBuf(data, buf, idx, colStart)
  case colTypes[colIdx]
  of colInt:
    try:
      col.iCol[row] = parseInt buf
    except ValueError:
      try:
        # before we copy everything check if can be parsed to float, this branch will only
        # be called a single time
        let fVal = parseFloat buf
        col = toColumn col.iCol.asType(float)
        col.fCol[row] = fVal
        colTypes[colIdx] = colFloat
      except ValueError:
        # object column
        col = toObjectColumn col
        colTypes[colIdx] = colObject
        col.oCol[row] = %~ buf
  of colFloat:
    try:
      col.fCol[row] = parseFloat buf
    except ValueError:
      # object column
      col = toObjectColumn col
      colTypes[colIdx] = colObject
      col.oCol[row] = %~ buf
  of colBool:
    try:
      col.bCol[row] = parseBool buf
    except ValueError:
      # object column
      col = toObjectColumn col
      colTypes[colIdx] = colObject
      col.oCol[row] = %~ buf
  of colString: col.sCol[row] = buf
  of colObject: col.oCol[row] = %~ buf
  of colConstant: discard # already set
  of colNone: doAssert false, "Invalid column to parse into: `colNone`"

template parseLine(data: ptr UncheckedArray[char], buf: var string,
                   col, idx, colStart, row: var int,
                   toBreak: static bool,
                   fnToCall: untyped): untyped =
  if unlikely(data[idx] in {'\n', '\r', '\l'}):
    fnToCall
    inc row
    col = 0
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

proc readCsvTyped*(fname: string): DataFrame =
  let toSkip = {' '}
  const sep = ','
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
    parseLine(data, buf, col, idx, colStart, row, toBreak = true):
      parseHeaderCol(data, buf, colNames, idx, colStart)
  # 2. peek the first line to determine the data types
  var colTypes = newSeq[ColKind](colNames.len)
  var lastIdx = idx
  var lastColStart = colStart
  while idx < ff.size:
    parseLine(data, buf, col, idx, colStart, row, toBreak = true):
      guessType(data, buf, colTypes, col, idx, colStart)
  # 2a. revert the indices (make it a peek)
  idx = lastIdx
  dec row
  colStart = lastColStart
  # 3. create the starting columns
  var cols = newSeq[Column](colNames.len)
  for i in 0 ..< colTypes.len:
    cols[i] = newColumn(colTypes[i], lineCnt - 1) # -1 because of header
  # 4. parse the actual data
  doAssert row >= 0, "Parsing the header failed"
  while idx < ff.size:
    parseLine(data, buf, col, idx, colStart, row, toBreak = false):
      parseCol(data, buf, cols[col], colTypes, col, idx, colStart, row)
  for i, col in colNames:
    result[col] = cols[i]
  result.len = row

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
