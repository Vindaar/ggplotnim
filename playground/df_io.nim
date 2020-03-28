import tables, strutils, streams, os, parsecsv, sequtils

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

proc readCsv2*(fname: string,
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
