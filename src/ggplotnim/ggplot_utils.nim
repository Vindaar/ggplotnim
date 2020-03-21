import math, options
import chroma, ginger

# TODO: move elsewhere!
func font*[T: SomeNumber](
  size: T = 12.0,
  color = color(0.0, 0.0, 0.0), # color defined in chroma
  family = "sans-serif",
  alignKind = taCenter,
  bold = false,
  slant: FontSlant = fsNormal # defined in ginger.types
    ): Font =
  result = Font(family: family,
                size: size,
                bold: bold,
                slant: slant,
                color: color,
                alignKind: alignKind)

template unwrap*[T](opt: Option[T], raiseIfNil = true): untyped =
  var tmp: T
  if isSome opt:
    tmp = get opt
  elif raiseIfNil:
    raise newException(Exception, "Option " & $opt & " must exist")
  tmp

proc calcRowsColumns*(rows, columns: int, nPlots: int): (int, int) =
  ## Calculates the desired rows and columns for # of `nPlots` given the user's
  ## input for `rows` and `columns`.
  ## - If no input is given, calculate the next possible rectangle of plots
  ##   that favors columns over rows.
  ## - If either row or column is 0, sets this dimension to 1
  ## - If either row or column is -1, calculate square of nPlots for rows / cols
  ## - If both row and column is -1 or either -1 and the other 0, default back
  ##   to the next possible square.
  if rows <= 0 and columns <= 0:
    # calc square of plots
    let sqPlt = sqrt(nPlots.float)
    result[1] = sqPlt.ceil.int
    result[0] = sqPlt.round.int
  elif rows == -1 and columns > 0:
    result[0] = (nPlots.float / columns.float).ceil.int
    result[1] = columns
  elif rows > 0 and columns == -1:
    result[0] = rows
    result[1] = (nPlots.float / rows.float).ceil.int
  elif rows == 0 and columns > 0:
    # 1 row, user desired # cols
    result = (1, columns)
  elif rows > 0 and columns == 0:
    # user desired # row, 1 col
    result = (rows, 1)
  else:
    result = (rows, columns)

when isMainModule:
  # test the calculation of rows and columns
  doAssert calcRowsColumns(2, 0, 4) == (2, 1)
  doAssert calcRowsColumns(0, 2, 4) == (1, 2)
  doAssert calcRowsColumns(7, 3, 1) == (7, 3)
  doAssert calcRowsColumns(0, 0, 1) == (1, 1)
  doAssert calcRowsColumns(0, 0, 2) == (1, 2)
  doAssert calcRowsColumns(0, 0, 3) == (2, 2)
  doAssert calcRowsColumns(0, 0, 4) == (2, 2)
  doAssert calcRowsColumns(0, 0, 5) == (2, 3)
  doAssert calcRowsColumns(0, 0, 6) == (2, 3)
  doAssert calcRowsColumns(0, 0, 7) == (3, 3)
  doAssert calcRowsColumns(0, 0, 8) == (3, 3)
  doAssert calcRowsColumns(0, 0, 9) == (3, 3)
  doAssert calcRowsColumns(-1, 2, 4) == (2, 2)
  doAssert calcRowsColumns(-1, 0, 4) == (2, 2)
  doAssert calcRowsColumns(2, -1, 4) == (2, 2)
