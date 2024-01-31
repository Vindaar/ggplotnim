import std / [strutils, sugar, options, typetraits]
import parsetoml
import ./ggplot_types
from ginger import Font, FontSlant, TextAlignKind, Quantity, quant, Color, ukCentimeter

proc assignKey(f: var Font, argTup: seq[string]) =
  ## Assigns the key (arg 0) to the correct field using value (arg 1)
  doAssert argTup.len == 2
  case argTup[0]
  of "bold": f.bold = parseBool argTup[1]
  of "family": f.family = argTup[1]
  of "size": f.size = parseFloat argTup[1]
  of "slant": f.slant = parseEnum[FontSlant](argTup[1])
  of "color": f.color = toOptColor(argTup[1]).get
  of "alignKind": f.alignKind = parseEnum[TextAlignKind](argTup[1])

proc assignIdx(f: var Font, arg: string, idx: int) =
  ## Assigns the value of `arg` to the field index `idx` of the proc `ggplot_utils/font`
  ##
  ##   size: T = 12.0,
  ##   color = color(0.0, 0.0, 0.0), # color defined in chroma
  ##   family = "sans-serif",
  ##   alignKind = taCenter,
  ##   bold = false,
  ##   slant: FontSlant = fsNormal # defined in ginger.types
  case idx
  of 0: f.size = parseFloat arg
  of 1: f.color = toOptColor(arg).get
  of 2: f.family = arg
  of 3: f.alignKind = parseEnum[TextAlignKind](arg)
  of 4: f.bold = parseBool arg
  of 5: f.slant = parseEnum[FontSlant](arg)
  else: doAssert false, "Invalid index " & $idx & "!"

proc parseFont(fnt: string): Font =
  if not fnt.startsWith("font("):
    raise newException(ValueError, "Invalid font description in the TOML file. Field is " &
      fnt & ", but should be of type `font(<args>)`.")
  let args = fnt.dup(removePrefix("font(")).dup(removeSuffix(")")).split(",")
  var argIdx = 0
  for arg in args:
    if "=" in arg:
      # has a key
      result.assignKey(arg.split("="))
    else:
      result.assignIdx(arg, argIdx)
    inc argIdx

template getInnerOptionType(t: typed): untyped =
  ## Given an element of `Option[T]` 'returns' `T`
  genericParams(typeof(t)).get(0)

proc parseAnyEnum[T](s: string, dtype: typedesc[T]): T =
  result = parseEnum[T](s)

proc parseTheme*(fname: string, tab = "Theme"): Theme =
  ## Parses a `Theme` to use for the plot from the given TOML file.
  ##
  ## This is done by attempting to walk all theme fields of the `Theme` Nim object
  ## and checking for fields of the `tab` table in the TOML file for fields of the
  ## same name. If any exists, sets them in the return.
  let config = parseToml.parseFile(fname)
  for field, val in fieldPairs(result):
    if field in config[tab]:
      let cVal = config[tab][field]
      when typeof(val) is Option[SomeNumber]:
        val = some(getInnerOptionType(val)(cVal.getFloat))
      elif typeof(val) is Option[Font]:
        let fontStr = cVal.getStr
        # now parse the font
        val = some(parseFont(fontStr))
      elif typeof(val) is Option[enum]:
        val = some(parseAnyEnum(cVal.getStr, getInnerOptionType(val)))
      elif typeof(val) is Option[bool]:
        val = some(cVal.getBool)
      elif typeof(val) is Option[Color]:
        val = toOptColor(cVal.getStr)
      elif typeof(val) is Option[Quantity]: ## XXX: for now assume centimeters
        val = some(quant(cVal.getFloat, ukCentimeter))
      else:
        echo "[WARNING] Field ", field, " of type ", typeof(val), " is currently still unsupported!"
