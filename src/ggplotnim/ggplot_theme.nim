import options, sequtils
import ginger
import ggplot_types, ggplot_scales
import datamancer

proc getPlotBackground*(theme: Theme): Style =
  ## returns a suitable style (or applies default) for the background of
  ## the plot area
  result = Style(color: color(0.0, 0.0, 0.0, 0.0))
  if theme.plotBackgroundColor.isSome:
    result.fillColor = theme.plotBackgroundColor.unsafeGet
  else:
    # default color: `grey92`
    result.fillColor = grey92

proc getCanvasBackground*(theme: Theme): Style =
  ## returns a suitable style (or applies default) for the background color of
  ## the whole plot canvas. By default it is transparent
  result = Style(color: transparent)
  if theme.canvasColor.isSome:
    result.fillColor = theme.canvasColor.unsafeGet
  else:
    # default background: white
    result.fillColor = white

proc getGridLineColor*(theme: Theme): Style =
  ## returns a suitable style (or applies default) for the color of the grid lines
  result = Style(lineWidth: 1.0,
                 color: white,
                 lineType: ltSolid)
  if theme.gridLineColor.isSome:
    result.color = theme.gridLineColor.unsafeGet

proc calculateMarginRange*(theme: Theme, scale: ginger.Scale, axKind: AxisKind): ginger.Scale =
  var margin: float
  case axKind
  of akX: margin = if theme.xMargin.isSome: theme.xMargin.unsafeGet else: 0.0
  of akY: margin = if theme.yMargin.isSome: theme.yMargin.unsafeGet else: 0.0
  let diff = scale.high - scale.low
  result = (low: scale.low - diff * margin,
            high: scale.high + diff * margin)

func hasSecondary*(theme: Theme, axKind: AxisKind): bool =
  case axKind
  of akX:
    if theme.xLabelSecondary.isSome:
      result = true
  of akY:
    if theme.yLabelSecondary.isSome:
      result = true

func labelName(filledScales: FilledScales, p: GgPlot, axKind: AxisKind): string =
  ## extracts the correct label for the given axis.
  ## First checks whether the theme sets a name, then checks the name of the
  ## x / y `Scale` and finally defaults to the column name.
  # doAssert p.aes.x.isSome, "x scale should exist?"
  case axKind
  of akX:
    let xScale = getXScale(filledScales)
    if xScale.name.len > 0:
      result = xScale.name
    else:
      result = $xScale.col
  of akY:
    let yScale = getYScale(filledScales)
    if yScale.name.len > 0:
      result = yScale.name
    elif yScale.col.name.len > 0:
      result = $yScale.col
    else:
      ## TODO: make this nicer by having a better approach to propagate
      ## the density information from geoms to here!
      result = if filledScales.geoms.anyIt(it.geom.statKind == stBin and
                                           it.geom.density):
                 "density"
               else:
                 "count"

proc buildTheme*(filledScales: FilledScales, p: GgPlot): Theme =
  ## builds the final theme used for the plot. It takes the theme of the
  ## `GgPlot` object and fills in all missing fields as required from
  ## `filledScales` and `p`.
  result = p.theme
  if result.xLabel.isNone:
    result.xLabel = some(labelName(filledScales, p, akX))
  if result.yLabel.isNone:
    result.yLabel = some(labelName(filledScales, p, akY))
  if result.xLabelSecondary.isNone and filledScales.hasSecondary(akX):
    result.xLabelSecondary = some(filledScales.getSecondaryAxis(akX).name)
  if result.yLabelSecondary.isNone and filledScales.hasSecondary(akY):
    result.yLabelSecondary = some(filledScales.getSecondaryAxis(akY).name)

  # calculate `xMarginRange`, `yMarginRange` if any
  let xScale = if result.xRange.isSome: result.xRange.unsafeGet else: filledScales.xScale
  result.xMarginRange = result.calculateMarginRange(xScale, akX)
  let yScale = if result.yRange.isSome: result.yRange.unsafeGet else: filledScales.yScale
  result.yMarginRange = result.calculateMarginRange(yScale, akY)
