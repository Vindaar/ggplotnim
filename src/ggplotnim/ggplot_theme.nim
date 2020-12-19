import options
import ginger
import ggplot_types

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
    # default background: transparent
    result.fillColor = transparent
