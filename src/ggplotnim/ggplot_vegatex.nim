from ../ggplotnim import ggsave, GgPlot, VegaTeX, VegaDraw
import ggplot_vega
export ggplot_vega

from std / options import some

proc `+`*(p: GgPlot, d: VegaTeX) =
  ## Generates two plots. The TeX version and the Vega-Lite version.
  # 1. generate the TeX version
  p.ggsave(d.fname & ".tex", texOptions = d.texOptions)
  # 2. generate the Vega version
  let vegaDraw = VegaDraw(fname: d.fname & ".json", width: d.width,
                          height: d.height,
                          asPrettyJson: some(true))
  p + vegaDraw
