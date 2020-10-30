import os, strutils
import shell

const RecipeFiles* = @["rStackedMpgHistogram",
                       "rNewtonAcceleration",
                       "rMpgStackedPointPlot",
                       "rLinePlotSize",
                       "rMpgHistoBinWidth",
                       "rMpgContinuousColorPoints",
                       "rAxionMassVsDensity",
                       "rMpgHistoNumBins",
                       "rMpgHistoCustomBreaks",
                       "rMpgCustomColorPoint",
                       "rMpgHistoPlusPoints",
                       "rSimpleLinePlot",
                       "rMpgSimpleBarPlot",
                       "rTwoSensorsBadStyle",
                       "rTwoSensorsGoodStyle",
                       "rPrebinnedHisto",
                       "rMassAttenuationFunction",
                       "rAxionMassesLogLog",
                       "rStackedMpgFreqpoly",
                       "rMpgStackedBarPlot",
                       "rBarPlotRotatedLabels",
                       "rBarPlotCompStats",
                       "rCustomAnnotations",
                       "rMpgDiscreteXScale",
                       "rDiscreteXLine",
                       "rEnlargeXRange",
                       "rLimitXRange",
                       "rCreateMarginBuffer",
                       "rHighlightMinMax",
                       "rFormulaAesthetic",
                       "rErrorBar",
                       "rDiscreteYAxis",
                       "rBothDiscreteAxes",
                       "rFreqPolyWithAlpha",
                       "rMultipleLegends",
                       "rSimpleTile",
                       "rSimpleGeomText",
                       "rClassifiedGeomText",
                       "rAnnotateUsingGeomText",
                       "rAnnotateMaxValues",
                       "rPeriodicTable",
                       "rAutoColoredNeuralSpikes",
                       "rCustomColoredNeuralSpikes",
                       "rNegativeBarPlot",
                       "rSimpleFacet",
                       "rFacetTpa",
                       "rFormatDatesPlot",
                       "rFormatDecimalsPlot",
                       "rWeightedHistogram",
                       "rPointInPolygons",
                       "rSimpleRaster",
                       "rFacetRaster",
                       "rCustomFill",
                       "rCustomMargins",
                       "rLongTitleMultiline"
                       ]

proc generateJsonFile*(f: string) =
  const tmpfile = "recipes/tmpfile.nim"
  discard existsOrCreateDir("resources/recipes")
  let fname = "recipes" / f
  # read the file
  let fcontent = readFile(fname & ".nim")
  # replace `ggsave` by `ggjson` and write file back
  const jsonImport = "from json import `%`\n"
  writeFile(tmpfile, jsonImport & fcontent.multiReplace(@[("ggsave(", "ggjson("),
                                                          ("media/", "resources/")]))
  # run the tmp file to generate json
  shell:
    nim c "-r" ($tmpfile)

when isMainModule:
  for f in RecipeFiles:
    generateJsonFile(f)
