import shell, os, macros, times

const recipes = ["rStackedMpgHistogram",
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
                 "rAnnotatedHeatmap",
                 "rMultiSubplots",
                 "rPeriodicTable"]

macro genCommands(prefix: static string,
                  suffix: static string = ""): untyped =
  var cmds = newStmtList()
  for r in recipes:
    let cmd = prefix & r & suffix
    cmds.add quote do:
      `cmd`
  echo cmds.repr
  result = quote do:
    let res = shellVerbose:
      `cmds`
    if res[1] != 0:
      raise newException(Exception, "Failed to build or run at least one recipe!")

if paramCount() == 0:
  let t0 = epochTime()
  genCommands("nim c -r recipes/", ".nim")
  echo "Compilating and running all recipes took ", epochTime() - t0
if paramCount() > 0:
  let p0 = paramStr(1)
  if p0 == "-c" or p0 == "--compile":
    genCommands("nim c recipes/", ".nim")
  elif p0 == "-cd" or p0 == "--compileDanger":
    genCommands("nim c -d:danger recipes/", ".nim")
  elif p0 == "-r" or p0 == "--run":
    let t0 = epochTime()
    genCommands("./recipes/")
    echo "Running all recipes took ", epochTime() - t0
