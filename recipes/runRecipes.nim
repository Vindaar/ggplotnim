import shell

let res = shellVerbose:
  nim c "-r recipes/rStackedMpgHistogram.nim"
  nim c "-r recipes/rNewtonAcceleration.nim"
  nim c "-r recipes/rMpgStackedPointPlot.nim"
  nim c "-r recipes/rLinePlotSize.nim"
  nim c "-r recipes/rMpgHistoBinWidth.nim"
  nim c "-r recipes/rMpgContinuousColorPoints.nim"
  nim c "-r recipes/rAxionMassVsDensity.nim"
  nim c "-r recipes/rMpgHistoNumBins.nim"
  nim c "-r recipes/rMpgHistoCustomBreaks.nim"
  nim c "-r recipes/rMpgCustomColorPoint.nim"
  nim c "-r recipes/rMpgHistoPlusPoints.nim"
  nim c "-r recipes/rSimpleLinePlot.nim"
  nim c "-r recipes/rMpgSimpleBarPlot.nim"
  nim c "-r recipes/rTwoSensorsBadStyle.nim"
  nim c "-r recipes/rTwoSensorsGoodStyle.nim"
  nim c "-r recipes/rPrebinnedHisto.nim"
  nim c "-r recipes/rMassAttenuationFunction.nim"
  nim c "-r recipes/rAxionMassesLogLog.nim"
  nim c "-r recipes/rStackedMpgFreqpoly.nim"
  nim c "-r recipes/rMpgStackedBarPlot.nim"
  nim c "-r recipes/rBarPlotRotatedLabels.nim"
if res[1] != 0:
  raise newException(Exception, "Failed to build or run at least one recipe!")
