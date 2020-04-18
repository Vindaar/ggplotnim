import ggplotnim, unittest, strutils, os, sequtils, osproc, shell

#[
This test simply builds all recipes, runs them and compares the final image of
all recipes with the expected plots in `media/expected`

NOTE: this test depends on imagemagick, since it converts the PNG files to PPM!
]#

suite "Compare recipe output":
  test "Compare recipe output":
    # first run recipes to make sure we have current recipe plots in
    # media/recipes which we can compare with media/expected
    let runRecipes = shellVerbose:
      nimble recipes
    let toContinue = runRecipes[1] == 0
    check toContinue
    if not toContinue:
      quit("Could not run recipes successfully, quitting recipe comparison")
    let files = @["rStackedMpgHistogram.png",
                  "rNewtonAcceleration.png",
                  "rMpgStackedPointPlot.png",
                  "rLinePlotSize.png",
                  "rMpgHistoBinWidth.png",
                  "rMpgContinuousColorPoints.png",
                  "rAxionMassVsDensity.png",
                  "rMpgHistoNumBins.png",
                  "rMpgHistoCustomBreaks.png",
                  "rMpgCustomColorPoint.png",
                  "rMpgHistoPlusPoints.png",
                  "rSimpleLinePlot.png",
                  "rMpgSimpleBarPlot.png",
                  "rTwoSensorsBadStyle.png",
                  "rTwoSensorsGoodStyle.png",
                  "rPrebinnedHisto.png",
                  "rMassAttenuationFunction.png",
                  "rAxionMassesLogLog.png",
                  "rStackedMpgFreqpoly.png",
                  "rMpgStackedBarPlot.png",
                  "rBarPlotRotatedLabels.png",
                  "rBarPlotCompStats.png",
                  #"rCustomAnnotations.png" # not compared, because it's too finicky
                  "rMpgDiscreteXScale.png",
                  "rDiscreteXLine.png",
                  "rEnlargeXRange.png",
                  "rLimitXRange.png",
                  "rCreateMarginBuffer.png",
                  "rHighlightMinMax.png",
                  "rFormulaAesthetic.png",
                  "rErrorBar.png",
                  "rDiscreteYAxis.png",
                  "rBothDiscreteAxes.png",
                  "rFreqPolyWithAlpha.png",
                  "rMultipleLegends.png",
                  "rSimpleTile.png",
                  #"rSimpleGeomText.png",
                  #"rClassifiedGeomText.png",
                  #"rAnnotateUsingGeomText.png",
                  #"rAnnotateMaxValues.png"]
                  "rAutoColoredNeuralSpikes.png",
                  "rCustomColoredNeuralSpikes.png",
                  "rNegativeBarPlot.png"]

    proc convertRead(path: string): seq[seq[string]] =
      for i, f in files:
        let pathF = path / $f
        check fileExists(pathF)
        let (_, _, fext) = pathF.splitFile
        let res = shellVerbose:
          convert ($pathF) ($pathF.replace(fext, ".ppm"))
        let data = readFile(pathF.replace(fext, ".ppm")).splitLines
        result.add data
    let expected = convertRead("media/expected")
    let isnow = convertRead("media/recipes")
    check isnow.len == expected.len
    template checkFiles(f1, f2, fname: untyped): untyped =
      # store in `comp` to avoid check obliterating our terminal with the diff
      let comp = f1 == f2
      check comp
      if not comp:
        echo "Comparison failed for file: ", fname
    for i in 0 ..< files.len:
      checkFiles(expected[i], isnow[i], files[i])
