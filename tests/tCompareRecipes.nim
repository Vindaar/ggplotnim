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
    require runRecipes[1] == 0
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
                  "rBarPlotRotatedLabels.svg"]
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
    for i in 0 ..< files.len:
      check expected[i] == isnow[i]
