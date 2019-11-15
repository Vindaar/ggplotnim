import ggplotnim, unittest, strutils, os, sequtils, osproc

#[
This test simply builds all recipes, runs them and compares the final image of
all recipes with the expected plots in `media/expected`
]#

suite "Compare recipe output":
  test "Compare recipe output":
    discard execCmdEx("nimble recipes")
    let files = @["rStackedMpgHistogram.pdf",
                  "rNewtonAcceleration.pdf",
                  "rMpgStackedPointPlot.pdf",
                  "rLinePlotSize.png",
                  "rMpgHistoBinWidth.pdf",
                  "rMpgContinuousColorPoints.pdf",
                  "rAxionMassVsDensity.pdf",
                  "rMpgHistoNumBins.pdf",
                  "rMpgHistoCustomBreaks.pdf",
                  "rMpgCustomColorPoint.pdf",
                  "rMpgHistoPlusPoints.pdf",
                  "rSimpleLinePlot.pdf",
                  "rMpgSimpleBarPlot.pdf",
                  "rTwoSensorsBadStyle.pdf",
                  "rTwoSensorsGoodStyle.pdf",
                  "rPrebinnedHisto.pdf",
                  "rMassAttenuationFunction.pdf",
                  "rAxionMassesLogLog.pdf",
                  "rStackedMpgFreqpoly.pdf",
                  "rMpgStackedBarPlot.pdf"]
    proc readFiles(path: string): seq[seq[string]] =
      for i, f in files:
        let data = readFile(path / $f).splitLines.filterIt("CreationDate" notin it)
        result.add data
    let expected = readFiles("media/expected")
    let isnow = readFiles("media/recipes")
    check isnow.len == expected.len
    for i in 0 ..< files.len:
      check expected[i] == isnow[i]
