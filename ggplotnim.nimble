# Package

version       = "0.2.0"
author        = "Sebastian Schmidt"
description   = "A port of ggplot2 for Nim"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.0.0"
requires "https://github.com/Vindaar/seqmath >= 0.1.3"
requires "ginger >= 0.1.4"
requires "persvector >= 1.0.0"
requires "shell >= 0.2.2" # to run tCompareRecipes test

task test, "Run tests":
  exec "nim c -r tests/testDf.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r tests/test_issue2.nim"
  exec "nim c -r tests/test_issue20.nim"
  exec "nim c -r tests/tCompareRecipes.nim"


import ospaths, strutils, strformat
const
  pkgName = "ggplotnim"
  orgFile = "docs" / (pkgName & ".org")
  rstFile = "docs" / (pkgName & ".rst")
  rstFileAuto = "docs" / (pkgName & "_autogen.rst")

proc basename(f: string): string =
  let (dir, name, ext) = f.splitFile
  result = name

proc removePrefix(f, prefix: string): string =
  result = f
  result.removePrefix(prefix)

# doc generation inspired by `strfmt`
task docs, "Generate HTML docs using the Org file":
  # https://github.com/jgm/pandoc/issues/4749
  exec "pandoc " & orgFile & " -o " & rstFile
  var files: seq[string]
  template walk(path: string, outf: untyped): untyped {.dirty.} =
    for filePath in listFiles(path):
      if filePath.endsWith(".nim"):
        let outfile = outf
        exec &"nim doc {outfile} {filePath}"
        files.add outfile.removePrefix("-o:")
  walk("src", "-o:index.html")
  walk("src" / pkgName, &"-o:{filePath.basename}.html")
  mvFile rstFile, rstFileAuto
  for f in files:
    let fname = f.basename & ".html"
    mvFile fname, "docs/" & $fname

import shell
task recipes, "Generate and run all recipes":
  exec "ntangle recipes.org"
  # without using shell running all these commands sometimes makes
  # travis throw up and stall....
  shell:
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

task recipesPlots, "Generate the PNGs from all recipes":
  exec """for f in media/recipes/r*.pdf; do inkscape $f --export-png="${f/.pdf/.png}"; done"""
