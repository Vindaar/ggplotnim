# Package

version       = "0.2.17"
author        = "Sebastian Schmidt"
description   = "A port of ggplot2 for Nim"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.0.0"
requires "https://github.com/Vindaar/seqmath >= 0.1.7"
requires "ginger = 0.1.14"
requires "persvector >= 1.0.0"
requires "shell >= 0.2.2" # to run tCompareRecipes test

task testCI, "Run standard tests w/o cairo dependency":
  # This runs all tests suitable for a CI environment, which does not provide
  # cairo. Most tests are independent of cairo anyways
  exec "nim c -d:noCairo -r tests/testDf.nim"
  exec "nim c -d:noCairo -r tests/tests.nim"
  exec "nim c -d:noCairo -r tests/test_issue2.nim"
  exec "nim c -d:noCairo -r tests/test_issue20.nim"
  exec "nim c -d:noCairo -r tests/test_issue28.nim"

task test, "Run standard tests":
  exec "nim c -r tests/testDf.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r tests/test_issue2.nim"
  exec "nim c -r tests/test_issue20.nim"
  exec "nim c -r tests/test_issue28.nim"

task fulltest, "Run all tests, including recipe comparison (requires ntangle)":
  exec "nim c -r tests/testDf.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r tests/test_issue2.nim"
  exec "nim c -r tests/test_issue20.nim"
  exec "nim c -r tests/test_issue28.nim"
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

task recipes, "Generate and run all recipes":
  exec "ntangle recipes.org"
  exec "nim c -r recipes/runRecipes.nim"

task recipesPlots, "Generate the PNGs from all recipes":
  exec """for f in media/recipes/r*.pdf; do inkscape $f --export-png="${f/.pdf/.png}"; done"""
