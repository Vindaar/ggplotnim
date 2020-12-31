# Package

version       = "0.3.21"
author        = "Sebastian Schmidt"
description   = "A port of ggplot2 for Nim"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.0.0"
requires "https://github.com/Vindaar/seqmath >= 0.1.11"
requires "ginger >= 0.2.8"
requires "persvector >= 1.0.0"
requires "shell >= 0.4.3"
requires "arraymancer >= 0.6.2"
requires "webview"

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

import os, strutils, strformat
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

template canImport(x: untyped): untyped =
  compiles:
    import x

when canImport(docs / docs):
  # can define the `gen_docs` task (docs already imported now)
  # this is to hack around weird nimble + nimscript behavior.
  # when overwriting an install nimble will try to parse the generated
  # nimscript file and for some reason then it won't be able to import
  # the module (even if it's put into `src/`).
  task gen_docs, "Generate ggplotnim documentation":
    # build the actual docs and the index
    exec "pandoc " & orgFile & " -o " & rstFile
    buildDocs(
      "src/", "docs/",
      defaultFlags = "--hints:off --warnings:off"
    )

task recipes, "Generate and run all recipes":
  when not defined(windows):
    # depend on existing `.nim` files in repo on windows then..
    exec "ntangle recipes.org"
  exec "nim c -r recipes/allRecipes.nim"

task recipesJson, "Generate and run all recipes with JSON output":
  when not defined(windows):
    exec "ntangle recipes.org"
  exec "nim c -r recipes/recipeFiles.nim" # to generate the `_json.nim` files
  exec "nim c -r recipes/allRecipesJson.nim"

task generateJson, "Generate the JSON results for all recipes":
  exec "nim c -r recipes/runRecipes.nim --json"

task recipesPlots, "Generate the PNGs from all recipes":
  exec """for f in media/recipes/r*.pdf; do inkscape $f --export-png="${f/.pdf/.png}"; done"""
