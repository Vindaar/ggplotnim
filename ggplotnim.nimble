# Package

version       = "0.5.9"
author        = "Sebastian Schmidt"
description   = "A port of ggplot2 for Nim"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.0.0"
requires "https://github.com/Vindaar/seqmath >= 0.1.16"
requires "ginger == 0.4.1"
requires "datamancer >= 0.3.2"
requires "arraymancer >= 0.7.22"
requires "shell >= 0.4.3"
requires "webview"
requires "https://github.com/SciNim/scinim >= 0.1.0"

task testCI, "Run standard tests w/o cairo dependency":
  # This runs all tests suitable for a CI environment, which does not provide
  # cairo. Most tests are independent of cairo anyways
  when defined(windows):
    exec "nim c -d:noCairo -d:lapack=liblapack -r tests/tests.nim"
    exec "nim c -d:noCairo -d:lapack=liblapack -r tests/tvega.nim"
    exec "nim c -d:noCairo -d:lapack=liblapack -r tests/test_issue2.nim"
  else:
    exec "nim c -d:noCairo -r tests/tests.nim"
    exec "nim c -d:noCairo -r tests/tvega.nim"
    exec "nim c -d:noCairo -r tests/test_issue2.nim"


task test, "Run standard tests":
  when defined(windows):
    exec "nim c -r -d:lapack=liblapack tests/tests.nim"
    exec "nim c -r -d:lapack=liblapack tests/tvega.nim"
    exec "nim c -r -d:lapack=liblapack tests/test_issue2.nim"
  else:
    exec "nim c -r tests/tests.nim"
    exec "nim c -r tests/tvega.nim"
    exec "nim c -r tests/test_issue2.nim"

task fulltest, "Run all tests, including recipe comparison (requires ntangle)":
  when defined(windows):
    exec "nim c -r -d:lapack=liblapack tests/tests.nim"
    exec "nim c -r -d:lapack=liblapack tests/test_issue2.nim"
    exec "nim c -r -d:lapack=liblapack tests/tvega.nim"
    exec "nim c -r -d:lapack=liblapack tests/tCompareRecipes.nim"
  else:
    exec "nim c -r tests/tests.nim"
    exec "nim c -r tests/test_issue2.nim"
    exec "nim c -r tests/tvega.nim"
    exec "nim c -r tests/tCompareRecipes.nim"

# Run the following command to generate everything required to possibly make CI pass, i.e.
# to update all files
task generateAll, "Generate all output files (requires lualatex, inkscape)":
  #exec "ntangle recipes.org" # generate recipes
  #exec "nim c -r recipes/recipeFiles.nim" # to generate the `_json.nim` files
  #exec "nim c -r -d:nimLegacyRandomInitRand recipes/allRecipesJson.nim" # generate `.json` output files
  exec "nim c -r recipes/rTikZLandau.nim" # generate `.tex` file for Landau
  exec "lualatex --shell-escape media/recipes/rTikZLandau.tex"
  exec "convert rTikZLandau.pdf rTikZLandau.png"
  exec "inkscape --export-type=png --pdf-poppler rTikZLandau.pdf"
  exec "mv rTikZLandau.png media/recipes/"
  exec "rm rTikZLandau.pdf rTikZLandau.aux rTikZLandau.log"

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
  when defined(windows):
    # depend on existing `.nim` files in repo on windows then..
    exec "nim c -r -d:nimLegacyRandomInitRand -d:lapack=liblapack recipes/allRecipes.nim"
  else:
    exec "ntangle recipes.org"
    exec "nim c -r -d:nimLegacyRandomInitRand recipes/allRecipes.nim"


task recipesJson, "Generate and run all recipes with JSON output":
  when defined(windows):
    # depend on existing `.nim` files in repo on windows then..
    exec "nim c -r -d:lapack=liblapack recipes/recipeFiles.nim" # to generate the `_json.nim` files
    exec "nim c -r -d:lapack=liblapack -d:nimLegacyRandomInitRand recipes/allRecipesJson.nim"
  else:
    exec "ntangle recipes.org"
    exec "nim c -r recipes/recipeFiles.nim" # to generate the `_json.nim` files
    exec "nim c -r -d:nimLegacyRandomInitRand recipes/allRecipesJson.nim"

task generateJson, "Generate all `_json.nim` recipe files that output JSON instead of a plot":
  exec "nim c -r recipes/runRecipes.nim --json"

task recipesPlots, "Generate the PNGs from all recipes":
  exec """for f in media/recipes/r*.pdf; do inkscape $f --export-png="${f/.pdf/.png}"; done"""
