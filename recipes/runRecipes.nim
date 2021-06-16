import shell, os, macros, times, cligen

import recipeFiles

macro genCommands(prefix: static string,
                  suffix: static string = ""): untyped =
  var cmds = newStmtList()
  for r in RecipeFiles:
    let cmd = prefix & r & suffix
    cmds.add quote do:
      `cmd`
  echo cmds.repr
  result = quote do:
    let res = shellVerbose:
      `cmds`
    if res[1] != 0:
      raise newException(Exception, "Failed to build or run at least one recipe!")

template printWarning(): untyped =
  echo "WARNING: Using `runRecipes` to compile and run all recipes is deprecated, "
  echo "because it leads to extremely bad performance, due to the overhead of "
  echo "compiling each module indepentently! Compile and run `allRecipes.nim` instead!"

proc main(compile = false,
          compileDanger = false,
          run = false,
          json = false) =
  if (not compile and not compileDanger and not run and not json) or
    (compile and run):
    let t0 = epochTime()
    printWarning()
    genCommands("nim c -r recipes/", ".nim")
    echo "Compiling and running all recipes took ", epochTime() - t0
  elif compileDanger and run:
    let t0 = epochTime()
    printWarning()
    genCommands("nim c -d:danger -r recipes/", ".nim")
    echo "Compiling and running (danger mode) all recipes took ", epochTime() - t0
  elif compile:
    printWarning()
    genCommands("nim c recipes/", ".nim")
  elif compileDanger:
    printWarning()
    genCommands("nim c -d:danger recipes/", ".nim")
  elif json and run:
    let t0 = epochTime()
    for r in RecipeFiles:
      generateJsonFile(r, toRun = true)
    echo "Generating all source files to generate JSON and running them took ", epochTime() - t0
  elif json:
    let t0 = epochTime()
    for r in RecipeFiles:
      generateJsonFile(r)
    echo "Generating all source files to generate JSON took ", epochTime() - t0
  elif run:
    let t0 = epochTime()
    genCommands("./recipes/")
    echo "Running all recipes took ", epochTime() - t0
  else:
    doAssert false

when isMainModule:
  dispatch main
