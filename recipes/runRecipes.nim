import shell, os, macros, times

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

if paramCount() == 0:
  let t0 = epochTime()
  printWarning()
  genCommands("nim c -r recipes/", ".nim")
  echo "Compilating and running all recipes took ", epochTime() - t0
if paramCount() > 0:
  let p0 = paramStr(1)
  if p0 == "-c" or p0 == "--compile":
    printWarning()
    genCommands("nim c recipes/", ".nim")
  elif p0 == "-cd" or p0 == "--compileDanger":
    printWarning()
    genCommands("nim c -d:danger recipes/", ".nim")
  elif p0 == "-r" or p0 == "--run":
    let t0 = epochTime()
    genCommands("./recipes/")
    echo "Running all recipes took ", epochTime() - t0
  elif p0 == "--json" or p0 == "--run":
    let t0 = epochTime()
    for r in RecipeFiles:
      generateJsonFile(r)
    echo "Running all recipes took ", epochTime() - t0
