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

if paramCount() == 0:
  let t0 = epochTime()
  genCommands("nim c -r recipes/", ".nim")
  echo "Compilating and running all recipes took ", epochTime() - t0
if paramCount() > 0:
  let p0 = paramStr(1)
  if p0 == "-c" or p0 == "--compile":
    genCommands("nim c recipes/", ".nim")
  elif p0 == "-cd" or p0 == "--compileDanger":
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
