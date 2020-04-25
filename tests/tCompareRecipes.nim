import ggplotnim, unittest, strutils, os, sequtils, osproc, shell, json

#[
This test simply builds all recipes, runs them and compares the final image of
all recipes with the expected plots in `media/expected`

NOTE: this test depends on imagemagick, since it converts the PNG files to PPM!
]#

import ../recipes/recipeFiles

proc echoFields(j1, j2: JsonNode) =
  doAssert j1.len >= j2.len
  for kx, vx in pairs(j1):
    echo "Is ", kx, ", with val: ", vx, " in j2? ", kx in j2
    if kx notin j2:
      echo "^^^^^^\n"

proc compareJObjects*(j1, j2: JsonNode): bool =
  template returnOnFalse(c1, c2: untyped): untyped =
    result = c1 == c2
    if not result:
      echo "Didn't match ", astToStr(c1), " == ", astToStr(c2)
      echo "Was ", c1, " and ", c2
      return false
  returnOnFalse(j1.kind, JObject)
  returnOnFalse(j2.kind, JObject)
  if j1.len > j2.len:
    echo "j1 contains more elements than j2!"
    echoFields(j1, j2)
  elif j2.len > j1.len:
    echo "j2 contains more elements than j1!"
    echoFields(j2, j1)
  else:
    # all good, same field number
    discard
  #returnOnFalse(j1.len, j2.len)
  for k, v in pairs(j1):
    returnOnFalse(k in j2, true)
    returnOnFalse(v.kind, j2[k].kind)
    case v.kind
    of JObject:
      returnOnFalse(compareJObjects(v, j2[k]), true)
    of JFloat:
      returnOnFalse(almostEqual(v.getFloat, j2[k].getFloat), true)
    else:
      returnOnFalse(v, j2[k])

suite "Compare recipe output":
  test "Compare recipe generated plots":
    # first run recipes to make sure we have current recipe plots in
    # media/recipes which we can compare with media/expected
    let runRecipes = shellVerbose:
      nimble recipes
    let toContinue = runRecipes[1] == 0
    check toContinue
    if not toContinue:
      quit("Could not run recipes successfully, quitting recipe comparison")

    ## NOTE: these files cannot be compared as image files, because they contain
    ## text on non white (transparent) background, which is turned black after conversion
    ## to `ppm`. The text rendering on the cairo library on travis is different than
    ## locally, so it fails. These files are only tested using JSON below.
    const FilesToSkip = @["rCustomAnnotations",
                          "rSimpleGeomText",
                          "rClassifiedGeomText",
                          "rAnnotateUsingGeomText",
                          "rAnnotateMaxValues",
                          "rPeriodicTable",
                          "rSimpleFacet",
                          "rFacetTpa"]

    proc convertRead(path: string): seq[seq[string]] =
      for i, f in RecipeFiles: # from `recipeFiles`
        if f in FilesToSkip: continue
        let pathF = path / $f & ".png"
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
    var idx = 0
    for f in RecipeFiles:
      if f in FilesToSkip: continue
      checkFiles(expected[idx], isnow[idx], f)
      inc idx

  test "Compare recipe plots via JSON":
    ## first generate JSON from all recipe files by creating temporary
    ## recipe files, in which the `ggsave` call is replaced by `ggjson`, which
    ## simply dumps
    const tmpfile = "recipes/tmpfile.nim"
    for f in RecipeFiles:
      let fname = "recipes" / f
      # read the file
      let fcontent = readFile(fname & ".nim")
      # replace `ggsave` by `ggjson` and write file back
      const jsonImport = "from json import `%`\n"
      writeFile(tmpfile, jsonImport & fcontent.multiReplace(@[("ggsave(", "ggjson("),
                                                              ("media/", "resources/")]))
      # run the tmp file to generate json
      #shell:
      discard execCmd("nim c -r " & $tmpfile)
      # compare generated json with expected json
      let resFile = parseFile "resources/recipes" / f & ".json"
      echo "Checking ", f & ".json"
      let expFile = parseFile(("resources/expected" / f & ".json").replace("recipes/", "expected/"))
      check compareJObjects(resFile, expFile)

    ## NOTE: this is only safe against regressions of `ggplotnim`, because the
    ## JSON we compare is ``before`` being embedded into the final root viewport
    ## in ginger! It is simply the `Viewport` and all objects + children, as they
    ## are handed to ginger to be drawn.
