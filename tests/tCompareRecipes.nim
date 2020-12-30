import ggplotnim, unittest, strutils, os, sequtils, osproc, shell, json, macros

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

template returnOnFalse(c1, c2: untyped, key = ""): untyped =
  result = c1 == c2
  if not result:
    echo "Didn't match ", astToStr(c1), " == ", astToStr(c2)
    echo "Was ", c1, " and ", c2
    if key.len > 0:
      echo "For keys: ", key
    return false

proc compareJson*(j1, j2: JsonNode): bool =
  returnOnFalse(j1.kind, j2.kind)
  case j1.kind
  of JObject:
    returnOnFalse(j1.len, j2.len)
    for k, v in pairs(j1):
      when not defined(linux):
        if k == "txtPos" or k == "txtText": # txtText broken for multiline
          echo "INFO: Skipping key ", k, " due to cairo differences in text " &
            "printing on different platforms"
          continue
      returnOnFalse(k in j2, true)
      returnOnFalse(compareJson(v, j2[k]), true, k)
  of JFloat:
    when defined(linux):
      let cmpFloat = almostEqual(j1.getFloat, j2.getFloat, 1e-4)
    else:
      ## TODO: due to some cairo issue related to different platforms we get different
      ## positions on mac/windows. For now we just use a much larger epsilon. Need to
      ## investigate this difference once I have access to a machine running windows again.
      let cmpFloat = abs(j1.getFloat - j2.getFloat) < 0.1 # crude manual...
    if not cmpFloat:
      echo "Float compare failed: ", j1.getFloat, " <-> ", j2.getFloat
    returnOnFalse(cmpFloat, true)
  of JArray:
    returnOnFalse(j1.len, j2.len)
    for i in 0 ..< j1.len:
      returnOnFalse(compareJson(j1[i], j2[i]), true)
  else:
    returnOnFalse(j1, j2)

suite "Compare recipe output":
  test "Compare recipe generated plots":
    # first run recipes to make sure we have current recipe plots in
    # media/recipes which we can compare with media/expected
    const path = getProjectPath().parentDir
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
                          "rFacetTpa",
                          "rFacetRaster",
                          "rCustomFill",
                          "rCustomMargins"
    ]

    proc convertRead(path, f: string): Tensor[int] =
      let pathF = path / $f & ".png"
      check fileExists(pathF)
      let (_, _, fext) = pathF.splitFile
      let args = "-set coflorspace Gray -separate -average -compress none -resize 40%"
      when not defined(windows):
        let res = shellVerbose:
          convert ($pathF) ($args) ($pathF.replace(fext, ".ppm"))
      else:
        # there's some windows tool called convert, need to prepend `magick`
        let res = shellVerbose:
          magick ($pathF) ($args) ($pathF.replace(fext, ".ppm"))
      result = readFile(pathF.replace(fext, ".ppm")).splitLines()[3 .. ^1].foldl(a & b, "")
        .strip.split.mapIt(it.parseInt).toTensor()
    template checkFiles(f1, f2, fname: untyped): untyped =
      # store in `comp` to avoid check obliterating our terminal with the diff
      let diff = (f1 -. f2).abs.sum
      let comp = diff.float / 256.0 < (f1.size.float * 0.001) # less than 1/1000 pixels different
      echo "Tensor is long: ", expected.len, " and diff ", diff
      echo "Real diff ", diff.float / 256.0, " needs to be smaller ", f1.size.float * 0.001
      check comp
      if not comp:
        echo "Comparison failed for file: ", fname, " difference is: ", diff

    var idx = 0
    for f in RecipeFiles:
      if f in FilesToSkip: continue
      let expected = convertRead(path / "media/expected", f)
      let isnow = convertRead(path / "media/recipes", f)
      check isnow.len == expected.len
      checkFiles(expected, isnow, f)

  when defined(linux):
    test "Compare recipe plots via JSON":
      ## first generate JSON from all recipe files by creating temporary
      ## recipe files, in which the `ggsave` call is replaced by `ggjson`, which
      ## simply dumps
      const path = getProjectPath().parentDir
      let runRecipesJson = shellVerbose:
        nimble recipesJson
      let toContinue = runRecipesJson[1] == 0
      check toContinue
      if not toContinue:
        quit("Could not run recipes for JSON successfully, quitting recipe comparison")

      const FilesToSkip = @["rMultiSubplots"] # cannot autogenerate JSON, because it does not
                                              # use a `ggsave` call
      for f in RecipeFiles:
        if f in FilesToSkip: continue
        # compare generated json with expected json
        let resFile = parseFile "resources/recipes" / f & ".json"
        echo "Checking ", f & ".json"
        let expFile = parseFile(("resources/expected" / f & ".json").replace("recipes/", "expected/"))
        let cmpRes = compareJson(resFile, expFile)
        check cmpRes
        if not cmpRes:
          break

      ## NOTE: this is only safe against regressions of `ggplotnim`, because the
      ## JSON we compare is ``before`` being embedded into the final root viewport
      ## in ginger! It is simply the `Viewport` and all objects + children, as they
      ## are handed to ginger to be drawn.
