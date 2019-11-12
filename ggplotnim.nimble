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

task test, "Run tests":
  exec "nim c -r tests/testDf.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r tests/test_issue2.nim"
  exec "nim c -r tests/test_issue20.nim"


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
