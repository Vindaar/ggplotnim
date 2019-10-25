# Package

version       = "0.1.1"
author        = "Sebastian Schmidt"
description   = "A port of ggplot2 for Nim"
license       = "MIT"
srcDir        = "src"

# Dependencies

requires "nim >= 1.0.0"
requires "https://github.com/Vindaar/seqmath#head"
requires "https://github.com/Vindaar/ginger#head"
requires "persvector#head"
#requires "https://github.com/Vindaar/chroma#addMoreSpaces"

task test, "Run tests":
  exec "nim c -r tests/testDf.nim"
  exec "nim c -r tests/tests.nim"
  exec "nim c -r tests/test_issue2.nim"
