# Package

version       = "0.1.0"
author        = "Sebastian Schmidt"
description   = "A port of ggplot2 for Nim"
license       = "MIT"
srcDir        = "src"



# Dependencies

requires "nim >= 0.19.9"
requires "https://github.com/Vindaar/seqmath"
requires "https://github.com/Vindaar/ginger#head"
requires "persvector#head"
#requires "https://github.com/Vindaar/chroma#addMoreSpaces"

task test, "Run tests":
  exec "nim c -r tests/testDF.nim"
  exec "nim c -r tests/tests.nim"
