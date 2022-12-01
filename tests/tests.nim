import unittest
# we include ggplotnim so that we can test non exported procs
include ../src/ggplotnim

import seqmath
import std / [random, math]
randomize(42)

proc almostEq(a, b: float, epsilon = 1e-8): bool =
  ## version of `almostEqual` for testing, which prints the values, if
  ## they mismatch
  result = almostEqual(a, b, epsilon)
  if not result:
    echo "Comparison failed: a = ", a, ", b = ", b

suite "Formula":
  test "Testing ~ formula creation using f{} macro":
    let s = Scale(col: f{"testCol"},
                  scKind: scTransformedData,
                  dcKind: dcContinuous,
                  trans: (proc(v: float): float =
                            result = v * 2.0
                  )
    )
    let col = $s.col
    var f3 = f{float: col ~ s.trans( df[col][idx] )}
    check f3.name == "(~ col (s.trans df[col][idx]))"
    # test function on DF
    let df = toDf( { "testCol" : @[1.0, 2.0, 3.0] })
    check f3.evaluate(df).toTensor(Value) == toTensor(%~ @[2.0, 4.0, 6.0])

suite "Geom":
  test "application of aes, style works":
    # Write test which tests that the application of things like an
    # aesthetic and a style, e.g. color, line size etc, is properly
    # applied for all geoms!
    # Take a look at the style check in the first GgPlot test
    discard

suite "Aesthetics":

  template compileFails(body: untyped): untyped =
    when not compiles(body):
      true
    else:
      false

  test "aes macro - simple valid inputs, all named args":
    let a = aes(x = "x", y = "y", color = "class")
    check a.x.isSome
    check a.y.isSome
    check a.color.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.color.get.col == "class"

  test "aes macro - simple valid inputs, some unnamed args":
    let a = aes("x", "y", color = "class")
    check a.x.isSome
    check a.y.isSome
    check a.color.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.color.get.col == "class"

  test "aes macro - unnamed after named arg":
    ## this is not necessarily a "nice" feature...
    let a = aes(x = "x", "y", color = "class")
    check a.x.isSome
    check a.y.isSome
    check a.color.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.color.get.col == "class"

  test "aes macro - invalid argument":
    check compileFails(aes(x = "x", y = "y", badArg = "class"))

  test "aes macro - invalid argument type":
    check compileFails(aes(x = "x", y = "y", color = {"I'm not" : "supported"}))

  test "aes macro - explicit formula":
    let a = aes("x", "y", xMin = f{0.2})
    check a.x.isSome
    check a.y.isSome
    check a.xMin.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.xMin.get.col == "0.2"

  test "aes macro - explicit complicated formula":
    let a = aes("x", "y", xMin = f{235 / `cty`})
    check a.x.isSome
    check a.y.isSome
    check a.xMin.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.xMin.get.col == "(/ 235 cty)"

  test "aes macro - explicit complicated formula for unnamed arg":
    let a = aes(f{235 / `cty`}, "y")
    check a.x.isSome
    check a.y.isSome

    check $a.x.get.col == "(/ 235 cty)"
    check $a.y.get.col == "y"

  test "aes macro - idents as strings":
    let a = aes("x", "y", color = hwy)
    check a.x.isSome
    check a.y.isSome
    check a.color.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.color.get.col == "hwy"

  test "aes macro - local variable overrides ident as string":
    let hwy = "cty"
    let a = aes("x", "y", color = hwy)
    check a.x.isSome
    check a.y.isSome
    check a.color.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.color.get.col == "cty"

  test "aes macro - force factorization (discrete) scale":
    let a = aes("x", "y", size = factor("shell"))
    check a.x.isSome
    check a.y.isSome
    check a.size.isSome

    check $a.x.get.col == "x"
    check $a.y.get.col == "y"
    check $a.size.get.col == "shell"
    check a.size.get.hasDiscreteness == true
    check a.size.get.dcKind == dcDiscrete

  when false:
    ## This test case does not work. Can't be checked via `compiles` I think because
    ## the `orNoneScales` is a generic proc, which essentially fails ``after``
    ## the `compiles` macro checks what's what.. But this does indeed fail, as it should!
    test "aes macro - proc of arg ident causes compile error":
      proc hwy(): float = 5.5
      let a = aes("x", "y", color = hwy)

suite "GgPlot":
  test "Histogram with discrete scale fails":
    let mpg = readCsv("data/mpg.csv")
    expect(ValueError):
      ggplot(mpg, aes("class")) + geom_histogram() + ggsave("fails.pdf")

  test "Bar with continuous scale fails":
    let mpg = readCsv("data/mpg.csv")
    expect(ValueError):
      ggplot(mpg, aes("cty")) + geom_bar() + ggsave("fails.pdf")

  test "Bar plot with string based scale":
    let mpg = readCsv("data/mpg.csv")
    let plt = ggcreate(ggplot(mpg, aes("class"), backend = bkCairo) + geom_bar())
    let plotView = plt.view[4]
    check plotView.name == "plot"
    proc calcPos(classes: seq[string]): seq[float] =
      ## given the possible classes, calculates the positions the
      ## labels have to be placed at
      ## NOTE: this is the same calculation happening in the `handleDisreteTicks`
      ## proc. Thus the test here is based on the assumption that this calc over
      ## there is correct. However, it's been checked by eye at the time of this
      ## commit (b1a3a155587d4ee54e6581ac99f3a428eea37c1f) that it produces the
      ## desired result.
      let discrMargin = quant(0.2, ukCentimeter).toRelative(
        length = some(pointWidth(plotView))
      ).val
      let nclass = classes.len
      let barViewWidth = (1.0 - 2 * discrMargin) / nclass.float
      let centerPos = barViewWidth / 2.0
      for i in 0 ..< nclass:
        let pos = discrMargin + i.float * barViewWidth + centerPos
        result.add pos
    let classes = mpg["class"].unique.toTensor(string).toSeq1D.sorted
    let checkPos = calcPos(classes)
    var
      idxTk = 0
      idxLab = 0
    for obj in plotView.objects:
      case obj.kind
      of goTick:
        # verify tick position
        if obj.tkAxis == akX:
          check obj.tkPos.x.pos == checkPos[idxTk]
          inc idxTk
      of goTickLabel:
        # verify position and text
        if obj.name == "xtickLabel":
          check obj.txtText == classes[idxLab]
          check obj.txtPos.x.pos == checkPos[idxLab]
          inc idxLab
      else: discard
    plt.ggdraw("bartest.pdf")


  test "Plot with continuous color scale":
    let mpg = readCsv("data/mpg.csv")
    ggplot(mpg, aes("displ", "hwy", color = "cty")) +
      geom_point() +
      ggsave("cont_color.pdf")
    # TODO: write an actual test here
    # NOTE: at least this works now! :) Only have to implement a legend for
    # colormaps and then we could add more colormaps.

  test "x,y aesthetics of geom picked over GgPlot":
    ## tests that the x, y aesthetics are picked from the present `geom`
    ## if x, y are defined, instead of the `GgPlot` object.
    let x = toSeq(0 .. 10).mapIt(it.float)
    let y1 = x.mapIt(cos(it))
    let y2 = x.mapIt(sin(it))
    let df = toDf({"x" : x, "cos" : y1, "sin" : y2})

    let gplt = ggplot(df, aes("x", "cos")) + #aes(x ~ cos)) +
      geom_line() + # line for cos
      geom_line(aes("x", "sin"), #x ~ sin), # line for sin
                color = some(color(0.0, 0.0, 0.0)),
                size = some(1.0))
    # geoms[0].x and y won't be set, since the aes from ggplot is used
    check (not gplt.geoms[0].aes.x.isSome)
    check (not gplt.geoms[0].aes.y.isSome)
    check gplt.geoms[1].aes.x.isSome
    check gplt.geoms[1].aes.y.isSome
    check gplt.aes.x.get.scKind == scLinearData
    check gplt.aes.y.get.scKind == scLinearData
    check $gplt.aes.x.get.col == "x"
    check $gplt.aes.y.get.col == "cos"
    check gplt.geoms[1].aes.x.get.scKind == scLinearData
    check gplt.geoms[1].aes.y.get.scKind == scLinearData
    check $gplt.geoms[1].aes.x.get.col == "x"
    check $gplt.geoms[1].aes.y.get.col == "sin"

    # bonus check
    let style = gplt.geoms[1].userStyle
    check style.color.isSome
    check style.color.get == color(0.0, 0.0, 0.0)
    check style.fillColor.isNone
    check style.lineWidth.isSome
    check style.lineWidth.get == 1.0
    check style.lineType.isNone

  test "Application of log scale works as expected":
    let x = linspace(1.0, 10.0, 500)
    let y1 = x.mapIt(pow(it, 2))
    let y2 = x.mapIt(pow(it, 4))
    let df = toDf({"x" : x, "xSquare" : y1, "x4" : y2})
    block:
      let plt = ggplot(df, aes("x", "xSquare")) +
        geom_line() +
        scale_x_log10()
      check plt.aes.x.isSome
      check plt.aes.y.isSome
      check $plt.aes.x.get.col == "x"
      check $plt.aes.y.get.col == "xSquare"
      check plt.aes.x.get.axKind == akX
      check plt.aes.y.get.axKind == akY
      check plt.aes.x.get.scKind == scTransformedData
      check plt.aes.y.get.scKind == scLinearData

    # check also applied to another geom added before
    block:
      let plt = ggplot(df, aes("x", "xSquare")) +
        geom_line(aes(y = "x4")) +
        geom_point(aes(y = "x4")) +
        scale_y_log10()
      check plt.aes.x.isSome
      check plt.aes.y.isSome
      check $plt.aes.x.get.col == "x"
      check $plt.aes.y.get.col == "xSquare"
      check plt.aes.x.get.axKind == akX
      check plt.aes.y.get.axKind == akY
      check plt.aes.x.get.scKind == scLinearData
      check plt.aes.y.get.scKind == scTransformedData
      check $plt.geoms[0].aes.y.get.col == "x4"
      check plt.geoms[0].aes.y.get.axKind == akY
      check plt.geoms[0].aes.y.get.scKind == scTransformedData
      plt.ggsave("sin_log.pdf")

    # check that it is ``not`` applied to a geom that is added ``after``
    # the call to `scale_*` (this is in contrast to `ggplot2` where the
    # order does not matter
    block:
      let plt = ggplot(df, aes("x", "xSquare")) +
        scale_x_log10() +
        geom_line(aes(y = "x4"))
      check plt.aes.x.isSome
      check plt.aes.y.isSome
      check $plt.aes.x.get.col == "x"
      check $plt.aes.y.get.col == "xSquare"
      check plt.aes.x.get.axKind == akX
      check plt.aes.y.get.axKind == akY
      check plt.aes.x.get.scKind == scTransformedData
      check plt.aes.y.get.scKind == scLinearData
      check $plt.geoms[0].aes.y.get.col == "x4"
      check plt.geoms[0].aes.y.get.axKind == akY
      check plt.geoms[0].aes.y.get.scKind == scLinearData

  test "Automatic margin setting for labels":
    let x = logspace(-6, 1.0, 100)
    let y = x.mapIt(exp(-it))
    let df = toDf({"x" : x, "exp" : y})
    let pltView = ggcreate(ggplot(df, aes("x", "exp"), backend = bkCairo) +
      geom_line() +
      scale_y_log10())
    let plt = pltView.view
    # extract x and y label of plt's objects
    let xLab = plt.children[4].objects.filterIt(it.name == "xLabel")
    let yLab = plt.children[4].objects.filterIt(it.name == "yLabel")
    template checkLabel(lab, labName, text, posTup, rot): untyped =
      check lab.name == labName
      check lab.kind == goLabel
      check lab.txtText == text
      check lab.txtAlign == taCenter
      when not defined(noCairo) and defined(linux):
        ## This check only works if we compile with the cairo backend. That is because the
        ## placement of the text in y position depends explicitly on the extent of the
        ## text, which is determined using cairo's TTextExtents object. The dummy backend
        ## provides only zeroes for these numbers.
        check lab.txtPos.y.toRelative.pos.almostEq(posTup.y.toRelative.pos)
        check lab.txtPos.x.toRelative.pos.almostEq(posTup.x.toRelative.pos)

      check lab.rotate == rot
      check lab.txtFont == Font(family: "sans-serif", size: 12.0, bold: false,
                                slant: fsNormal, color: color(0.0, 0.0, 0.0, 1.0),
                                alignKind: taCenter)
    # the default label margin is 1 cm, i.e. ~28.34 pixels at 72 dpi
    checkLabel(xLab[0], "xLabel", "x",
               Coord(x: Coord1D(pos: 0.5, kind: ukRelative),
                     y: Coord1D(pos: 424.403937007874, kind: ukPoint, length: some(pointHeight(plt.children[4])))),
               none[float]())
    checkLabel(yLab[0], "yLabel", "exp",
               Coord(x: Coord1D(pos: -0.09393183283490754, kind: ukRelative),
                     y: Coord1D(pos: 0.5, kind: ukRelative)),
               some(-90.0))
    check yLab[0].txtPos.x.toPoints.pos != quant(1.0, ukCentimeter).toPoints.val
    plt.ggdraw("exp.pdf")

  test "Set manual margin and text for labels":
    let x = logspace(-6, 1.0, 100)
    let y = x.mapIt(exp(-it))
    let df = toDf({"x" : x, "exp" : y})
    const xMargin = 0.5
    const yMargin = 1.7
    let pltView = ggcreate(ggplot(df, aes("x", "exp"), backend = bkCairo) +
      geom_line() +
      xlab("Custom label", margin = xMargin) +
      ylab("More custom!", margin = yMargin) +
      scale_y_log10())
    let plt = pltView.view
    # extract x and y label of plt's objects
    let view = plt.children[4]
    let xLab = view.objects.filterIt(it.name == "xLabel")
    let yLab = view.objects.filterIt(it.name == "yLabel")
    template checkLabel(lab, labName, text, rot): untyped =
      check lab.name == labName
      check lab.kind == goLabel
      check lab.txtText == text
      check lab.txtAlign == taCenter
      check lab.rotate == rot
      check lab.txtFont == Font(family: "sans-serif", size: 12.0, bold: false,
                                slant: fsNormal, color: color(0.0, 0.0, 0.0, 1.0),
                                alignKind: taCenter)
    # the default label margin is 1 cm, i.e. ~28.34 pixels at 72 dpi
    checkLabel(xLab[0], "xLabel", "Custom label",
               none[float]())
    checkLabel(yLab[0], "yLabel", "More custom!",
               some(-90.0))
    check almostEq(yLab[0].txtPos.x.toPoints.pos,
                      -quant(yMargin, ukCentimeter).toPoints.val,
                      epsilon = 1e-6)
    check almostEq(xLab[0].txtPos.y.toPoints.pos,
                      height(view).toPoints(some(view.hView)).val + quant(xMargin, ukCentimeter).toPoints.val,
                      epsilon = 1e-6)
    plt.ggdraw("exp2.pdf")

  test "Plot requiring `y` scale throws if none given":
    let mpg = readCsv("data/mpg.csv")
    try:
      ggplot(mpg, aes("year")) +
        geom_point() +
        ggsave("never_produced.png")
      check false
    except AestheticError:
      check true

  test "Plot not needing `y` scale throws if given":
    let mpg = readCsv("data/mpg.csv")
    try:
      ggplot(mpg, aes(x = "class", y = "cyl")) +
        geom_bar() +
        ggsave("never_produced.png")
      check false
    except AestheticError:
      check true

suite "Theme":
  test "Canvas background color":
    let mpg = readCsv("data/mpg.csv")
    let white = color(1.0, 1.0, 1.0)
    proc checkPlt(plt: GgPlot) =
      check plt.theme.canvasColor.isSome
      check plt.theme.canvasColor.unsafeGet == white
      let pltGinger = ggcreate(plt)
      # don't expect root viewport to have more than 1 element here
      check pltGinger.view.objects.len == 1
      let canvas = pltGinger.view.objects[0]
      check canvas.kind == goRect
      check canvas.style.isSome
      let canvasStyle = canvas.style.get
      check canvasStyle.fillColor == white

    block:
      let plt = ggplot(mpg, aes("hwy", "cty"), backend = bkCairo) +
        geom_point() +
        canvasColor(color = white)
      checkPlt(plt)
    block:
      let plt = ggplot(mpg, aes("hwy", "cty"), backend = bkCairo) +
        geom_point() +
        theme_opaque()
      checkPlt(plt)

suite "Annotations":
  test "Annotation using relative coordinates":
    let df = readCsv("data/mpg.csv")
    let annot = "A simple\nAnnotation\nMulti\nLine"
    let plt = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
      geom_line() +
      annotate(annot,
               left = 0.5,
               bottom = 1.0,
               font = font(size = 12.0,
                           family = "monospace")))
    let view = plt.view
    # get actual plot view
    let actPlot = view[4]
    var count = 0
    for gobj in actPlot.objects:
      if "multiLineText" in gobj.name:
        when not defined(noCairo) and defined(linux):
          ## text extent based calcs are not supported without cairo!
          check almostEq(gobj.txtPos.x.pos, 0.5, epsilon = 1e-6)
        # we don't check y because it depends on the line
        inc count
      elif "annotationBackground" in gobj.name:
        # rough position check. Values should align with bottom left of
        # the rectangle, placed in the plot viewport. Takes into
        # account the margin we use:
        when not defined(noCairo) and defined(linux):
          check almostEq(gobj.reOrigin.x.pos, 0.49167, epsilon = 1e-4)
          check almostEq(gobj.reOrigin.y.pos, 0.85176428, epsilon = 1e-4)
        else:
          discard
    # check number of lines
    check count == annot.strip.splitLines.len

  test "Annotation using data coordinates":
    let df = readCsv("data/mpg.csv")
    let annot = "A simple\nAnnotation\nMulti\nLine"
    let font = font(size = 12.0,
                    family = "monospace")
    let plt = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
      geom_point() +
      annotate(annot,
               x = 10.0,
               y = 20.0,
               font = font))
    let view = plt.view
    # get actual plot view
    let actPlot = view[4]
    var count = 0
    for gobj in actPlot.objects:
      if "multiLineText" in gobj.name:
        when not defined(noCairo) and defined(linux):
          ## text extent based calcs are not supported without cairo!
          check almostEq(gobj.txtPos.x.pos, 0.0, epsilon = 1e-6)
        # we don't check y because it depends on the line
        check gobj.txtFont == font
        check gobj.txtText == annot.strip.splitLines[count]
        inc count
      elif "annotationBackground" in gobj.name:
        # rough position check
        when not defined(noCairo) and defined(linux):
          check almostEq(gobj.reOrigin.x.pos, -0.008327, epsilon = 1e-4)
          check almostEq(gobj.reOrigin.y.pos, 0.35176428, epsilon = 1e-4)
        check gobj.style.isSome
        check gobj.style.unsafeGet.color == color(1.0, 1.0, 1.0, 1.0)
        check gobj.style.unsafeGet.fillColor == color(1.0, 1.0, 1.0, 1.0)
    # check number of lines
    check count == annot.strip.splitLines.len

  test "Manually set x and y limits":
    let df = readCsv("data/mpg.csv")
    let dfAt44 = df.filter(f{c"hwy" == 44})
    check dfAt44.len == 2
    check dfAt44["cty"].toTensor(float) == toTensor @[33.0, 35.0]
    block:
      let plt = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
        geom_point() +
        ylim(5, 30)) # will cut off two values at hwy = 44, clip them to `30`, since
                      # default is `outsideRange = "clip"` (`orkClip`)
      let view = plt.view[4]
      check view.yScale == (low: 5.0, high: 30.0)
      for gobj in view[0].objects:
        case gobj.kind
        of goPoint:
          if gobj.ptPos.x.pos == 44.0:
            check almostEq(gobj.ptPos.y.pos, 30.0, 1e-8)
        else: discard
    block:
      let plt = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
        geom_point() +
        ylim(5, 30, outsideRange = "drop")) # will drop 2 values at `hwy = 44`
      let view = plt.view
      check view.yScale == (low: 5.0, high: 30.0)
      var count = 0
      for gobj in view[4][0].objects:
        case gobj.kind
        of goPoint:
          if gobj.ptPos.x.pos == 44.0:
            inc count
        else: discard
      check count == 0
    block:
      let plt = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
        geom_point() +
        ylim(5, 30, outsideRange = "none")) # will leave two values at `hwy = 44` somewhere
                                             # outside the plot
      let view = plt.view
      check view.yScale == (low: 5.0, high: 30.0)
      for gobj in view[4][0].objects:
        case gobj.kind
        of goPoint:
          if gobj.ptPos.x.pos == 44.0:
            check (almostEq(gobj.ptPos.y.pos, 33.0, 1e-8) or
                   almostEq(gobj.ptPos.y.pos, 35.0, 1e-8))
        else: discard

  test "Set custom plot data margins":
    let df = readCsv("data/mpg.csv")
    const marg = 0.05
    let plt = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
        geom_point() +
        xMargin(marg))
    let pltRef = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
        geom_point())
    let pltRefXScale = pltRef.view[4].xScale
    let view = plt.view[4]
    # naive `xScale` is low to high
    let xScale = (low: colMin(df, "hwy"), high: colMax(df, "hwy"))
    check pltRefXScale != xScale # scale is adjusted by calculation of tick positions!
    check view.xScale == (low: pltRefXScale.low - marg * (pltRefXScale.high - pltRefXScale.low),
                          high: pltRefXScale.high + marg * (pltRefXScale.high - pltRefXScale.low))

  test "Margin plus limit using orkClip clips to range + margin":
    let df = readCsv("data/mpg.csv")
    const marg = 0.1
    #let pltRef = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
    #    geom_point())
    let plt = ggcreate(ggplot(df, aes("hwy", "cty"), backend = bkCairo) +
        geom_point() +
        xlim(0.0, 30.0) +
        xMargin(marg))
    ## the interesting aspect here is that the points are not clipped to `30.0` as given
    ## by the limit, but rather to limit + margin. This allows to create a sort of
    ## buffer area where points show up, which are outside the desired range (e.g. to
    ## highlight `inf`, `-inf`). However, ``all`` values > 30.0 are clipped to `33`!
    let view = plt.view[4]
    # results in range +- (range.high - range.low) * marg
    check view.xScale == (low: -3.0, high: 33.0)
    for gobj in view[0].objects:
      case gobj.kind
      of goPoint:
        if gobj.ptPos.x.pos > 30.0:
          check almostEq(gobj.ptPos.x.pos, 33.0, 1e-8)
      else: discard

  test "Negative margins raise ValueError":
    let df = readCsv("data/mpg.csv")
    expect(ValueError):
      ggplot(df, aes("hwy", "cty")) +
        geom_point() +
        xMargin(-0.5) +
        ggsave("raisesInstead")
    expect(ValueError):
      ggplot(df, aes("hwy", "cty")) +
        geom_point() +
        yMargin(-0.5) +
        ggsave("raisesInstead")

  test "Merging of 'empty' data scales results in useful scale":
    ## This is a rather subtle. If the input data on one axis is
    ## only 0 we end up ignoring it in `mergeScales` during post processing.
    ## However, if the user also adds min and max values (yMin, yMax for instance)
    ## setting constant values, the result is still well defined. This was fixed
    ## in
    ## 71983ef6e5a41c4a65ba165799bfb2297dd35bb6
    ## This test is just using the previously broken example as a test.
    var spikes = @[0]
    var neurons = newSeqWith(27, 0)
    spikes.add newSeqWith(26, 502)
    let df = toDf(spikes, neurons)


    block:
      let plt = ggcreate(
        ggplot(df, aes("spikes", "neurons"), backend = bkCairo) +
          geom_linerange(aes(ymin = f{-1.0},
                             ymax = f{1.0})) +
          scale_y_continuous() + # make sure y is considered cont.
          ylim(-1, 1) + # at the moment ymin, ymax are not considered for the plot range (that's a bug)
          ggtitle("Spike raster plot")
      )

      let fs = plt.filledScales
      check fs.xScale == (0.0, 1.0)
      check fs.yScale == (-1.0, 1.0)

      check fs.yMin.more.len == 1
      check fs.yMax.more.len == 1

      check fs.yMin.more[0].col.val.toInt == -1
      check fs.yMax.more[0].col.val.toInt == 1

      var xLabelCount = 0
      for ch in plt.view[4].objects:
        case ch.kind
        of goTickLabel:
          if ch.txtPos.x.pos > 0.0:
            # should mean we're looking at x axis tick labels
            check ch.txtText in @["0", "502"]
            inc xLabelCount
        else: discard
      check xLabelCount == 2

      # now check if we have two child viewports for discrete X scale
      # now get data viewport of plot and check it has 4 children
      let dataView = plt.view[4][0]
      check dataView.name == "data"
      check dataView.children.len == 4
      check dataView.xScale == (0.0, 1.0)
      check dataView.yScale == (-1.0, 1.0)

    block:
      let plt = ggcreate(
        ggplot(df, aes("spikes", "neurons"), backend = bkCairo) +
          geom_linerange(aes(ymin = f{-1.0},
                             ymax = f{1.0})) +
          scale_y_continuous() + # make sure y is considered cont.
          ggtitle("Spike raster plot")
      )

      let fs = plt.filledScales
      check fs.xScale == (0.0, 1.0)
      check fs.yScale == (-1.0, 1.0)

      check fs.yMin.more.len == 1
      check fs.yMax.more.len == 1

      check fs.yMin.more[0].col.val.toInt == -1
      check fs.yMax.more[0].col.val.toInt == 1

      var xLabelCount = 0
      for ch in plt.view[4].objects:
        case ch.kind
        of goTickLabel:
          if ch.txtPos.x.pos > 0.0:
            # should mean we're looking at x axis tick labels
            check ch.txtText in @["0", "502"]
            inc xLabelCount
        else: discard
      check xLabelCount == 2

      # now check if we have two child viewports for discrete X scale
      # now get data viewport of plot and check it has 4 children
      let dataView = plt.view[4][0]
      check dataView.name == "data"
      check dataView.children.len == 4
      check dataView.xScale == (0.0, 1.0)
      check dataView.yScale == (-1.0, 1.0)

    block:
      ## This test is essentially a test for a current bug, namely
      ## that all 0 values for an axis are not allowed (read: ignored by
      ## `mergeScales` in postprocessing!
      ## TODO: fix the bug!
      expect(ValueError):
        discard ggcreate(
          ggplot(df, aes("spikes", "neurons"), backend = bkCairo) +
            geom_linerange(aes(ymin = f{-1.0})) +
            scale_y_continuous() + # make sure y is considered cont.
            ylim(-1, 1) + # at the moment ymin, ymax are not considered for the plot range (that's a bug)
            ggtitle("Spike raster plot")
        )

  test "geom_bar w/ stat identity has yscale at 0":
    ## ref: issue #61
    ## we forgot to force the minimum value for geom_bar used for identity stat
    ## to 0. This meant the automatically determined data scale (even minimum y)
    ## was used, resulting in a botched plot

    let df = toDf({ "Age" : @[22, 54, 34],
                    "Height" : @[1.87, 1.75, 1.78],
                    "Name" : @["Mike", "Laura", "Sue"] })
    let plt = ggcreate(
      ggplot(df, aes("Name","Height"), backend = bkCairo) +
        geom_bar(stat="identity")
    )

    let expScale = (0.0, 2.0)
    check plt.view[4].yScale == expScale
    check plt.filledScales.yScale == expScale

  test "geom_bar w/ stat identity has yscale at neg value if data negative":
    ## ref issue: #64
    ## related to issue #61 and its fix (see test above). Instead of forcing the
    ## negative value to 0 explicitly, we should select the minimum of 0 and the
    ## current y scale, to allow negative values
    let trials = @["A", "B", "C", "D", "E"]
    let values = @[1.0, 0.5, 0, -0.5, -1.0]
    let df = toDf({ "Trial" : trials,
                    "Value" : values })
    let plt = ggcreate(
      ggplot(df, aes(x="Trial", y="Value"), backend = bkCairo) +
        geom_bar(stat="identity", position="identity")
    )

    let expScale = (-1.0, 1.0)
    check plt.view[4].yScale == expScale
    check plt.filledScales.yScale == expScale

  test "application of `factor` on aes has desired effect":
    let xs = arange(0, 30)
    let ys = xs.mapIt(it * it)
    let cs = newSeqWith(xs.len, rand(8))
    let df = toDf({ "x" : xs, "y" : ys, "class" : cs })
    block ClassIndeedContinuous:
      # first check that this does indeed result in a classification by
      # guessType that's continuous
      let plt = ggcreate(
        ggplot(df, aes(x, y, color = class), backend = bkCairo) +
          geom_line()
      )
      check plt.filledScales.color.main.isSome
      let cScale = plt.filledScales.color.main.get
      check $cScale.col == "class"
      check not cScale.hasDiscreteness
      check cScale.dcKind == dcContinuous

    block FactorMakesDiscrete:
      # now check factor has desired effect
      let plt = ggcreate(
        ggplot(df, aes(x, y, color = factor(class)), backend = bkCairo) +
          geom_line()
      )
      check plt.filledScales.color.main.isSome
      let cScale = plt.filledScales.color.main.get
      check $cScale.col == "class"
      # NOTE: `hasDiscreteness` is ``not`` copied over during `collect_and_fill.fillScale`!
      # check cScale.hasDiscreteness
      check cScale.dcKind == dcDiscrete
