import ggplot_types, ggplot_scales
import dataframe / arraymancer_backend

import ginger except Scale

import seqmath
import math, sequtils

proc smallestPow(invTrans: ScaleTransform, x: float): float =
  doAssert x > 0.0
  result = 1.0
  var exp = 0
  if x < 1.0:
    while result > x and not result.almostEqual(x):
      result = invTrans((exp - 1).float)
      dec exp
  else:
    while result < x and not result.almostEqual(x):
      result = invTrans((exp + 1).float)
      inc exp
    result = invTrans((exp - 1).float)

proc largestPow(invTrans: ScaleTransform, x: float): float =
  doAssert x > 0.0
  result = 1.0
  var exp = 0
  if x < 1.0:
    while result > x and not result.almostEqual(x):
      result = invTrans((exp - 1).float)
      dec exp
    result = invTrans((exp + 1).float)
  else:
    while result < x and not result.almostEqual(x):
      result = invTrans((exp + 1).float)
      inc exp

proc tickPosTransformed(s: ginger.Scale,
                        trans, invTrans: ScaleTransform,
                        numTicks: int, minv, maxv: float,
                        boundScale: ginger.Scale,
                        hideTickLabels = false,
                        format: proc(x: float): string): (seq[string], seq[float]) =
  ## Calculates the positions and labels of a log10 data scale given
  ## a min and max value. Takes into account a final bound scale outside
  ## of which no ticks may lie.
  let numTicks = (trans(maxv) - trans(minv)).round.int + 1
  var
    labs = newSeq[string]()
    labPos = newSeq[float]()
  var exp = floor(boundScale.low).int
  let base = invTrans(1.0)
  while exp.float < boundScale.high:
    let cur = invTrans(exp.float)
    let numToAdd = invTrans(1.0).round.int
    let minors = linspace(cur, invTrans((exp + 1).float) - cur, numToAdd - 1)
    labPos.add minors.mapIt(trans(it))
    if (boundScale.high - boundScale.low) > 1.0 or hideTickLabels:
      if not hideTickLabels:
        labs.add cur.format
      else:
        labs.add ""
      # add one less than minors.len of `""`
      labs.add(toSeq(0 ..< minors.high).mapIt(""))
    else:
      # use all minors as labelledn
      labs.add minors.mapIt(it.format)
    inc exp
  # add the current exp to the labels (not in loop anymore). In log2 `maxv` is
  # contained in loop, but real range is larger. In log10, maxv is not contained.
  if not hideTickLabels:
    labs.add(format(invTrans(exp.float)))
  else:
    labs.add ""
  labPos.add(exp.float)
  # for simplicity apply removal afterwards
  let filterIdx = toSeq(0 ..< labPos.len).filterIt(
    labPos[it] >= boundScale.low and
    labPos[it] <= boundScale.high
  )
  # apply filters to `labs` and `labPos`
  labs = filterIdx.mapIt(labs[it])
  labPos = filterIdx.mapIt(labPos[it])
  result = (labs, labPos)

proc tickPosLinear(scale: ginger.Scale, numTicks: int): seq[float] =
  ## computes the tick position and tick labels for non transformed data
  let (newScale, _, newNumTicks) = calcTickLocations(scale, numTicks)
  result = linspace(newScale.low, newScale.high, newNumTicks + 1).mapIt(it)

proc applyBoundScale(ticks: seq[float], boundScale: ginger.Scale): seq[float] =
  ## filters the given tick positions by the `boundScale` so that now
  ## tick position remains outside that range
  result = ticks.filterIt(it >= boundScale.low and it <= boundScale.high)

proc computeLabel(tick: float, tickScale = 0.0,
                  fmt: ContinuousFormat = nil): string =
  if fmt != nil:
    result = fmt tick
  else:
    result = formatTickValue(tick, tickScale)

proc computeLabels(ticks: seq[float], tickScale = 0.0, fmt: ContinuousFormat = nil): seq[string] =
  ## computes the labels for the given ticks. Simply a string representation
  ## of the labels
  result = ticks.mapIt(it.computeLabel(tickScale, fmt))

proc getCorrectDataScale(scale: Viewport, axKind: AxisKind): ginger.Scale =
  # first get base scale from `Scale`
  case axKind
  of akX: result = scale.xScale
  of akY: result = scale.yScale

proc applyScaleTrans(scale: ginger.Scale, trans: Option[FormulaNode]): ginger.Scale =
  # possibly modify using secondary transformation
  result = scale
  if trans.isSome:
    let fn = trans.get
    result.low = result.low * fn.evaluate().toFloat
    result.high = result.high * fn.evaluate().toFloat

proc revertScaleTrans(ticks: seq[float], trans: Option[FormulaNode]): seq[float] =
  # possibly reverts the secondary transformation
  result = ticks
  if trans.isSome:
    let fn = trans.get
    result = ticks.mapIt(it / fn.evaluate().toFloat)

proc toCoord1D(ticks: seq[float], axKind: AxisKind, scale: ginger.Scale): seq[Coord1D] =
  ## returns the given ticks as `Coord1D`
  result = ticks.mapIt(Coord1D(pos: it, kind: ukData, scale: scale, axis: axKind))

proc handleContinuousTicks(view: Viewport, p: GgPlot, axKind: AxisKind,
                           dataScale: ginger.Scale,
                           scKind: ScaleKind,
                           numTicks: int, theme: Theme,
                           trans, invTrans: ScaleTransform = nil,
                           secAxisTrans: Option[FormulaNode] = none[FormulaNode](),
                           format: ContinuousFormat = nil,
                           isSecondary = false,
                           hideTickLabels = false,
                           margin = none[Coord1D]()): seq[GraphObject] =
  var boundScale = if isSecondary: dataScale
                   elif axKind == akX: theme.xMarginRange else: theme.yMarginRange
  var rotate: Option[float]
  var alignTo: Option[TextAlignKind]
  case axKind
  of akX:
    rotate = theme.xTicksRotate
    alignTo = theme.xTicksTextAlign
  of akY:
    rotate = theme.yTicksRotate
    alignTo = theme.yTicksTextAlign

  case scKind
  of scLinearData:
    # if secondary, use transformed scale
    let scale = dataScale.applyScaleTrans(secAxisTrans)
    boundScale = boundScale.applyScaleTrans(secAxisTrans)
    var ticks = tickPosLinear(scale, numTicks)
      .applyBoundScale(boundScale)
    let labels = computeLabels(ticks, scale.high - scale.low, format)
    # if we have done a `secAxisTrans` invert that transformation for the ticks. This is so
    # that the ``position`` of the ticks makes sense in the transformed space (for which the
    # labels were computed, but their position can be well defined in normal space
    ticks = ticks.revertScaleTrans(secAxisTrans)
    let tickCoord = ticks.toCoord1D(axKind, scale)
    let (tickObjs, labObjs) = view.tickLabels(tickCoord, labels, axKind, isSecondary = isSecondary,
                                              rotate = rotate,
                                              alignToOverride = alignTo,
                                              font = theme.tickLabelFont,
                                              margin = margin)
    if not hideTickLabels:
      view.addObj concat(tickObjs, labObjs)
    result = tickObjs
  of scTransformedData:
    #let scale = getCorrectScale(dataScale, axKind, secAxisTrans)
    let scale = dataScale.applyScaleTrans(secAxisTrans)
    let minVal = invTrans.smallestPow(invTrans(dataScale.low))
    let maxVal = invTrans.largestPow(invTrans(dataScale.high))
    let (labs, labelpos) = tickPosTransformed(scale, trans, invTrans,
                                              numTicks, minVal, maxVal, boundScale,
                                              hideTickLabels = hideTickLabels,
                                              format = format)
    let vScale = getCorrectDataScale(view, axKind)
    let tickLocs = labelPos.toCoord1D(axKind, vScale)
    case axKind
    of akX: view.xScale = (low: trans(minVal), high: trans(maxVal))
    of akY: view.yScale = (low: trans(minVal), high: trans(maxVal))
    let (tickObjs, labObjs) = view.tickLabels(tickLocs, labs, axKind, isSecondary = isSecondary,
                                              rotate = rotate,
                                              alignToOverride = alignTo,
                                              font = theme.tickLabelFont,
                                              margin = margin)
    if not hideTickLabels:
      view.addObj concat(tickObjs, labObjs)
    result = tickObjs
  else: discard

proc handleDiscreteTicks*(view: Viewport, p: GgPlot, axKind: AxisKind,
                          labelSeq: seq[Value],
                          theme: Theme,
                          isSecondary = false,
                          hideTickLabels = false,
                          centerTicks = true,
                          margin = none[Coord1D](),
                          format: proc(x: Value): string): seq[GraphObject] =
  # create custom tick labels based on the possible labels
  # and assign tick locations based on ginger.Scale for
  # linear/trafo kinds and evenly spaced based on string?
  # start with even for all
  if isSecondary:
    raise newException(Exception, "Secondary axis for discrete axis not yet implemented!")
  let numTicks = labelSeq.len
  var tickLabels: seq[string]
  var tickLocs: seq[Coord1D]

  # TODO: check if we should use w/hImg here, distinguish the axes
  let discrMarginOpt = p.theme.discreteScaleMargin
  var discrMargin = 0.0
  if discrMarginOpt.isSome:
    case axKind
    of akX: discrMargin = discrMarginOpt.unsafeGet.toRelative(length = some(pointWidth(view))).val
    of akY: discrMargin = discrMarginOpt.unsafeGet.toRelative(length = some(pointHeight(view))).val
  # NOTE: the following only holds if def. of `wview` changed in ginger
  # doAssert view.wview != view.wimg
  let barViewWidth = (1.0 - 2 * discrMargin) / numTicks.float
  var centerPos = barViewWidth / 2.0
  if not centerTicks:
    case axKind
    of akX: centerPos = 0.0
    of akY: centerPos = barViewWidth
  for i in 0 ..< numTicks:
    if not hideTickLabels: tickLabels.add format(labelSeq[i])
    else: tickLabels.add ""
    # in case of a discrete scale we have categories, which are evenly spaced.
    # taking into account the margin of the plot, calculate center of all categories
    let pos = discrMargin + i.float * barViewWidth + centerPos
    let scale = (low: 0.0, high: 1.0)
    tickLocs.add Coord1D(pos: pos,
                         kind: ukRelative)
  var rotate: Option[float]
  var alignTo: Option[TextAlignKind]
  case axKind
  of akX:
    rotate = theme.xTicksRotate
    alignTo = theme.xTicksTextAlign
  of akY:
    rotate = theme.yTicksRotate
    alignTo = theme.yTicksTextAlign
  let (tickObjs, labObjs) = view.tickLabels(tickLocs, tickLabels, axKind, rotate = rotate,
                                            alignToOverride = alignTo,
                                            font = theme.tickLabelFont,
                                            margin = margin)
  if not hideTickLabels:
    view.addObj concat(tickObjs, labObjs)
  result = tickObjs

proc handleTicks*(view: Viewport, filledScales: FilledScales, p: GgPlot,
                  axKind: AxisKind, theme: Theme,
                  hideTickLabels = false,
                  numTicksOpt = none[int](),
                  boundScaleOpt = none[ginger.Scale]()): seq[GraphObject] =
  ## This handles the creation of the tick positions and tick labels.
  ## It automatically updates the x and y scales of both the viewport and the `filledScales`!
  ## `margin` is the tick label margin in centimeter!
  var marginOpt: Option[Coord1D]
  var scale: Scale
  var numTicks: int
  case axKind
  of akX:
    scale = filledScales.getXScale()
    numTicks = if numTicksOpt.isSome: numTicksOpt.unsafeGet else: p.numXTicks
    if theme.xTickLabelMargin.isSome:
      marginOpt = some(view.c1(theme.xTickLabelMargin.unsafeGet, axKind, ukCentimeter))
  of akY:
    scale = filledScales.getYScale()
    numTicks = if numTicksOpt.isSome: numTicksOpt.unsafeGet else: p.numYTicks
    if theme.yTickLabelMargin.isSome:
      marginOpt = some(view.c1(theme.yTickLabelMargin.unsafeGet, axKind, ukCentimeter))
  when defined(defaultBackend):
    let hasScale = not scale.col.isNil
  else:
    let hasScale = scale.col.name.len > 0
  if hasScale:
    case scale.dcKind
    of dcDiscrete:
      result = view.handleDiscreteTicks(p, axKind, scale.labelSeq, theme = theme,
                                        hideTickLabels = hideTickLabels,
                                        margin = marginOpt,
                                        format = scale.formatDiscreteLabel)
      if hasSecondary(filledScales, axKind):
        let secAxis = filledScales.getSecondaryAxis(axKind)
        result.add view.handleDiscreteTicks(p, axKind, scale.labelSeq, theme = theme,
                                            isSecondary = true,
                                            hideTickLabels = hideTickLabels,
                                            margin = marginOpt,
                                            format = scale.formatDiscreteLabel)
    of dcContinuous:
      result = view.handleContinuousTicks(p, axKind, scale.dataScale,
                                          scale.scKind,
                                          numTicks,
                                          trans = scale.trans,
                                          invTrans = scale.invTrans,
                                          theme = theme,
                                          hideTickLabels = hideTickLabels,
                                          margin = marginOpt,
                                          format = scale.formatContinuousLabel)
      if hasSecondary(filledScales, axKind):
        let secAxis = filledScales.getSecondaryAxis(axKind)
        # we discard the result, because we only use it to generate the grid lines. Those
        # are focused based on the primary axis of course
        discard view.handleContinuousTicks(p, axKind, scale.dataScale,
                                           scale.scKind,
                                           numTicks,
                                           trans = scale.trans,
                                           invTrans = scale.invTrans,
                                           secAxisTrans = secAxis.trans,
                                           theme = theme,
                                           hideTickLabels = hideTickLabels,
                                           isSecondary = true,
                                           margin = marginOpt,
                                           format = scale.formatContinuousLabel)
  else:
    ## TODO: merge this into the other branch
    # this should mean the main geom is histogram like?
    doAssert axKind == akY, "we can have akX without scale now?"
    # in this case don't read into anything and just call ticks / labels
    var boundScale: ginger.Scale
    if boundScaleOpt.isSome:
      boundScale = boundScaleOpt.unsafeGet
    else:
      boundScale = if axKind == akX: theme.xMarginRange else: theme.yMarginRange
    let ticks = view.initTicks(axKind, numTicks, boundScale = some(boundScale))
    var tickLabs: seq[GraphObject]
    tickLabs = view.tickLabels(ticks, font = theme.tickLabelFont,
                               margin = marginOpt)
    if not hideTickLabels:
      view.addObj concat(ticks, tickLabs)
    result = ticks
