import ggplot_types, ggplot_scales, ggplot_utils
import datamancer

import ginger except Scale

import seqmath
import std / [math, sequtils, times, sets, algorithm]

proc getTicks*(s: Scale): int =
  ## Returns a valid number of ticks based on the given `Scale`. This will either be
  ## the `numTicks` field if it is some, else it will be a default (10).
  result = s.numTicks.get(otherwise = 10)

proc getXTicks*(fs: FilledScales): int =
  ## Returns a valid number of ticks for X. Either returns the number of ticks given to
  ## the X scale (e.g. via `scale_x_continuous` or friends) or returns a default of 10 ticks.
  result = fs.getXScale().getTicks()

proc getYTicks*(fs: FilledScales): int =
  ## Returns a valid number of ticks for Y. Either returns the number of ticks given to
  ## the Y scale (e.g. via `scale_y_continuous` or friends) or returns a default of 10 ticks.
  result = fs.getYScale().getTicks()

proc smallestPow*(invTrans: ScaleTransform, x: float): float =
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

proc largestPow*(invTrans: ScaleTransform, x: float): float =
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

proc computeLabel(tick: float, tickScale = 0.0,
                  fmt: ContinuousFormat = nil): string =
  if fmt != nil:
    result = fmt tick
  else:
    ## Hack: decrease tickScale by /5, cause hard coded in `ginger.nim` atm and
    ## too large (1/10). Value that is too small does not matter for the purpose,
    ## which is to aviod tiny O(1e-17) numbers to be counted as 0.
    result = formatTickValue(tick, tickScale / 5.0)

proc computeLabels(ticks: seq[float], tickScale = 0.0, fmt: ContinuousFormat = nil): seq[string] =
  ## computes the labels for the given ticks. Simply a string representation
  ## of the labels
  result = ticks.mapIt(it.computeLabel(tickScale, fmt))

proc tickPosTransformed*(boundScale: ginger.Scale,
                         trans, invTrans: ScaleTransform,
                         breaks: seq[float] = @[],
                         hideTickLabels = false,
                         format: proc(x: float): string): (seq[string], seq[float]) =
  ## Calculates the positions and labels of a log10 data scale given
  ## a min and max value. Takes into account a final bound scale outside
  ## of which no ticks may lie.
  ##
  ## The `boundScale` must be already transformed i.e. log10 values for scale_x_log10!
  var
    labs = newSeq[string]()
    labPos = newSeq[float]()
  if breaks.len == 0: # if no breaks given, compute manually
    var exp = floor(boundScale.low).int
    while exp.float < boundScale.high:
      let cur = invTrans(exp.float)
      if cur == Inf: break
      let numToAdd = invTrans(1.0).round.int
      let minors = linspace(cur, invTrans((exp + 1).float) - cur, numToAdd - 1)
      labPos.add minors.mapIt(trans(it))
      if (boundScale.high - boundScale.low) > 1.0 or hideTickLabels:
        if not hideTickLabels:
          labs.add cur.computeLabel(fmt = format)
        else:
          labs.add ""
        # add one less than minors.len of `""`
        labs.add(newSeqWith(minors.high, ""))
      else:
        # use all minors as labelledn
        labs.add minors.mapIt(it.computeLabel(fmt = format))
      inc exp
    # add the current exp to the labels (not in loop anymore). In log2 `maxv` is
    # contained in loop, but real range is larger. In log10, maxv is not contained.
    if not hideTickLabels:
      labs.add(computeLabel(invTrans(exp.float), fmt = format))
    else:
      labs.add ""
    labPos.add(exp.float)
  else: # use user given breaks. Will be given in data space, inverse transform
    labPos = breaks.mapIt(trans(it))
    labs = labPos.mapIt(computeLabel(invTrans(it), fmt = format))
  # for simplicity apply removal afterwards
  let filterIdx = toSeq(0 ..< labPos.len).filterIt(
    labPos[it] >= boundScale.low and
    labPos[it] <= boundScale.high
  )
  # apply filters to `labs` and `labPos`
  labs = filterIdx.mapIt(labs[it])
  labPos = filterIdx.mapIt(labPos[it])
  result = (labs, labPos)

proc tickPosLinear(scale: ginger.Scale, numTicks: int,
                   breaks: seq[float] = @[]): seq[float] =
  ## Computes the tick position and tick labels for non transformed data
  ##
  ## If `breaks` are given, these take precedent over our manual calculations.
  if breaks.len == 0:
    let (newScale, _, newNumTicks) = calcTickLocations(scale, numTicks)
    result = linspace(newScale.low, newScale.high, newNumTicks + 1).mapIt(it)
  else:
    result = breaks

proc applyBoundScale(ticks: seq[float], boundScale: ginger.Scale): seq[float] =
  ## filters the given tick positions by the `boundScale` so that now
  ## tick position remains outside that range
  result = ticks.filterIt(it >= boundScale.low and it <= boundScale.high)

proc getCorrectDataScale(view: Viewport, axKind: AxisKind): ginger.Scale =
  # first get base scale from `view`
  ## TODO: do we sometimes want the `dataScale` from `scale`?
  case axKind
  of akX: result = view.xScale
  of akY: result = view.yScale

proc applyScaleTrans*(scale: ginger.Scale, trans: Option[FormulaNode]): ginger.Scale =
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

proc toCoord1D*(ticks: seq[float], axKind: AxisKind, scale: ginger.Scale): seq[Coord1D] =
  ## returns the given ticks as `Coord1D`
  result = ticks.mapIt(Coord1D(pos: it, kind: ukData, scale: scale, axis: axKind))

proc handleContinuousTicks(view: Viewport, p: GgPlot, axKind: AxisKind,
                           dataScale: ginger.Scale,
                           scKind: ScaleKind,
                           numTicks: int, theme: Theme,
                           breaks: seq[float] = @[], # if given use these tick positions
                           trans, invTrans: ScaleTransform = nil,
                           secAxisTrans: Option[FormulaNode] = none[FormulaNode](),
                           format: ContinuousFormat = nil,
                           isSecondary = false,
                           hideTickLabels = false,
                           margin = none[Coord1D](),
                           tStyle = none[Style]()): seq[GraphObject] =
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
    var ticks = tickPosLinear(scale, numTicks, breaks)
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
                                              tickKind = theme.tickKind.get(tkOneSide),
                                              margin = margin,
                                              style = tStyle)
    if not hideTickLabels:
      view.addObj concat(tickObjs, labObjs)
    result = tickObjs
  of scTransformedData:
    let scale = dataScale.applyScaleTrans(secAxisTrans)
    ## `dataScale` is already transformed. Get min and max powers. `invTrans` gets
    ## back non transformed values, then gets power based on those. `smallest/largestPow`
    ## returns the *power* only, so inverse transform _those_ to get min/max values
    ## in non-transformed coordinates.
    let minVal = invTrans.smallestPow(invTrans(dataScale.low))
    let maxVal = invTrans.largestPow(invTrans(dataScale.high))
    let (labs, labelPos) = tickPosTransformed(boundScale, trans, invTrans,
                                              breaks = breaks,
                                              hideTickLabels = hideTickLabels,
                                              format = format)
    let tickLocs = labelPos.toCoord1D(axKind, dataScale)
    case axKind
    of akX: view.xScale = (low: trans(minVal), high: trans(maxVal))
    of akY: view.yScale = (low: trans(minVal), high: trans(maxVal))
    let (tickObjs, labObjs) = view.tickLabels(tickLocs, labs, axKind, isSecondary = isSecondary,
                                              rotate = rotate,
                                              alignToOverride = alignTo,
                                              font = theme.tickLabelFont,
                                              margin = margin,
                                              tickKind = theme.tickKind.get(tkOneSide),
                                              style = tStyle)
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
                          format: proc(x: Value): string,
                          tStyle = none[Style]()): seq[GraphObject] =
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
                                            margin = margin,
                                            tickKind = theme.tickKind.get(tkOneSide),
                                            style = tStyle)
  if not hideTickLabels:
    view.addObj concat(tickObjs, labObjs)
  result = tickObjs

proc getTickLabelMargin(view: Viewport, theme: Theme, axKind: AxisKind): Coord1D =
  ## takes the given tick label margin if user defined or else defines a suitable
  ## margin based on the font.
  var margin = 0.0
  case axKind ## XXX: update this for modern ginger!! But the otherwise branch shouldn't be encountered in theory
  of akX: margin = theme.xTickLabelMargin.get(1.75)
  of akY: margin = theme.yTickLabelMargin.get(-1.25)
  # if no default font, use 8pt
  let font = theme.tickLabelFont.get(font(8.0))
  ## 2.0 times the given string height. Making it string height dependent guarantees it's
  ## going to `appear` to be at the same distant no matter the facetting or size of the
  ## resulting plot
  ## Using `M` for a default height
  # TODO: possibly add `bearing` or `advance` to position? Need to adjust position at which
  # text is printed based on it I think?
  result = Coord1D(pos: margin, kind: ukStrHeight, backend: view.backend,
                   text: "M", font: font)
  case axKind
  of akX: result = result.toRelative(length = some(pointHeight(view)))
  of akY: result = result.toRelative(length = some(pointWidth(view)))

proc withoutIdxs[T](s: seq[T], idxs: HashSet[int]): seq[T] =
  ## Simple helper to return a sequence that does not contain the indices
  ## in `idxs`. This is easier to maintain than a mutating `while` loop with
  ## a bunch of new index computions (as indices change after each removal).
  result = newSeqOfCap[T](idxs.card)
  for i, x in s:
    if i notin idxs:
      result.add x

proc removeTicksWithinSpacing(ticks: var seq[float], tickLabels: var seq[string],
                              dateSpacing: Duration, timeZone: Timezone) =
  ## Remove all ticks that are not `dateSpacing` away from the last tick, so that we have
  ## ticks in a well spaced distance.
  ##
  ## This is the main logic associated to the `dtaFilter` algorithm for determination of
  ## date based tick labels.
  doAssert ticks.len > 0
  var curDur = ticks[0].int.fromUnix.inZone(timeZone)
  var lastDist: Duration
  var idxsToDelete = initHashSet[int]()
  # 1. pass over all ticks and compute indices to remove
  for i in 1 ..< ticks.len:         # start at index 1, 0 stored in `curDur`
    let tpU = ticks[i].int.fromUnix.inZone(timeZone)
    if abs(tpU - curDur) >= dateSpacing: # cross over point of dateSpacing
      # check whether the last one or this one was closer to `dateSpacing`
      if lastDist < abs((tpU - curDur) - dateSpacing): # keep last one
        idxsToDelete.excl i - 1 # ``exclude`` the last one as it was included last iter!
                                # else nothing to do, keep current one
        idxsToDelete.incl i     # but remove `i`
        curDur = ticks[i-1].int.fromUnix.inZone(timeZone) # new start last index
      else: curDur = tpU        # new start point this index
    else: idxsToDelete.incl i   # else delete index
    lastDist = abs((tpU - curDur) - dateSpacing)
  # 2. remove all indices that are marked as delete
  ticks = ticks.withoutIdxs(idxsToDelete)
  tickLabels = tickLabels.withoutIdxs(idxsToDelete)

proc computeTickPosByDateSpacing(firstTick, lastTick: DateTime,
                                 dateSpacing: Duration): seq[DateTime] =
  ## Compues the tick positions of the dates based on the first valid start
  ## date using the `dateSpacing`.
  ##
  ## This is the implementation of the `dateAlgo` `dtaAddDuration`.
  result = @[firstTick]
  var t = firstTick
  while t < lastTick:
    t = t + dateSpacing
    result.add t

proc handleDateScaleTicks*(view: Viewport, p: GgPlot, axKind: AxisKind, scale: Scale,
                           theme: Theme,
                           hideTickLabels = false,
                           margin = none[Coord1D](),
                           tStyle = none[Style]()): seq[GraphObject] =
  ## Handles generation of ticks that respect the `DateScale` object. Namely parses the data
  ## in the given axis according to `isTimestamp` or `parseDate` and then leaves only those
  ## ticks within `dateSpacing` according to `formatString`.
  var rotate: Option[float]
  var alignTo: Option[TextAlignKind]
  case axKind
  of akX:
    rotate = theme.xTicksRotate
    alignTo = theme.xTicksTextAlign
  of akY:
    rotate = theme.yTicksRotate
    alignTo = theme.yTicksTextAlign
  # get data for column
  let dateScale = scale.dateScale.get
  let timeZone = dateScale.timeZone
  var
    tickLabels: seq[string]
    tickPosUnix: seq[float]
  case dateScale.dateAlgo
  of dtaFilter:
    var data: seq[DateTime]
    if dateScale.isTimestamp:
      data = p.data[getColName(scale), int].map_inline(x.fromUnix().inZone(timeZone)).toSeq1D
    else:
      data = p.data[getColName(scale), string].map_inline(dateScale.parseDate(x)).toSeq1D
    let firstTick = data.min
    let offset = (firstTick - firstTick.format(dateScale.formatString).parse(dateScale.formatString))
    # using date time data, compute the ticks using `formatString`
    tickLabels = data.mapIt(it.format(dateScale.formatString)).deduplicate
    # unique values are now our ticks
    # compute their location
    tickPosUnix = tickLabels.mapIt((it.parse(dateScale.formatString) + offset).toTime.toUnixFloat)
    # remove those that are too close
    removeTicksWithinSpacing(tickPosUnix, tickLabels, dateScale.dateSpacing, timeZone)
  of dtaAddDuration:
    var
      firstTick: DateTime
      lastTick: DateTime
    if dateScale.isTimestamp:
      firstTick = p.data[getColName(scale), int].min.fromUnix().inZone(timeZone)
      lastTick  = p.data[getColName(scale), int].max.fromUnix().inZone(timeZone)
    else:
      # apply `parseDate` and sort the dates. `toSeq1D` required due to tensor `sorted` regression
      let dates = p.data[getColName(scale), string].map_inline(dateScale.parseDate(x)).toSeq1D.sorted
      firstTick = dates.min
      lastTick  = dates.max
    let offset = (firstTick - firstTick.format(dateScale.formatString).parse(dateScale.formatString))
    let tickPos = computeTickPosByDateSpacing(firstTick, lastTick,
                                              dateScale.dateSpacing)
      # make sure we didn't end up with more than we asked for (due to info loss)
      .filterIt(it >= firstTick and it <= lastTick)
    # using date time data, compute the ticks using `formatString`
    tickLabels = tickPos.mapIt(it.format(dateScale.formatString)).deduplicate
    # unique values are now our ticks
    # compute their location
    tickPosUnix = tickLabels.mapIt((it.parse(dateScale.formatString) + offset).toTime.toUnixFloat)
  of dtaCustomBreaks:
    tickPosUnix = dateScale.breaks
    tickLabels = tickPosUnix.mapIt(it.fromUnixFloat.inZone(timeZone).format(dateScale.formatString)).deduplicate
    if tickPosUnix.len == 0:
      raise newException(ValueError, "`dateAlgo` is dtaCustomBreaks, but no `breaks` are given " &
        "in the call to `scale_x/y_date`.")

  let tickCoord = tickPosUnix.toCoord1D(axKind, scale.dataScale) # convert to coordinates
  let (tickObjs, labObjs) = view.tickLabels(tickCoord, tickLabels, axKind, isSecondary = false,
                                            rotate = rotate,
                                            alignToOverride = alignTo,
                                            font = theme.tickLabelFont,
                                            margin = margin,
                                            tickKind = theme.tickKind.get(tkOneSide),
                                            style = tStyle)
  if not hideTickLabels:
    view.addObj concat(tickObjs, labObjs)
  result = tickObjs

proc tickStyle*(theme: Theme, width, height: float): Option[Style] =
  ## Note: currently we rescale the ticks such that at height 480 they have a length of 5
  ## and for higher / lower values scaled linearly.
  ##
  ## The tick width is 1/5 of the length, unless overwritten.
  let tickLength = theme.tickLength.get(5.0) #height / 480.0 * 5.0)
  let tickWidth  = theme.tickWidth.get(tickLength / 5.0)
  let tickColor  = theme.tickColor.get(color(0.0, 0.0, 0.0))
  result = some(Style(lineWidth: tickWidth,
                      color: tickColor,
                      size: tickLength, # total length of tick
                      lineType: ltSolid))


proc handleTicks*(view: Viewport, filledScales: FilledScales, p: GgPlot,
                  axKind: AxisKind, theme: Theme,
                  hideTickLabels = false,
                  numTicksOpt = none[int](),
                  boundScaleOpt = none[ginger.Scale]()): seq[GraphObject] =
  ## This handles the creation of the tick positions and tick labels.
  ## It automatically updates the x and y scales of both the viewport and the `filledScales`!
  ## `margin` is the tick label margin in centimeter!
  let tStyle = tickStyle(theme, view.wImg.val, view.hImg.val) # wImg/hImg is in points
  var marginOpt: Option[Coord1D]
  var scale: Scale
  var numTicks: int
  case axKind
  of akX:
    scale = filledScales.getXScale()
    numTicks = if numTicksOpt.isSome: numTicksOpt.unsafeGet else: scale.getTicks()
    if theme.xTickLabelMargin.isSome:
      marginOpt = some(view.getTickLabelMargin(theme, axKind))
  of akY:
    scale = filledScales.getYScale()
    numTicks = if numTicksOpt.isSome: numTicksOpt.unsafeGet else: scale.getTicks()
    if theme.yTickLabelMargin.isSome:
      marginOpt = some(view.getTickLabelMargin(theme, axKind))
  let hasScale = scale.col.name.len > 0
  if hasScale:
    case scale.dcKind
    of dcDiscrete:
      let format =
        if scale.formatDiscreteLabel != nil: scale.formatDiscreteLabel
        else: (proc(x: Value): string = $x)
      if scale.dateScale.isNone:
        result = view.handleDiscreteTicks(p, axKind, scale.labelSeq, theme = theme,
                                          hideTickLabels = hideTickLabels,
                                          margin = marginOpt,
                                          format = format,
                                          tStyle = tStyle)
      else:
        result = view.handleDateScaleTicks(p, axKind, scale, theme,
                                           hideTickLabels, marginOpt)
      if hasSecondary(filledScales, axKind):
        result.add view.handleDiscreteTicks(p, axKind, scale.labelSeq, theme = theme,
                                            isSecondary = true,
                                            hideTickLabels = hideTickLabels,
                                            margin = marginOpt,
                                            format = format,
                                            tStyle = tStyle)
    of dcContinuous:
      let dataScale = view.getCorrectDataScale(axKind)
      if scale.dateScale.isNone:
        result = view.handleContinuousTicks(p, axKind, dataScale,
                                            scale.scKind,
                                            numTicks,
                                            breaks = scale.breaks, # might be empty
                                            trans = scale.trans,
                                            invTrans = scale.invTrans,
                                            theme = theme,
                                            hideTickLabels = hideTickLabels,
                                            margin = marginOpt,
                                            format = scale.formatContinuousLabel,
                                            tStyle = tStyle)
      else:
        result = view.handleDateScaleTicks(p, axKind, scale, theme,
                                           hideTickLabels, marginOpt,
                                           tStyle = tStyle)
      if hasSecondary(filledScales, axKind):
        let secAxis = filledScales.getSecondaryAxis(axKind)
        # we discard the result, because we only use it to generate the grid lines. Those
        # are focused based on the primary axis of course
        let trans = if secAxis.scKind == scLinearData: secAxis.trans
                    else: none(FormulaNode)
        let transFn = if secAxis.scKind == scTransformedData: secAxis.transFn
                      else: nil
        let invTransFn = if secAxis.scKind == scTransformedData: secAxis.invTransFn
                      else: nil
        discard view.handleContinuousTicks(p, axKind, dataScale,
                                           secAxis.scKind,
                                           numTicks,
                                           breaks = scale.breaks, # might be empty
                                           trans = transFn,
                                           invTrans = invTransFn,
                                           secAxisTrans = trans,
                                           theme = theme,
                                           hideTickLabels = hideTickLabels,
                                           isSecondary = true,
                                           margin = marginOpt,
                                           format = scale.formatContinuousLabel,
                                           tStyle = tStyle)
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
    let ticks = view.initTicks(axKind, numTicks, boundScale = some(boundScale), style = tStyle,
                               tickKind = theme.tickKind.get(tkOneSide))
    var tickLabs: seq[GraphObject]
    tickLabs = view.tickLabels(ticks, font = theme.tickLabelFont,
                               margin = marginOpt)
    if not hideTickLabels:
      view.addObj concat(ticks, tickLabs)
    result = ticks
