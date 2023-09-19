import std / [math, os]
import sdl2 except Color, Point

import pkg / [ggplotnim, ginger, cairo]

# Zoom mentioned these two libs as options for file dialogs
# > I forgot which one worked better, https://github.com/Patitotective/tinydialogs or https://github.com/Tormund/os_files last time I checked

proc draw*(renderer: RendererPtr, sdlSurface: SurfacePtr, view: Viewport, filename: string, texOptions: TeXOptions = TeXOptions()) =
  var img = initBImage(CairoBackend,
                       filename,
                       width = view.wImg.val.round.int, height = view.hImg.val.round.int,
                       ftype = fkSvg,
                       texOptions = texOptions)

  ## XXX: overwrite `img.surface` by
  img.backend.cCanvas = image_surface_create(cast[cstring](sdlSurface.pixels),
                                             FORMAT_RGB24,
                                             sdl_surface.w,
                                             sdl_surface.h,
                                             sdl_surface.pitch)

  img.draw(view)
  #img.backend.ctx.paint()
  #img.destroy()
  let texture = createTextureFromSurface(renderer, sdl_surface)
  freeSurface(sdl_surface)
  # copy the surface to the renderer
  copy(renderer, texture, nil, nil)

#[
1. when update in while true loop, update coordinates based on zoom etc
2. produce ggplot using `ggcreate`
3. pass surface pointer to `draw`
4. copy back to SDL? Needed? probably not.
]#

type
  Context = object
    plt: GgPlot
    sizesSet = false
    requireRedraw = true
    # the initial scales when the plot is created as is
    xScaleInit: ginger.Scale
    yScaleInit: ginger.Scale
    # the current scales after zooming etc
    xScale: ginger.Scale
    yScale: ginger.Scale
    plotOrigin: Point
    plotWidth: int
    plotHeight: int
    zoomStart: Point # rectangle zoom (right mouse button) from
    zoomEnd: Point   # rectangle zoom (right mouse button) to
    # sizes of the window, also size of the plot!
    width: int
    height: int
    # mouse buttons
    mouseDown = false
    pan = false # currently panning: left button clicked
    panTo: Point # panning target
    mousePos: Point # current mouse position
    resetZoom = false # reset zoom? -> right button click without movement
    mouseZoom: int # mouse wheel zoom amount

proc zoom(orig: float, length: int, startIn, stopIn: float, scale: ginger.Scale): ginger.Scale =
  ## Zooms the view.
  let start = min(startIn, stopIn).float
  let stop = max(startIn, stopIn).float
  let length = length.float
  if start >= orig and start <= orig + length:
    ## Inside the plot
    let stop = clamp(stop, start, orig + length)
    let rel1 = (start - orig) / length
    let rel2 = ( stop - orig) / length
    let dif = (scale.high - scale.low)
    result = (low: dif * rel1 + scale.low, high: dif * rel2 + scale.low)
  else:
    result = scale

proc zoom(ctx: Context, axis: AxisKind): ginger.Scale =
  case axis
  of akX: result = zoom(ctx.plotOrigin[0], ctx.plotWidth, ctx.zoomStart[0], ctx.zoomEnd[0], ctx.xScale)
  of akY:
    result = zoom(ctx.plotOrigin[1], ctx.plotHeight, ctx.zoomStart[1], ctx.zoomEnd[1], ctx.yScale)
    # now invert (origin is at top)
    result = (low: (ctx.yScale.high) - result.high + ctx.yScale.low, high: (ctx.yScale.high) - result.low + ctx.yScale.low)

proc pan(start, stop: float, length: int, scale: ginger.Scale): ginger.Scale =
  ## Translates the position
  let scDif = scale.high - scale.low
  let dif = (start - stop) / length.float * scDif
  result = (low: scale.low + dif, high: scale.high + dif)

proc pan(ctx: var Context, axis: AxisKind): ginger.Scale =
  # 1. compute center of current window
  # 2. compute center of pan target
  case axis
  of akX: result = pan(ctx.zoomStart[0], ctx.panTo[0], ctx.plotWidth, ctx.xScale)
  # invert arguments as we move in opposite direction
  of akY: result = pan(ctx.panTo[1], ctx.zoomStart[1], ctx.plotHeight, ctx.yScale)

const ZoomFactor {.intdefine.} = 10 # 5 %
proc mouseZoom(ctx: var Context, axis: AxisKind): ginger.Scale =
  let zoomFactor = ctx.mouseZoom.float * (ZoomFactor.float / 100.0)
  # 1. pan to the point where the mouse is
  let start = (ctx.plotOrigin[0] + ctx.plotWidth.float / 2.0, ctx.plotOrigin[1] + ctx.plotHeight.float / 2.0)
  ctx.panTo = start
  ctx.zoomStart = ctx.mousePos
  case axis
  of akX:
    ctx.xScale = ctx.pan(axis) # pan
    # 2. perform the zoom
    let scDif = (ctx.xScale.high - ctx.xScale.low)
    let zoomDif = scDif * (zoomFactor)
    ctx.xScale = (low: ctx.xScale.low + zoomDif / 2.0, high: ctx.xScale.high - zoomDif / 2.0)
  of akY:
    ctx.yScale = ctx.pan(axis) # pan
    # 2. perform the zoom
    let scDif = (ctx.yScale.high - ctx.yScale.low)
    let zoomDif = scDif * (zoomFactor)
    ctx.yScale = (low: ctx.yScale.low + zoomDif / 2.0, high: ctx.yScale.high - zoomDif / 2.0)
  # 3. pan back to the original point
  ctx.panTo = ctx.zoomStart
  ctx.zoomStart = start
  result = ctx.pan(axis)

proc calcNewScale(ctx: var Context, axis: AxisKind): ginger.Scale =
  if ctx.pan: # panning
    result = ctx.pan(axis)
    case axis
    of akX: ctx.zoomStart[0] = ctx.panTo[0]
    of akY: ctx.zoomStart[1] = ctx.panTo[1]
  elif ctx.mouseZoom != 0: # need to mouse zoom
    result = mouseZoom(ctx, axis)
  elif not ctx.pan and ctx.sizesSet and ctx.zoomStart != ctx.zoomEnd:
    result = ctx.zoom(axis)
  elif ctx.resetZoom:
    result = (low: 0.0, high: 0.0)
  else:
    case axis
    of akX: result = ctx.xScale
    of akY: result = ctx.yScale

proc renderPlot(ctx: var Context, renderer: RendererPtr, surface: SurfacePtr) =
  if ctx.requireRedraw:
    # Compute new scales
    ctx.xScale = calcNewScale(ctx, akX)
    ctx.yScale = calcNewScale(ctx, akY)
                 #else: (low: 0.0, high: 0.0)
    var ggplt = ctx.plt
    if ctx.xScale.high != ctx.xScale.low:
      ggplt = ggplt + xlim(ctx.xScale[0], ctx.xScale[1])
    elif ctx.resetZoom:
      ctx.xScale = ctx.xScaleInit
    if ctx.yScale.high != ctx.yScale.low:
      ggplt = ggplt + ylim(ctx.yScale[0], ctx.yScale[1])
    elif ctx.resetZoom:
      ctx.yScale = ctx.yScaleInit
    var plt: PlotView
    try:
      plt = ggcreate(ggplt, ctx.width, ctx.height)
      if not ctx.sizesSet:
        ## 1. get coordinates of the plot viewport by converting `origin` to ukPoint, same width/height
        ## 2. determine if zoom inside/outside
        ## 3. convert relative zoom in x / y of total scale to ginger.scale using xScale and yScale
        ## 4. add a `xlim`, `ylim` to call
        for ch in plt.view:
          if ch.name == "plot":
            # echo ch.origin, "  ", ch.width, "  ", ch.height, "  ", ch.wImg, "  ", ch.hImg, "  ", ch.wView, "  ", ch.hView, "  xSc ", ch.xScale, "  ", ch.yScale
            ctx.xScale = ch.xScale
            ctx.yScale = ch.yScale
            if ctx.xScaleInit == (0.0, 0.0): ## Only assign initial scales a single time!
              ctx.xScaleInit = ctx.xScale
              ctx.yScaleInit = ctx.yScale
            let orig = ch.origin.to(ukPoint, absWidth = some(ch.wImg), absHeight = some(ch.hImg))
            ctx.plotOrigin = (orig.x.pos, orig.y.pos)
            ctx.plotWidth  = (times(ch.wView, ch.width)).toPoints().val.round.int
            ctx.plotHeight  = (times(ch.hView, ch.height)).toPoints().val.round.int
            ctx.sizesSet = true
            #echo ctx

      renderer.draw(surface, plt.view, "")
      ctx.requireRedraw = false
    except ValueError: ## `ValueError` will be thrown when the window is too small to hold the plot
      discard

proc initContext(plt: GgPlot): Context =
  result = Context(plt: plt)

proc render(ctx: var Context, width, height: int) =
  discard sdl2.init(INIT_EVERYTHING)
  var screen = sdl2.createWindow("Interactive".cstring,
                                 SDL_WINDOWPOS_UNDEFINED,
                                 SDL_WINDOWPOS_UNDEFINED,
                                 width.cint, height.cint,
                                 SDL_WINDOW_OPENGL or SDL_WINDOW_RESIZABLE);
  var renderer = sdl2.createRenderer(screen, -1, 1)
  if screen.isNil:
    quit($sdl2.getError())

  var quit = false
  var event = sdl2.defaultEvent

  var window = sdl2.getsurface(screen)

  ## XXX: IMPLEMENT change of vertical field of view using mouse wheel! sort of a zoom
  while not quit:
    var anyEvents = false
    ctx.mouseZoom = 0 # reset mouse zoom after each iteration
    while pollEvent(event):
      anyEvents = true
      case event.kind
      of QuitEvent:
        quit = true
      of KeyDown:
        const dist = 1.0
        case event.key.keysym.scancode
        of SDL_SCANCODE_LEFT, SDL_SCANCODE_RIGHT, SDL_SCANCODE_A, SDL_SCANCODE_D:
          discard
        else: discard
      of MousebuttonDown:
        ## activate relative mouse motion
        let ev = evMouseButton(event)
        echo "Clicked at: ", (ev.x, ev.y), " button: ", ev.button
        case ev.button
        of 1, 3: # left click, zoom
          ctx.zoomStart = (ev.x.float, ev.y.float)
          ctx.zoomEnd   = (ev.x.float, ev.y.float)
          ctx.mouseDown = true
          ctx.pan = ev.button == 1
        #of 3: # right click, pan
        else: discard
      of MousebuttonUp:
        ## activate relative mouse motion
        let ev = evMouseButton(event)
        echo "Lifted at: ", (ev.x, ev.y), " button: ", ev.button
        case ev.button
        of 3: # right click, zoom
          ctx.zoomEnd = (ev.x.float, ev.y.float)
          ctx.resetZoom = ctx.zoomStart == ctx.zoomEnd
          ctx.requireRedraw = true
        of 1: # left click, pan
          ctx.zoomEnd = ctx.zoomStart
          ctx.resetZoom = false
          ctx.mouseDown = false
          ctx.pan = false
        else: discard
        #ctx.sizesSet = false # need to update the sizes
      of MouseMotion:
        # pan from `zoomStart` to current position
        let ev = evMouseMotion(event)
        ctx.mousePos = (ev.x.float, ev.y.float)
        if ctx.mouseDown and ctx.pan:
          ctx.panTo = ctx.mousePos
          ctx.requireRedraw = true
          ctx.resetZoom = false
      of MouseWheel:
        # Zoom at current mouse position
        let ev = evMouseWheel(event)
        ctx.mouseZoom = ev.y
        ctx.requireRedraw = true
      of WindowEvent:
        freeSurface(window)
        window = sdl2.getsurface(screen)
        if window.w != ctx.width or window.h != ctx.height:
          ctx.sizesSet = false # need to re read the sizes!
          ctx.width = window.w
          ctx.height = window.h
          ctx.requireRedraw = true
      else: echo event.kind
    #discard lockSurface(window)

    ## rendering of this frame
    renderPlot(ctx, renderer, window)

    #unlockSurface(window)
    #sdl2.clear(arg.renderer)
    sdl2.present(renderer)
    if not anyEvents:
      sleep(10)
  sdl2.quit()

type
  SdlDraw* = object
    width: int
    height: int
    draw: Draw

proc `+`*(p: GgPlot, sdlDraw: SdlDraw) =
  # 1. create the output file (if filename given)
  if sdlDraw.draw.fname.len > 0:
    p + sdlDraw.draw
  # 2. render the plot
  var plt = p
  plt.backend = bkCairo
  var ctx = initContext(plt)
  ctx.render(sdlDraw.width, sdlDraw.height)

proc ggshow*(fname: string,
             width = 640.0, height = 480.0,
             useTeX = false,
             onlyTikZ = false,
             standalone = false,
             texTemplate = "",
             caption = "",
             label = "",
             placement = "htbp",
             backend = bkNone): SdlDraw =
  ## Overload of `ggshow` which also produces a regular plot.
  let texOptions = toTeXOptions(useTeX, onlyTikZ, standalone, texTemplate,
                                caption, label, placement)
  let draw = Draw(fname: fname,
                  width: some(width),
                  height: some(height),
                  texOptions: texOptions,
                  backend: backend)
  result = SdlDraw(width: width.round.int, height: height.round.int, draw: draw)

proc ggshow*(width = 640, height = 480): SdlDraw =
  ## Creates an SDL2 window in which the plot will be shown. Basic mouse interaction is possible
  ## to pan and zoom (mouse wheel and right click -> zoom to rectangle).
  result = SdlDraw(width: width, height: height)
