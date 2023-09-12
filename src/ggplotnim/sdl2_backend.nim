import std / [math, os]

import sdl2 except Color, Point

import ginger
import cairo

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
    yMax = 0.0
    xn = 0
    yn = 0
    sizesSet = false
    requireRedraw = true
    xScale: ginger.Scale
    yScale: ginger.Scale
    plotOrigin: Point
    plotWidth: int
    plotHeight: int
    zoomStart: Point
    zoomEnd: Point

proc calcNewScale(orig: float, length: int, startIn, stopIn: float, scale: ginger.Scale): ginger.Scale =
  let start = min(startIn, stopIn).float
  let stop = max(startIn, stopIn).float
  let length = length.float
  if start >= orig and start <= orig + length:
    ## Inside the plot
    let stop = clamp(stop, start, orig + length)
    let rel1 = (start - orig) / length
    let rel2 = (stop - orig) / length
    let dif = (scale.high - scale.low)
    result = (low: dif * rel1 + scale.low, high: dif * rel2 + scale.low)
  else:
    result = scale

proc calcNewScale(ctx: Context, axis: AxisKind): ginger.Scale =
  if ctx.sizesSet and ctx.zoomStart != ctx.zoomEnd:
    case axis
    of akX: result = calcNewScale(ctx.plotOrigin[0], ctx.plotWidth, ctx.zoomStart[0], ctx.zoomEnd[0], ctx.xScale)
    of akY:
      result = calcNewScale(ctx.plotOrigin[1], ctx.plotHeight, ctx.zoomStart[1], ctx.zoomEnd[1], ctx.yScale)
      # now invert
      result = (low: ctx.yScale.high - result.high, high: ctx.yScale.high - result.low)
  else:
    case axis
    of akX: result = (low: 0.float, high: ctx.xn.float)
    of akY: result = (low: 0.float, high: ctx.yn.float)

proc renderPlot(ctx: var Context, renderer: RendererPtr, surface: SurfacePtr) =
  if ctx.requireRedraw:
    let xScale = calcNewScale(ctx, akX)
    let yScale = if ctx.sizesSet: calcNewScale(ctx, akY)
                 else: (low: 0.float, high: ctx.yn.float)
    var ggplt = ctx.plt
    if xScale.high != xScale.low:
      ggplt = ggplt + xlim(xScale[0], xScale[1])
    if yScale.high != yScale.low:
      ggplt = ggplt + ylim(yScale[0], yScale[1])
    let plt = ggcreate(ggplt, 640.0, 480.0)

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
          let orig = ch.origin.to(ukPoint, absWidth = some(ch.wImg), absHeight = some(ch.hImg))
          ctx.plotOrigin = (orig.x.pos, orig.y.pos)
          ctx.plotWidth  = (times(ch.wView, ch.width)).toPoints().val.round.int
          ctx.plotHeight  = (times(ch.hView, ch.height)).toPoints().val.round.int
          ctx.sizesSet = true
          #echo ctx

    renderer.draw(surface, plt.view, "")
    ctx.requireRedraw = false

proc initContext(plt: GgPlot): Context =
  result = Context(plt: plt)

proc render(ctx: var Context, width, height: int) =
  discard sdl2.init(INIT_EVERYTHING)
  var screen = sdl2.createWindow("Interactive".cstring,
                                 SDL_WINDOWPOS_UNDEFINED,
                                 SDL_WINDOWPOS_UNDEFINED,
                                 width.cint, height.cint,
                                 SDL_WINDOW_OPENGL);
  var renderer = sdl2.createRenderer(screen, -1, 1)
  if screen.isNil:
    quit($sdl2.getError())

  var quit = false
  var event = sdl2.defaultEvent

  var window = sdl2.getsurface(screen)

  ## XXX: IMPLEMENT change of vertical field of view using mouse wheel! sort of a zoom
  while not quit:
    var anyEvents = false
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
        echo "Clicked at: ", (ev.x, ev.y)
        ctx.zoomStart = (ev.x.float, ev.y.float)
        ctx.zoomEnd   = (ev.x.float, ev.y.float)
      of MousebuttonUp:
        ## activate relative mouse motion
        let ev = evMouseButton(event)
        echo "Liftet at: ", (ev.x, ev.y)
        ctx.zoomEnd = (ev.x.float, ev.y.float)
        ctx.requireRedraw = true
      of WindowEvent:
        freeSurface(window)
        window = sdl2.getsurface(screen)
      of MouseMotion:
        ## for now just take a small fraction of movement as basis
        discard
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

proc `+`*(p: GgPlot, sdlDraw: SdlDraw) =
  var plt = p
  plt.backend = bkCairo
  var ctx = initContext(plt)
  ctx.render(sdlDraw.width, sdlDraw.height)

proc ggshow*(width = 640, height = 480): SdlDraw =
  result = SdlDraw(width: width, height: height)
