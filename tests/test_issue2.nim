import ggplotnim, ginger
import seqmath, sequtils
import unittest

const Backend = when defined(noCairo): bkDummy else: bkCairo

test "Issue2":
  let xdata = toSeq(0 ..< 2560)
  let ydata = xdata.mapIt(it.float * it.float)

  let plt = ggplot(seqsToDf({ "x" : xdata,
                              "y" : ydata }),
                   aes("x", "y"),
                   backend = Backend) +
    geom_line()

  let plotView = plt.ggcreate()
  let view = plotView.view
  let xScale = (low: 0.0, high: 3000.0)
  let yScale = (low: 0.0, high: 7000000.0)
  for ch in view.children:
    if ch.name == "plot":
      check ch.xScale == xScale # tick calculation modifies max
      check ch.yScale == yScale
      for el in ch.children:
        if el.name == "data":
          # data child must inherit the new scales from its plot parent
          check el.xScale == xScale
          check el.yScale == yScale
          for p in el.objects:
            # plot scales also must inherit its parent's scale
            check p.kind == goPolyLine
            for xy in p.plPos:
              check xy.x.kind == ukData
              check xy.x.scale == xScale
              check xy.y.scale == yScale

  # the scale assigned to the total plot does not have to be the one finally
  # TODO: decide if we want to make sure to update it regardless
  #check view.xScale == (low: 0.0, high: 3000.0) # tick calculation modifies max
  check view.yScale == yScale
  plt.ggsave("test_issue2.pdf")
