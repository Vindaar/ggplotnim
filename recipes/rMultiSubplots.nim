import ggplotnim, seqmath, math, sequtils, complex, ginger
let t = linspace(-0.02, 0.05, 1000)
let y1 = t.mapIt(exp(im(2'f64) * Pi * 50 * it).re)
let y2 = t.mapIt(exp(im(2'f64) * Pi * 50 * it).im)
let df = seqsToDf({ "t" : t,
                    "Re x(t)" : y1,
                    "Im x(t)" : y2 })
let plt1 = ggcreate(
  ggplot(df, aes("t", "Re x(t)"),
         backend = bkCairo) + # tell `ggsave` we wish to create the plot on the cairo backend
    geom_line() + 
    xlim(-0.02, 0.05) + 
    ggtitle("Real part of x(t)=e^{j 100 π t}"),
  width = 800, height = 300
)
let plt2 = ggcreate(
  ggplot(df, aes("t", "Im x(t)"),
         backend = bkCairo) +
    geom_line() + 
    xlim(-0.02, 0.05) + 
    ggtitle("Imaginary part of x(t)=e^{j 100 π t}"),
  width = 800, height = 300
)
# combine both into a single viewport to draw as one image
var plt = initViewport(wImg = 800, hImg = 600, backend = bkCairo) # set backend of this viewport too
plt.layout(1, rows = 2)
# embed the finished plots into the the new viewport
plt.embedAt(0, plt1.view)
plt.embedAt(1, plt2.view)
plt.draw("media/recipes/rMultiSubplots.png")
