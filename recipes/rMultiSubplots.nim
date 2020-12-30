import ggplotnim, seqmath, math, sequtils, complex, ginger
let t = linspace(-0.02, 0.05, 1000)
let y1 = t.mapIt(exp(im(2'f64) * Pi * 50 * it).re)
let y2 = t.mapIt(exp(im(2'f64) * Pi * 50 * it).im)
let df = seqsToDf({ "t" : t,
                    "Re x(t)" : y1,
                    "Im x(t)" : y2 })
let plt1 = ggcreate(
  ggplot(df, aes("t", "Re x(t)")) + 
    geom_line() + 
    xlim(-0.02, 0.05) + 
    theme_opaque() +
    ggtitle("Real part of x(t)=e^{j 100 π t}"),
  width = 800, height = 300
)
let plt2 = ggcreate(
  ggplot(df, aes("t", "Im x(t)")) + 
    geom_line() + 
    xlim(-0.02, 0.05) + 
    theme_opaque() +
    ggtitle("Imaginary part of x(t)=e^{j 100 π t}"),
  width = 800, height = 300
)
# combine both into a single viewport to draw as one image
var plt = initViewport(wImg = 800, hImg = 600)#wImg = 800.0, hImg = 800)
plt.layout(1, rows = 2)
# embed the finished plots into the the new viewport
plt.embedAt(0, plt1.view)
plt.embedAt(1, plt2.view)
plt.draw("media/recipes/rMultiSubplots.png")
