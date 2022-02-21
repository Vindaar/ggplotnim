import ggplotnim
import seqmath # for gauss

# generate some gaussian 2D data
var
  xs = newSeq[float]()
  ys = newSeq[float]()
  zs = newSeq[float]()
let coords = linspace(-1.0, 1.0, 200)
for y in coords:
  for x in coords:
    xs.add x
    ys.add y
    zs.add gauss(sqrt(x*x + y*y), mean = 0.0, sigma = 0.2) # small sigma to cover multiple σ

# get the Inferno colormap    
var customInferno = inferno()
customInferno.name = "InfernoWithTransparent"
# and assign the 0th element to exact transparent
customInferno.colors[0] = 0 # transparent

ggplot(toDf(xs, ys, zs), aes("xs", "ys", fill = "zs")) +
  geom_raster() +
  xlim(-1, 1) + ylim(-1, 1) +
  scale_fill_gradient(customInferno) +
  ggtitle("Modified Inferno colormap with 0 set to transparent") + 
  ggsave("media/recipes/rCustomColormap.png")
