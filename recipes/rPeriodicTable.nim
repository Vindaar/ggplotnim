import ggplotnim, sequtils, seqmath, strutils

##
## This is a straight up adaptation from the genius `plotnine` example
## here:
## https://plotnine.readthedocs.io/en/stable/generated/plotnine.geoms.geom_tile.html
##

var elements = readCsv("data/elements.csv")
  .mutate(f{Value -> int: "group" ~ (
    if `group` == "-": -1
    else: `group`.toInt)})
echo elements.pretty(5)

# split the lanthanides and actinides from the rest
var top = elements.filter(f{`group` != -1})
  .rename(f{"x" <- "group"},
          f{"y" <- "period"})
var bottom = elements.filter(f{`group` == -1})
echo top["x"]
echo top["y"]

const nrows = 2
const hshift = 3.5
const vshift = 3.0
bottom["x"] = toColumn cycle(arange(0, bottom.len div nrows), nrows).mapIt(it.float + hshift)
bottom = bottom.mutate(f{"y" ~ `period` + vshift})
const tile_width = 0.95
const tile_height = 0.95

# replace `elements` by stacked top and bottom
elements = bind_rows([top, bottom])

let splitDf = toDf({
  "y": @[6, 7],
  "metal": @["lanthanoid", "actinoid"]
})

func cycle[T](s: openArray[T]; nums: seq[int]): seq[T] =
  result = newSeq[T](nums.foldl(a + b))
  var idx = 0
  for i in 0 ..< nums.len:
    for j in 0 ..< nums[i]:
      result[idx] = s[i]
      inc idx
# finally define rows and cols
let groupdf = toDf({
    "group": arange(1, 19),
    "y": cycle(@[1, 2, 4, 2, 1], @[1, 1, 10, 5, 1])})
let periodDf = toDf({
    "period": arange(1, 8),
    "x": cycle(@[0.5], @[7])})

ggplot(elements, aes("x", "y", fill = "metal")) +
  geom_tile(aes = aes(width = tileWidth,
                      height = tileHeight)) +
  geom_tile(data = splitDf,
            aes = aes(x = 3 - tileWidth/4.0 + 0.25,
                      width = tileWidth / 2.0,
                      height = tileHeight)) +
  scale_y_continuous() +
  geom_text(aes(x = f{`x` + 0.15},
                y = f{`y` + 0.15},
                text = "atomic number"),
            font = font(6.0)) +
  geom_text(aes(x = f{`x` + 0.5},
                y = f{`y` + 0.4},
                text = "symbol"),
            font = font(9.0)) +
  geom_text(aes(x = f{`x` + 0.5},
                y = f{`y` + 0.6},
                text = "name"),
            font = font(4.5)) +
  geom_text(aes(x = f{`x` + 0.5},
                y = f{`y` + 0.8},
                text = "atomic mass"),
            font = font(4.5)) +
  geom_text(data = groupdf,
            aes = aes(x = f{`group` + 0.5},
                      y = f{`y` - 0.2},
                      text = "group"),
            font = font(9.0, color = color(0.5, 0.5, 0.5))) +
  geom_text(data = periodDf,
            aes = aes(x = f{`x` + 0.3},
                      y = f{`period` + 0.5},
                      text = "period"),
            font = font(9.0, color = color(0.5, 0.5, 0.5))) +
  legendPosition(0.82, 0.1) +
  theme_void() +
  margin(right = 5, bottom = 2) + # adjust margin as `theme_void` implies tight margins
  scale_y_reverse() +
  scale_x_continuous() +
  ggsave("media/recipes/rPeriodicTable.png",
         width = 1000,
         height = 500)
