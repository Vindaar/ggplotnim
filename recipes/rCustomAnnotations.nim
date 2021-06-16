import ggplotnim
import algorithm, sequtils, strformat, strutils
# get the data from one of the other recipes
let df = readCsv("data/50-18004.CSV")
let dfnew = df.gather(["C1_in_V", "C2_in_V"], key = "Channel", value = "V")
# assume we want to create an annotation that prints the largest 5 values of
# Channel 2; get largest values, sorted by time (`in_s`)
let dfChMax = dfNew.filter(f{c"Channel" == "C2_in_V"})
  .arrange("V", SortOrder.Descending)
  .head(5)
  .arrange("in_s") # sort again by x axis
# build an annotation:
var annot: string
let idxs = toSeq({'A'..'E'})
for j, id in idxs:
  let xVal = alignLeft(formatFloat(dfChMax["in_s", j, float], precision = 2), 9)
  let yVal = formatFloat(dfChMax["V", j, float], precision = 4)
  annot.add &"{id}: (x: {xVal}, y: {yVal})"
  if j < idxs.high:
    annot.add "\n"
# create a font to use using the `ggplotnim.font` helper
let font = font(12.0, family = "monospace")
# now create the plot and put the annotation where we want it
ggplot(dfNew, aes("in_s", "V", color = "Channel")) +
  geom_line() +
  # either for instance in relative coordinates of the plot viewport
  # Values smaller 0.0 or larger 1.0 work too. Puts the annotation outside 
  # of the plot
  annotate(annot,
           left = 0.5,
           bottom = 1.0,
           font = font) +
  # or in data coordinates using `(x, y)`
  annotate(annot,
           x = -2e-6,
           y = 0.06,
           font = font,
           backgroundColor = parseHex("FFEBB7")) +
  theme_opaque() +
  ggsave("media/recipes/rCustomAnnotations.png")
