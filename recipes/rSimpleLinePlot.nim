import ggplotnim, strformat, math

#[
This example shows how to create a simple line plot, which is:
- saved as a PNG
- with width = 720, height = 480
- and shows a single black line for a sin
]#

const
  width = 720
  height = 480

# let's create some data.
var
  # need the data as two sequences (well actually as a DataFrame, but that is
  # created most easily from two or more sequences).
  x: seq[float]
  y: seq[float]
for i in 0 ..< 1000:
  let pos = 2 * PI / 100.0 * i.float
  x.add pos
  y.add sin(pos)
# the actual
# create a dataframe from the data. The column names will be the identifiers
# of the given sequences, so "x", "y"
let df = seqsToDf(x, y)
# if one wants to use a different name, use the following:
# let df = seqsToDf({ "myX" : x,
#                     "myY" : y })
# Note that in this case the args of `aes` would have to be changed of course!
# now create the plot
ggplot(df, aes("x", "y")) +
  geom_line() +
  ggsave("rSimpleLinePlot.png", width = width, height = height)
