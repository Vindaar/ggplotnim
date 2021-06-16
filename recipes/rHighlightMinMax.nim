import ggplotnim, algorithm
# base this on one of the above examples
let df = readCsv("data/50-18004.CSV")
  .gather(["C1_in_V", "C2_in_V"], key = "Channel", value = "V")
# filter to Channel 2 and sort by voltage
let dfSorted = df.filter(f{c"Channel" == "C2_in_V"})
  .arrange("V", SortOrder.Descending)
# get min and max
let dfMax = dfSorted.head(1)
let dfMin = dfSorted.tail(1)
ggplot(df, aes("in_s", "V", color = "Channel")) +
  geom_line() + # the actual data
  # add additional geom with `data =` arg and set styles. 
  # NOTE: Style arguments use `Option[T]`!
  geom_point(data = dfMax,
             color = some(parseHex("FF0000")),
             size = some(10.0),
             marker = some(mkCross)) +
  geom_point(data = dfMin,
             color = some(parseHex("0000FF")),
             size = some(5.0)) +
  theme_opaque() +
  ggsave("media/recipes/rHighlightMinMax.png")
