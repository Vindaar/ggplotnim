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
  geom_point(data = dfMax,
             color = "#FF0000", # named colors (e.g. "red") are also possible!
             size = 5.0,
             marker = mkCross) +
  geom_point(data = dfMin,
             color = "#0000FF",
             size = 5.0) +
  ggsave("media/recipes/rHighlightMinMax.png")
