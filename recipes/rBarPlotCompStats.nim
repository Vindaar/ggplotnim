import ggplotnim, sequtils, seqmath, strutils
let cols = toSeq(0 .. 7).mapIt($it)
# make `parseInt` work on Values, so we can parse the long form 
# `Channel` column
#liftScalarStringProc(parseInt)
let df = readCsv("data/szinti_channel_counts.txt",
                 sep = '\t',
                 colNames = cols)
  .gather(cols, key = "Channel", value = "Count")
  .mutate(f{string -> int: "Channel" ~ parseInt( df["Channel"][idx] )})
let dfMean = df.group_by("Channel").summarize(f{float: "Mean counts / min" << mean( c"Count" )})
# calculate mean for each channel
ggplot(dfMean, aes("Channel", "Mean counts / min")) +
  geom_bar(stat = "identity", position = "identity") +
  scale_x_discrete(name = "Channel number") +
  ggtitle("Mean counts per channel") +
  ggsave("media/recipes/rBarPlotCompStats.png")
