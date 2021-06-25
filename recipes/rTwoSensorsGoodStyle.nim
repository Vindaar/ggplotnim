import ggplotnim, sequtils, seqmath
let df = readCsv("data/50-18004.CSV")
let dfnew = df.gather(["C1_in_V", "C2_in_V"], key = "Channel", value = "V")
# Plotting via `df` directly causes scale problems!
ggplot(dfNew, aes("in_s", "V", color = "Channel")) +
  geom_line() +
  ggsave("media/recipes/rTwoSensorsGoodStyle.png")
