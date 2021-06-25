import ggplotnim, sequtils, seqmath
let df = readCsv("data/50-18004.CSV")
ggplot(df) +
  geom_line(aes(x = "in_s", y = "C1_in_V", color = "C1")) +
  geom_line(aes(x = "in_s", y = "C2_in_V", color = "C2")) +
  ggsave("media/recipes/rTwoSensorsBadStyle.png")
