import ggplotnim

let trials = @["A", "B", "C", "D", "E"]
let values = @[1.0, 0.5, 0, -0.5, -1.0]

let df = seqsToDf({ "Trial" : trials,
                    "Value" : values })

ggplot(df, aes(x="Trial", y="Value")) +
  geom_bar(stat="identity", position="identity") +
  theme_opaque() +
  ggsave("media/recipes/rNegativeBarPlot.png")
