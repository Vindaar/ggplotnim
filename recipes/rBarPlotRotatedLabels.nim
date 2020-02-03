import ggplotnim
let df = toDf(readCsv("data/fake_shifter_data.txt"))
ggplot(df, aes("Shifters", fill = "Year")) +
  geom_bar() +
  xlab(rotate = -45.0, margin = 1.75, alignTo = "right") +
  ggtitle("Number of shifts done by each shifter by year") +
  ggsave("media/recipes/rBarPlotRotatedLabels.png", width = 5000, height = 1000)
