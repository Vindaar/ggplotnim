import ggplotnim 
let df = toDf(readCsv("data/mpg.csv"))
let dfMax = df.mutate(f{"mpgMean" ~ (`cty` + `hwy`) / 2.0})
  .arrange("mpgMean")
  .tail(1)
ggplot(df, aes("hwy", "displ")) + 
  geom_point(aes(color = "cty")) + # set point specific color mapping
  # Add the annotation for the car model below the point
  geom_text(data = dfMax,
            aes = aes(y = f{c"displ" - 0.2}, 
                      text = "model")) +
  # and add another annotation of the mean mpg above the point
  geom_text(data = dfMax,
            aes = aes(y = f{c"displ" + 0.2}, 
                      text = "mpgMean")) +
  theme_opaque() +
  ggsave("media/recipes/rAnnotateMaxValues.png")
