import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes("hwy", "displ")) + 
  geom_point() +
  geom_text(aes(x = f{"hwy" + 0.3}, 
            text = "manufacturer"),
            alignKind = taLeft, 
            # font = some(font(10.0, ...)) <- you can also change the font
            ) + 
  ggsave("media/recipes/rAnnotateUsingGeomText.png")
