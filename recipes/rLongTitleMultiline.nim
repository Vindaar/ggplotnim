import ggplotnim, sequtils, seqmath

let x = linspace(0.0, 30.0, 1000)
let y = x.mapIt(pow(sin(it), 2.0))

let df = seqsToDf(x, y)

ggplot(df, aes("x", "y")) +
  geom_line() +
  margin(top = 2) +
  ggtitle("This is a very long title which gets cropped on the right side as it's longer than the image width.") +
  theme_opaque() +
  ggsave("media/recipes/rLongTitleMultiline.png")
