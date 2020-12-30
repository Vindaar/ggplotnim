import ggplotnim, seqmath, sequtils
# create some polynomial data 
let x = linspace(0, 1.0, 10)
let y = x.mapIt(0.5 * it - 1.2 * it * it + 1.1 * it * it * it)
let df = seqsToDf(x, y)
# let's assume we have asymmetric errors, 0.03 down and 0.05 up
ggplot(df, aes("x", "y")) +
  geom_point() +
  # define errors as a formula, which references our "y" scale
  geom_errorbar(aes(yMin = f{`y` - 0.03}, yMax = f{`y` + 0.05})) +
  theme_opaque() +
  ggsave("media/recipes/rErrorBar.png")
