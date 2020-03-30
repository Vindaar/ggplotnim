import ggplotnim
let df = toDf(readCsv("data/mpg.csv"))
ggplot(df, aes(f{235 / c"cty"}, "displ")) + 
  geom_point() +
  xlab("cty [L / 100km]") +
  ggsave("media/recipes/rFormulaAesthetic.png")
