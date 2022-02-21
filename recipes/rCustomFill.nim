import ggplotnim, sequtils, seqmath, chroma, tables

let
  pos = [1, 2, 3, 1, 2, 3]
  name = ["a", "a", "a", "b", "b", "b"]
  n = [0, 1, 4, 4, 2, 3]
  df = seqsToDf(pos, name, n)

ggplot(df, aes("pos", "name")) +
  geom_tile(aes(fill = "n")) +
  geom_text(aes(text = "n"), size = 25.0) +
  scale_x_discrete() +
  scale_y_discrete() +
  scale_fill_manual({ 0 : color(1.0, 0.0, 0.0),
                      1 : color(0.0, 1.0, 0.0),
                      2 : color(0.0, 0.0, 1.0),
                      3 : color(1.0, 1.0, 0.0),
                      4 : color(1.0, 0.0, 1.0) }.toTable) +
  ggsave("media/recipes/rCustomFill.png")
