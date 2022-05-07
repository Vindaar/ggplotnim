import ggplotnim

let
  pos = [1, 2, 3, 1, 2, 3]
  name = ["a very long long label", "a very long long label", "a very long long label", "b", "b", "b"]
  n = [0, 1, 4, 4, 2, 3]
  df = toDf(pos, name, n)

ggplot(df, aes("pos", "name")) +
  geom_tile(aes(fill = "n")) +
  geom_text(aes(text = "n"), size = 25.0) +
  scale_x_discrete() +
  scale_y_discrete() +
  margin(left = 6.0) +
  ggsave("media/recipes/rCustomMargins.png")
