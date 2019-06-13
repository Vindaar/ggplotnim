

# Random plots that are known to work
# works now, identity works too

ggplot(mpg, aes("cty", color = "class")) +
  geom_freqpoly(position = "stack") +
  ggsave("figs/1.pdf")

ggplot(mpg, aes("cty", color = "class")) +
  geom_freqpoly(position = "identity") +
  ggsave("figs/2.pdf")
