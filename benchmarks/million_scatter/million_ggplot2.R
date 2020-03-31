library(ggplot2)
x <- runif(1000000, min = 0, max = 1.0)
y <- runif(1000000, min = 0, max = 1.0)
df <- data.frame(x, y)
ggplot(df, aes(x, y)) +
    geom_point(size = 0.1) +
    ggsave("million_ggplot2.pdf")

# save as pdf
# - 15.5 s
# save as png
# - can't save because of some font error.
