import ggplotnim
let df = readCsv("data/commits_nimble.csv")
ggplot(df, aes("days", "count")) +
  geom_line() + # plot the raw data as a line
  geom_smooth() + # draw a default smoother. This is a Saitzky-Golay filter of
                  # order 5 and a window `span` of 70%
  geom_smooth(smoother = "poly", # add a polynomial smoother using the full range
              polyOrder = 7,     # of order 7
              color = some(parseHex("FF0000")), # and red line
              size = some(1.0)) + # that is not as thick
  ggsave("media/recipes/rGeomSmooth.png")
