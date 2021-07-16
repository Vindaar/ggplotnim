import ggplotnim, times
var df = readCsv("data/commits_nimble.csv")
ggplot(df, aes("days", "count")) +
  geom_line() +
  scale_x_date(isTimestamp = true,                       # x is unix timestamp
               formatString = "MMM-yyyy",                # format as 'Jan-1970'
               dateSpacing = initDuration(days = 1,      # one year is roughly
                                          weeks = 52)) + # 365.25 days, but we use 365
  geom_smooth(span = 0.64) +
  ylab("Commit count") + xlab("Date") +
  ggtitle("Daily commit counts in all nimble repositories") +
  ggsave("media/recipes/rScaleXDate.png")
