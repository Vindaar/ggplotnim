import ggplotnim
let df = toDf(readCsv("data/run_305_tpa_data.csv"))
# gather all columns to a long format df
echo df
let dfLong = df.gather(getKeys(df), key = "Property", value = "Value")
ggplot(dfLong, aes("Value")) +
  facet_wrap("Property", 
             scales = "free") + # each property has very different data ranges, Leave free
  geom_histogram(bins = 100, position = "identity", 
                 binBy = "subset") + # `binBy subset` means the histogram will be calculated 
                                     # in the data range of each properties data range
  theme_opaque() +
  ggsave("media/recipes/rFacetTpa.png", width = 800, height = 600)
