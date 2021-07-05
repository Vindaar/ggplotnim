import ggplotnim, unittest

let dataCsv = """
w1,w2,w3,w4
4,0,0,2
39,1,5,2
11,1,0,2
"""

var df = parseCsvString(dataCsv)
df = df.gather(df.getKeys(), "Types", "Values")
try:
  ggplot(df, aes("Types", fill = "Values", color = "Values")) +
    geom_bar() + ggsave("/tmp/img1.png")
except ValueError as e:
  check e.msg == "Cannot perform continuous action using the following formulae: [(Column: Values, kind: scColor), (Column: Values, kind: scFillColor)] in a `count` statistics on column `Types`."
