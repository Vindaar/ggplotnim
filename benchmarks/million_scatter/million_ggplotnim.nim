import ggplotnim

proc main =
  var xt = randomTensor[float](1_000_000, 1.0)
  var yt = randomTensor[float](1_000_000, 1.0)

  let df = seqsToDf(xt, yt)
  ggplot(df, aes("xt", "yt")) +
    geom_point(size = some(0.1)) +
    ggsave("million.pdf")

when isMainModule:
  main()

# compiling with `-d:danger` on arraymancer backend.
# save as pdf:
# - 15.7 s
# save as PNG:
# - 3.6 s
