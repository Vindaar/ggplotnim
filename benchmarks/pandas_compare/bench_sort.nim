import ggplotnim, benchmarker

proc df_arrange(df: var DataFrame) =
  discard df.arrange("A")

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size) })

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for sorting",
                          fname = "bench_sort.pdf")

benchmark.benchmark_all(df_arrange)
benchmark.print_results()
benchmark.plot_results()
