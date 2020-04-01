import ggplotnim, benchmarker

proc df_unique(df: var DataFrame) =
  discard df.unique("A")

proc df_unique_col(df: var DataFrame) =
  discard df["A"].unique

proc df_unique_col_alt(df: var DataFrame) =
  discard df["A"].unique_alt

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size) })

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for unique",
                          fname = "bench_unique.pdf")
benchmark.benchmark_all(df_unique, df_unique_col, df_unique_col_alt)
benchmark.print_results()
benchmark.plot_results()
