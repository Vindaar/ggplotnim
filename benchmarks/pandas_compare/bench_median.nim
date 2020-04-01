import ggplotnim, benchmarker, arraymancer

proc df_summarize(df: var DataFrame) =
  discard df.summarize(f{int -> float: "median" << median(`A`)})

proc df_median_tensor(df: var DataFrame) =
  discard df["A"].toTensor(int).median

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size) })

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for median",
                          fname = "bench_median.pdf",
                          user_df_size_powers = @[2, 3, 4, 5, 6, 7], #, 8],
                          user_loop_size_powers = @[5, 5, 5, 5, 3, 2])#, 1])

benchmark.benchmark_all(df_summarize, df_median_tensor)
benchmark.print_results()
benchmark.plot_results()
