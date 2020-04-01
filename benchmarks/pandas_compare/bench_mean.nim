import ggplotnim, benchmarker, arraymancer

proc df_summ_as_int(df: var DataFrame) =
  discard df.summarize(f{int -> int: "mean" << mean(`A`)})

proc df_summ_as_float(df: var DataFrame) =
  discard df.summarize(f{float -> float: "mean" << mean(`A`)})

proc df_mean_int_tensor(df: var DataFrame) =
  discard df["A"].toTensor(int).mean

proc df_mean_float_tensor(df: var DataFrame) =
  discard df["A"].toTensor(float).mean

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size) })

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for mean",
                          fname = "bench_mean.pdf")

benchmark.benchmark_all(df_summ_as_int, df_summ_as_float, df_mean_int_tensor, df_mean_float_tensor)
benchmark.print_results()
benchmark.plot_results()
