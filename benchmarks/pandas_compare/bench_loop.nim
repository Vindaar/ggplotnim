import ggplotnim, benchmarker

proc df_iter_as_value(df: var DataFrame) =
  for row in df:
    discard

proc df_slice_df(df: var DataFrame) =
  for idx in 0 ..< df.high:
    discard df[idx ..< idx + 1]

proc df_values(df: var DataFrame) =
  for row in values(df):
    discard

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size),
                      "C" : randomTensor(df_size, df_size),
                      "D" : randomTensor(df_size, df_size)})

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for iterating over rows",
                          fname = "bench_loop.pdf",
                          user_df_size_powers = @[2, 3, 4, 5, 6],
                          user_loop_size_powers = @[2, 2, 1, 1, 1])
benchmark.benchmark_all(df_iter_as_value, df_slice_df, df_values)
benchmark.print_results()
benchmark.plot_results()
