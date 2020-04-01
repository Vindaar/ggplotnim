import ggplotnim, benchmarker

proc df_filter(df: var DataFrame) =
  discard df.filter(f{int: `A` > 0 and `A` < 100})

#proc df_filter_alt(df: var DataFrame) =
#  discard df.filterAlt(f{int: `A` > 0 and `A` < 100})

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size),
                      "C" : randomTensor(df_size, df_size),
                      "D" : randomTensor(df_size, df_size)})

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for selections",
                          fname = "bench_selection.pdf",
                          user_df_size_powers = @[2, 3, 4, 5, 6, 7], #, 8],
                          user_loop_size_powers = @[3, 3, 2, 2, 2, 2]) #, 1])
                          #user_df_size_powers = @[2, 3, 4, 5],
                          #user_loop_size_powers = @[3, 2, 1, 1])


benchmark.benchmark_all(df_filter) #, df_filter_alt)#, df_filter_alt)
benchmark.print_results()
benchmark.plot_results()
