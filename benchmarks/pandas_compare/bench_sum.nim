import ggplotnim, benchmarker

proc df_reduce_inline(df: var DataFrame) =
  discard reduce_inline(df["A"].toTensor(int)):
    x += y

proc df_summarize(df: var DataFrame) =
  discard df.summarize(f{int -> int: "sumA" << sum(`A`)})

proc df_sum_direct(df: var DataFrame) =
  discard df["A"].toTensor(int).sum

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size) })

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for summation",
                          fname = "bench_sum.pdf")
benchmark.benchmark_all(df_reduce_inline, df_summarize, df_sum_direct)
benchmark.print_results()
benchmark.plot_results()
