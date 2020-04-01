import ggplotnim, benchmarker, arraymancer

proc df_summ_product(df: var DataFrame) =
  discard df.summarize(f{int -> int: "prod" << product(`A`)})

proc df_prod_tensor(df: var DataFrame) =
  discard df["A"].toTensor(int).product

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size) })

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for product",
                          fname = "bench_prod.pdf")

benchmark.benchmark_all(df_summ_product, df_prod_tensor)
benchmark.print_results()
benchmark.plot_results()
