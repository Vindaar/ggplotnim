import ggplotnim, benchmarker

proc df_map_inline(df: var DataFrame) =
  df["E"] = map3_inline(df["A"].toTensor(int),
                        df["B"].toTensor(int),
                        df["C"].toTensor(int)):
    x * y + z


proc df_mutate(df: var DataFrame) =
  df = df.mutate(f{int -> int: "E" ~ `A` * `B` + `C`})

proc df_direct_formula_eval(df: var DataFrame) =
  df["E"] = (f{int -> int: "E" ~ `A` * `B` + `C`}).fnV(df)

proc genDf(df_size: int): DataFrame =
  result = seqsToDf({ "A" : randomTensor(df_size, df_size),
                      "B" : randomTensor(df_size, df_size),
                      "C" : randomTensor(df_size, df_size),
                      "D" : randomTensor(df_size, df_size) })

var benchmark = initBench(genDf = genDf,
                          title = "Benchmark for column creation",
                          fname = "bench_create_column.pdf")
benchmark.benchmark_all(df_map_inline, df_mutate, df_direct_formula_eval)
benchmark.print_results()
benchmark.plot_results()
