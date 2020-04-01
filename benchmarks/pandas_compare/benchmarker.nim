import ggplotnim, times, macros, sequtils, math, strutils
import criterion

type
  EvalFunc = proc(df: var DataFrame)
  Benchmarker* = object
    genDf: proc(df_size: int): DataFrame
    title: string
    fname: string
    df_size_powers: seq[int]
    loop_size_powers: seq[int]
    valid: bool
    results: DataFrame

proc addPandasResults*(bench: var Benchmarker) =
  let sizes = bench.df_size_powers
  let data = readFile("pandas_results/" & bench.fname.replace("bench", "benchmark")
    .replace(".pdf", ".txt")).strip.splitLines
  let names = data[0].split(",")
  let pdResults = data[1 .. ^1]
    .mapIt(it.strip(chars = Whitespace + {'[', ']'})
             .split(',').mapIt(it.strip.parseFloat))
  doAssert pdResults.len == names.len
  for tup in zip(names, pdResults):
    var df = newDataFrame()
    df["perLoop"] = toColumn tup[1]
    df["dfSize"] = toColumn sizes
    df["evalFunc"] = constantColumn(tup[0], sizes.len)
    bench.results.add df


proc initBench*(genDf: proc(df_size: int): DataFrame,
                fname: string,
                title: string,
                user_df_size_powers: seq[int] = @[],
                user_loop_size_powers: seq[int] = @[]): Benchmarker =
  let dfsize = if user_df_size_powers.len == 0: @[2, 3, 4, 5, 6, 7]
               else: user_df_size_powers
  let loopSize = if user_loop_size_powers.len == 0: @[4, 4, 3, 3, 2, 1]
                 else: user_loop_size_powers
  result = Benchmarker(genDf: genDf,
                       title: title,
                       fname: fname,
                       df_size_powers: dfsize,
                       loop_size_powers: loopsize)
  result.results = newDataFrame(3)
  # TODO: validate functions

#proc validate_functions(bench: var Benchmarker): bool =
#  functions_results = []
#  df_size = 10 ** self.df_size_powers[0]
#  df = eval(self.df_generator)
#  for function_to_evaluate in self.functions_to_evaluate:
#      functions_results.append(function_to_evaluate(df))
#
#  valid = True
#  for i in range(len(functions_results)):
#      for j in range(i + 1, len(functions_results)):
#          if isinstance(functions_results[i], pd.DataFrame):
#              if not functions_results[i].equals(functions_results[j]): valid = False
#          elif isinstance(functions_results[i], np.ndarray):
#              if not np.array_equal(functions_results[i], functions_results[j]): valid = False
#          else:
#              try:
#                  if not (functions_results[i] == functions_results[j]): valid = False
#              except Exception as e:
#                  valid = False
#
#  return valid

proc benchmark_time*(bench: Benchmarker, evalFunc: EvalFunc, name: string): DataFrame =
  var perLoop = newSeq[float]()
  var dfSizes = newSeq[int]()
  for (df_size_power, loop_size_power) in zip(bench.df_size_powers, bench.loop_size_powers):
    let df_size = 10 ^ df_size_power
    echo "\tTesting with a dataframe of size: ", df_size
    var df = bench.genDf(df_size)
    let loop_size = 10 ^ loop_size_power
    let start_time = epochTime()

    for loop_counter in 0 ..< loop_size:
      evalFunc(df)

    let end_time = epochTime()
    let per_loop_time = (end_time - start_time) / loop_size.float
    echo "\tResult (seconds): ", per_loop_time
    perLoop.add per_loop_time
    dfSizes.add df_size_power
  result = newDataFrame(3)
  result["perLoop"] = toColumn perLoop
  result["dfSize"] = toColumn dfSizes
  result["evalFunc"] = constantColumn(name, perLoop.len)

macro benchmark_all*(bench: var Benchmarker, fns: varargs[untyped]): untyped =
  result = newStmtList()
  for fn in fns:
    result.add quote do:
      echo "Benchmarking sizes: ", `bench`.df_size_powers
      echo "Benchmarking function: ", astToStr(`fn`)
      `bench`.results.add `bench`.benchmark_time(`fn`, astToStr(`fn`))
      echo `bench`.results
  echo result.repr

proc plot_results*(bench: var Benchmarker) =
  # run locally on my machine:
  bench.addPandasResults()
  echo "Plotting now!----------------------------------------------\n"
  ggplot(bench.results, aes("dfSize", "perLoop", color = "evalFunc")) +
    geom_line() +
    geom_point() +
    #geom_line() +
    #scale_x_continuous() +
    xlab("Power of DF size") +
    scale_y_log10() +
    ggtitle(bench.title) +
    theme_opaque() +
    ggsave(bench.fname, width = 800, height = 450)
  #for result, function_name in zip(self.benchmark_results, self.functions_to_evaluate):
  #  plt.semilogy(list(range(len(result))), result, marker="o", label=function_name.__name__)
  #
  #plt.title(self.title, fontsize=15)
  #plt.ylabel("Seconds", fontsize=13)
  #plt.xticks(range(len(self.df_size_powers)), ["$10^{}$".format(x) for x in self.df_size_powers])
  #plt.legend(frameon=True)
  #
  #plt.sca(ax[1])
  #scaled_results = []
  #for result in self.benchmark_results:
  #    scaled_results.append(np.divide(np.array(result), np.array(self.benchmark_results[0])))
  #
  #max_diff = np.max(scaled_results)
  #if max_diff < 3:
  #    plt.ylim(ymax=3)
  #
  #for result, function_name in zip(scaled_results, self.functions_to_evaluate):
  #    plt.plot(list(range(len(result))), result, marker="o", label=function_name.__name__)
  #
  #plt.title(self.title, fontsize=15)
  #plt.ylabel("w.r.t. to '{}' time".format(self.functions_to_evaluate[0].__name__), fontsize=13)
  #plt.xlabel("Dataframe size", fontsize=13)
  #plt.xticks(range(len(self.df_size_powers)), ["$10^{}$".format(x) for x in self.df_size_powers])
  #plt.legend(frameon=True)
  #plt.savefig("exports/{}.png".format(self.title), bbox_inches="tight")
  #plt.show()

proc print_results*(bench: Benchmarker) =
  echo bench.results.pretty(-1)
