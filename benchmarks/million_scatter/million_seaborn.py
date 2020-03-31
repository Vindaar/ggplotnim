import numpy as np
import seaborn

def main():
  xt = np.random.rand(1_000_000)
  yt = np.random.rand(1_000_000)

  ax = seaborn.scatterplot(xt, yt, size = 0.1)
  ax.get_figure().savefig("million_matplotlib_seaborn.png")

main()

# save as pdf:
# - 136 s
# save as png:
# - 21 s
