import numpy as np
import matplotlib.pyplot as plt

def main():
  xt = np.random.rand(1_000_000)
  yt = np.random.rand(1_000_000)

  plt.scatter(xt, yt, s = 0.1)
  plt.savefig("million_matplotlib_scatter.png")

main()

# save as pdf
# - 24 s
# save as png
# - 2.8 s
