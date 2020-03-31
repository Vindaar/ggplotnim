import numpy as np
import matplotlib.pyplot as plt

def main():
  xt = np.random.rand(1_000_000)
  yt = np.random.rand(1_000_000)

  plt.plot(xt, yt, markersize = 0.1, marker = ".", fillstyle = 'full',
           color = "black", linestyle="")
  plt.savefig("million_matploltib_plot.pdf")

main()

# save as pdf:
# - 22s
# save as png:
# - 1.1 s
