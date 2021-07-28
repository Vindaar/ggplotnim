import ggplotnim, math, sequtils, latexdsl, strutils, ginger
proc landauApprox(x: float): float =
  result = 1.0 / sqrt(2 * PI) * exp(- (x + exp(-x)) / 2 )

proc annotateText(): string =
  # pure math in raw string due to too many invalid nim constructs for `latex` 
  # macro (would result in ugly mix of strings and identifiers)
  # Need manual math mode via `$`!
  let eq = r"$p(x) = \frac{1}{2πi}\int_{a-i∞}^{a+i∞} e^{s \log(s) + xs}\, ds$" 
  let eqApprox = r"$p(x) \approx \frac{1}{\sqrt{2π}} \exp\left(- \frac{x + e^{-x}}{2} \right)$"
  # align text with math using 2 line breaks for better separation (single line break is too
  # squished. Cannot use `equation` or similar in a TikZ node afaiu :/)
  result = r"The Landau distribution\\ \\ " &
    eq & r"\\ \\" &
    r"reduces to the approximation:\\ \\ " &
    eqApprox & r"\\ \\ " & 
    r"for $μ = 0, c = 1$"
     
let x = linspace(-5.0, 15.0, 1000)
let y = x.mapIt(landauApprox(it))
let df = seqsToDf(x, y)
ggplot(df, aes("x", "y")) +
  geom_line() +                             # draw our Landau data as a line
  annotate(annotateText(),                  # add our text annotation
           x = 5.0, y = 0.1,                # at this location in 'data space'
           backgroundColor = transparent) + # transparent background as we do manual TeX line breaks
  ggtitle(r"Approximation of Landau distribution: " & 
    r"$p(x) \approx \frac{1}{\sqrt{2π}} \exp\left(- \frac{x + e^{-x}}{2} \right)$",
    titleFont = font(12.0)) +
  ggsave("media/recipes/rTikZLandau.tex", standalone = true) # standalone to get TeX file w/ only plot
  # use:
  # ggsave("media/recipes/rTikZLandau.pdf", useTex = true, standalone = true)
  # to directly compile to standalone PDF (using local xelatex)
