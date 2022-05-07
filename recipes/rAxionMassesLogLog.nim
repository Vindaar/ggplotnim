import sequtils, seqmath, ggplotnim

proc effPhotonMass(ne: float): float =
  ## returns the effective photon mass for a given electron number density
  const alpha = 1.0 / 137.0
  const me = 511e3 # 511 keV
  # note the 1.97e-7 cubed to account for the length scale in `ne`
  result = sqrt( pow(1.97e-7, 3) * 4 * PI * alpha * ne / me )

proc numDensity(c: float): float =
  ## converts a molar concentration in mol / m^3 to a number density
  const Z = 2 # number of electron in helium atom
  result = Z * 6.022e23 * c

proc molarAmount(p, vol, temp: float): float =
  ## calculates the molar amount of gas given a certain pressure,
  ## volume and temperature
  ## the pressure is assumed in mbar
  const gasConstant = 8.314 # joule K^-1 mol^-1
  let pressure = p * 1e2 # pressure in Pa
  result = pressure * vol / (gasConstant * temp)

proc babyIaxoEffMass(p: float): float =
  ## calculates the effective photon (and thus axion mass) for BabyIAXO given
  ## a certain helium pressure in the BabyIAXO magnet
  const vol = 10.0 * (PI * pow(0.3, 2)) # 10m length, bore radius 30 cm
  # UPDATE: IAXO will be run at 4.2 K instead of 1.7 K
  # const temp = 1.7 # assume 1.7 K same as CAST
  const temp = 4.2
  once:
    echo "BabyIAXO magnet volume is ", vol, " m^3"
    echo "BabyIAXO magnet temperature is ", temp, " K"
  let amountMol = molarAmount(p, vol, temp) # amount of gas in mol
  let numPerMol = numDensity(amountMol / vol) # number of electrons per m^3
  result = effPhotonMass(numPerMol)

proc logspace(start, stop: float, num: int, base = 10.0): seq[float] =
  ## generates evenly spaced points between start and stop in log space
  let linear = linspace(start, stop, num)
  result = linear.mapIt(pow(base, it))

proc makePlot(pstart, pstop: float, fname: string) =
  let pressures = logspace(pstart, pstop, 1000) # 1000 values from 1e-5 mbar to 500 mbar
  let masses = pressures.mapIt(babyIaxoEffMass(it)) # corresponding masses
  # convert both seqs to a dataframe
  let df = toDf({"P / mbar" : pressures, "m_a / eV" : masses})
  ggplot(df, aes("P / mbar", "m_a / eV")) +
    geom_line() +
    scale_x_log10() +
    scale_y_log10() +
    ggtitle("Sensitive axion mass in eV depending on helium pressure in mbar") +
    ggsave(fname)

makePlot(-6.0, 2.0, "media/recipes/rAxionMassesLogLog.png")
