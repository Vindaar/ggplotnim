import sequtils, seqmath, ggplotnim

proc logspace(start, stop: float, num: int, base = 10.0): seq[float] =
  ## generates evenly spaced points between start and stop in log space
  let linear = linspace(start, stop, num)
  result = linear.mapIt(pow(base, it))

proc density(p: float, temp = 4.2): float = 
  ## returns the density of the gas for the given pressure.
  ## The pressure is assumed in `mbar` and the temperature (in `K`).
  ## The default temperature corresponds to BabyIAXO aim.
  ## Returns the density in `g / cm^3`
  const gasConstant = 8.314 # joule K^-1 mol^-1
  const M = 4.002602 # g / mol
  let pressure = p * 1e2 # pressure in Pa
  # factor 1000 for conversion of M in g / mol to kg / mol
  result = pressure * M / (gasConstant * temp * 1000.0)
  # convert the result to g / cm^3 for use with mass attenuations
  result = result / 1000.0

proc numDensity(c: float): float =
  ## converts a molar concentration in mol / m^3 to a number density
  const Z = 2 # number of electron in helium atom
  result = Z * 6.022e23 * c

proc effPhotonMass(ne: float): float =
  ## returns the effective photon mass for a given electron number density
  const alpha = 1.0 / 137.0
  const me = 511e3 # 511 keV
  # note the 1.97e-7 cubed to account for the length scale in `ne`
  result = sqrt( pow(1.97e-7, 3) * 4 * PI * alpha * ne / me )

proc pressureGivenEffPhotonMass(m_gamma: float, T = 4.2): float =
  ## calculates the inverse of `babyIaxoEffPhotonMass`, i.e. the pressure 
  ## from a given effective photon mass in BabyIAXO
  result = m_gamma * m_gamma * T / 0.01988

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
  const temp = 4.2
  let amountMol = molarAmount(p, vol, temp) # amount of gas in mol
  let numPerMol = numDensity(amountMol / vol) # number of electrons per m^3
  result = effPhotonMass(numPerMol) 

proc vacuumMassLimit(energy: float, magnetLength = 10.0): float =
  ## given an axion energy in keV, calculate the limit of coherence
  ## for the vacuum case in BabyIAXO
  let babyIaxoLen = magnetLength / 1.97e-7 # length in "eV"
  result = sqrt(PI * 2 * energy * 1e3 / babyIaxoLen) # convert energy to eV

const babyIaxoVacuumMassLimit = vacuumMassLimit(4.2)
proc m_a_vs_density(pstart, pstop: float) =
  let pressures = logspace(pstart.log10, pstop.log10, 1000)
  let densities = pressures.mapIt(density(it, 4.2))
  let masses = pressures.mapIt(babyIaxoEffMass(it)) # corresponding masses
  # convert both seqs to a dataframe
  let ref1Bar = density(1000, 293.15)
  let df1bar = toDf({"ρ / g/cm^3" : @[ref1Bar, ref1Bar], "m_a / eV" : @[1e-2, 1.0]})
  let ref3Bar = density(3000, 293.15)
  let df3bar = toDf({"ρ / g/cm^3" : @[ref3Bar, ref3Bar], "m_a / eV" : @[1e-2, 1.0]})
  let refVacLimit = density(pressureGivenEffPhotonMass(babyIaxoVacuumMassLimit))
  let dfVacLimit = toDf({"ρ / g/cm^3" : @[refVacLimit, refVacLimit], "m_a / eV" : @[1e-2, 1.0]})
  let df = toDf({"ρ / g/cm^3" : densities, "m_a / eV" : masses})
  let dfComb = bind_rows([("ma vs ρ", df),
                          ("1 bar @ 293 K", df1bar),
                          ("3 bar @ 293 K", df3bar),
                          ("Vacuum limit", dfVacLimit)],
                         id = "Legend")
  ggplot(dfComb, aes("ρ / g/cm^3", "m_a / eV", color = "Legend")) +
    geom_line() + 
    scale_x_log10() + 
    scale_y_log10() +
    ggtitle("Sensitive axion mass in eV depending on helium density in g / cm^3") +
    ggsave("media/recipes/rAxionMassVsDensity.png")

m_a_vs_density(pressureGivenEffPhotonMass(babyIaxoVacuumMassLimit) * 0.9,
               pressureGivenEffPhotonMass(0.4521) * 1.1)
