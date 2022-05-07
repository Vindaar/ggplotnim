import sequtils, seqmath, ggplotnim

proc logMassAttenuation(e: float): float =
  ## calculates the logarithm of the mass attenuation coefficient for a given
  ## energy `e` in `keV` and the result in `cm^2/g`
  result = -1.5832 + 5.9195 * exp(-0.353808 * e) + 4.03598 * exp(-0.970557 * e)

let energies = linspace(0.0, 10.0 , 1000) # from 0 to 10 keV
let logMuOverRho = energies.mapIt(logMassAttenuation(it))
# now the non-log values
let muOverRho = logMuOverRho.mapIt(exp(it))

const massAttenuationFile = "data/mass_attenuation_nist_data.txt"
# skip one line after header, second header line
var dfMuRhoTab = readCsv(massAttenuationFile, header = "#", 
                         sep = ' ')
  # convert MeV energy to keV
  .mutate(f{"Energy" ~ c"Energy" * 1000.0})
  .filter(f{float: c"Energy" >= energies.min and c"Energy" <= energies.max})
# create df of interpolated values
let dfMuRhoInterp = toDf({ "E / keV" : energies,
                               "mu/rho" : muOverRho })
# rename the columns of the tabulated values df and create a log column
dfMuRhoTab = dfMuRhoTab.rename(f{"E / keV" <- "Energy"})
# build combined DF
let dfMuRho = bind_rows([("Interpolation", dfMuRhoInterp), 
                         ("NIST", dfMuRhoTab)], 
                        id = "type")

# and the plot of the raw mu/rho values
ggplot(dfMuRho, aes("E / keV", "mu/rho", color = "type")) + 
  geom_line(data = dfMuRho.filter(f{`type` == "Interpolation"})) +
  geom_point(data = dfMuRho.filter(f{`type` == "NIST"})) +
  scale_y_log10() +
  ggtitle("Mass attenuation coefficient interpolation and data") +
  ggsave("media/recipes/rMassAttenuationFunction.png")
