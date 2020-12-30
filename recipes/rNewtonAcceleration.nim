import ggplotnim
import seqmath # for linspace, pow
import sequtils # for mapIt

func newtonAcceleration(r: float): float =
  ## returns the graviational acceleration experienced by a test mass
  ## at different distances from Earth (or inside Earth).
  ## `r` is the radial distance given in `m`
  const R = 6371 * 1000 # mean radius of Earth in m
  const m_E = 5.972e24 # kg
  const G = 6.674e-11 # m^3 kg^-1 s^-2
  if r < R:
    result = G * m_E * r / pow(R, 3.0)
  else:
    result = G * m_E / (r * r)

let radii = linspace(0.0, 35_000_000, 1000) # up to geostationary orbit
# and the corresponding accelerations
let a = radii.mapIt(newtonAcceleration(it))

var df = seqsToDf({ "r / m" : radii,
                    "g(r) / m s¯²" : a})

df = df.transmute(f{"r / km" ~ c"r / m" / 1000.0}, f{"g(r) / m s¯²"})

ggplot(df, aes("r / km", "g(r) / m s¯²")) +
  geom_line() +
  ggtitle("Gravitational acceleration of Earth depending on radial distance") +
  theme_opaque() +
  ggsave("media/recipes/rNewtonAcceleration.png")

let maxG = df.summarize(f{float: "g_max" << max(c"g(r) / m s¯²")})

let maxG_alt = df["g(r) / m s¯²"].toTensor(float).max

echo "Max acceleration:\n ", maxG

echo "At surface = ", newtonAcceleration(6371000)
