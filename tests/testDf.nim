import ggplotnim

var mpg = toDf(readCsv("../data/mpg.csv"))

let mpggroup = mpg.group_by("cyl")

echo mpg.summarize(f{"mean_cyl" ~ mean("cyl")},
                   f{"mean_hwy" ~ mean("hwy")})
echo "----"
echo mpggroup.summarize(f{"mean_displ" ~ mean("displ")},
                        f{"mean_hwy" ~ mean("hwy")})
