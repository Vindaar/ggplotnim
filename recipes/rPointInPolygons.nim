import ggplotnim, sequtils, chroma, options, sugar, random

type 
  Point = object
    x, y: float
  Vertex {.borrow: `.`.} = distinct Point
  Polygon = object
    vertices: seq[Vertex]

func len(p: Polygon): int = p.vertices.len
func `[]`(p: Polygon, idx: int): Vertex = p.vertices[idx]
    
proc initPolygon[T](vs: varargs[tuple[x, y: T]]): Polygon =
  result.vertices = newSeq[Vertex](vs.len)
  for i, v in vs:
    result.vertices[i] = Point(x: v[0].float, y: v[1].float).Vertex

proc flatten(p: Polygon): (seq[float], seq[float]) =
  result = (p.vertices.mapIt(it.x), p.vertices.mapIt(it.y))
  # add first point to get proper drawn polygon with geom line!
  result[0].add p.vertices[0].x
  result[1].add p.vertices[0].y

proc inPolygon(p: Point, poly: Polygon): bool = 
  # based on: https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html
  var j = poly.len - 1
  for i in 0 ..< poly.vertices.len:
    if ((poly[i].y <= p.y and p.y < poly[j].y) or
        (poly[j].y <= p.y and p.y < poly[i].y)) and
       (p.x < (poly[j].x - poly[i].x) * (p.y - poly[i].y) / (poly[j].y - poly[i].y) + poly[i].x):
      result = not result
    j = i

proc inAnyPolygon(p: Point, polys: seq[Polygon]): bool =
  for poly in polys:
    if p.inPolygon(poly): return true

let p1 = initPolygon((0, 1), (6, 0), (5, 2), (4, 1), (2, 4))
let p2 = initPolygon((5, 4), (8, 10), (10, 2), (7, 4))

let df1 = seqsToDf({ "x" : p1.flatten[0],
                    "y" : p1.flatten[1] })
let df2 = seqsToDf({ "x" : p2.flatten[0],
                    "y" : p2.flatten[1] })
let df = bind_rows(("Polygon 1", df1), ("Polygon 2", df2), "Num")

var rnd = initRand(42)
# now sample a bunch of points in (0, 10) plane and plot it
let points = collect(newSeq):
  for i in 0 ..< 300:
    Point(x: rnd.rand(10.0), y: rnd.rand(10.0))
let inPoly = points.mapIt(it.inAnyPolygon(@[p1, p2]))
  
let dfPoints = seqsToDf({ "x" : points.mapIt(it.x),
                          "y" : points.mapIt(it.y),
                          "InPoly" : inPoly })

# TODO: results in vertical line at start of polygon
ggplot(df, aes(x, y)) +
  geom_line(aes = aes(fill = "Num"), fillColor = "#ebba34") +
  geom_point(data = dfPoints, aes = aes(color = "InPoly")) +
  ggsave("./media/recipes/rPointInPolygons.png")
