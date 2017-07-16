module MandelBulb.Utils.Domain(
  generateDomain,
  generatePlane,
  rowMajor
)where

type Point = (Float, Float, Float)
type Step = Float
type Domain = [Plane]
type Plane = [Row]
type Row = [Point]

generateDomain :: Point -> Point -> Step -> [[[Point]]]
generateDomain a@(sx, sy, sz) b@(ex, ey, ez)  h
  | sz >= ez = (generatePlane a b h) : []
  | otherwise = (generatePlane a b h) : (generateDomain (sx, sy, sz + h) b h)


generatePlane :: Point -> Point -> Step -> [[Point]]
generatePlane a@(sx, sy, sz) b@(ex, ey, ez)  h
  | sy >= ey = (generateRow a b h) : []
  | otherwise = (generateRow a b h) : (generatePlane (sx, sy + h, sz) b h)


generateRow :: Point -> Point -> Step -> [Point]
generateRow a@(sx, sy, sz) b@(ex, ey, ez)  h
  | sx >= ex = a : []
  | otherwise = a : (generateRow (sx + h, sy, sz) b h)


rowMajor :: [[[a]]] -> [a]
rowMajor = concat . concat
