module MandelBulb.Math.Core(
  doBulb
)where

type Vect = (Double, Double, Double)
type Iter = Integer
type Limit = Double


r :: Vect -> Double
r v = norm v

theta :: Vect -> Double
theta (x, y, z) = atan(y / x)

phi :: Vect -> Double
phi vec@(_, _, z) = acos(z / (norm vec))

v_n :: Double -> Vect -> Vect
v_n n v = (x_new, y_new, z_new)
  where r_n = (r v) ** n
        x_new = r_n * sin(n*(theta v)) * cos(n*(phi v))
        y_new = r_n * sin(n*(theta v)) * sin(n*(phi v))
        z_new = r_n * cos(n*(theta v))


v_next :: Double -> Vect -> Vect
v_next n v = v_n n v

addVec :: Vect -> Vect -> Vect
addVec (a, b, c) (d, e, f) = (a+d, b+e, c+f)


iterateBlb :: Double -> Vect -> Vect -> Vect
iterateBlb _ (0,0,0) c = c
iterateBlb n v c = addVec (v_next n v) c


bulb :: Double -> Iter -> Iter -> Limit -> Vect -> Vect -> Iter
bulb n i m l c v
  | (i < m) && ((norm v) < l) = bulb n (i+1) m l c v_new
  | otherwise = i
    where v_new = iterateBlb n v c

doBulb :: Double -> Iter -> Iter -> Limit -> Vect -> Iter
doBulb n i m l c = bulb n i m l c (0, 0, 0)


norm :: Vect -> Double
norm (x, y, z) = sqrt $ (x*x) + (y*y) + (z*z)
