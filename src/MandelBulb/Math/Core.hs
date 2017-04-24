module MandelBulb.Math.Core(
  doBulb
)where

type Vect = (Double, Double, Double)
type Iter = Integer
type Limit = Double


v_next :: Vect -> Vect
v_next (x, y, z) = (x_new, y_new, z_new)
  where x_new = ((3*z*z-x*x-y*y)*x*(x*x-3*y*y))/(x*x+y*y)
        y_new = ((3*z*z-x*x-y*y)*y*(3*x*x-y*y))/(x*x+y*y)
        z_new = z*(z*z-3*x*x-3*y*y)


addVec :: Vect -> Vect -> Vect
addVec (a, b, c) (d, e, f) = (a+d, b+e, c+f)

iterateBlb :: Vect -> Vect -> Vect
iterateBlb (0,0,0) c = c
iterateBlb v c = addVec (v_next v) c


bulb :: Iter -> Iter -> Limit -> Vect -> Vect -> Iter
bulb i m l c v
  | (i < m) && ((norm v) < l) = bulb (i+1) m l c v_new
  | otherwise = i
    where v_new = iterateBlb v c

doBulb :: Iter -> Iter -> Limit -> Vect -> Iter
doBulb i m l c = bulb i m l c (0, 0, 0)

norm :: Vect -> Double
norm (x, y, z) = sqrt $ (x*x) + (y*y) + (z*z)
