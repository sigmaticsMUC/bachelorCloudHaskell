module MandelBulb.Math.Core(
  doBulb,
  norm
)where

type Vect = (Float, Float, Float)
type Iter = Integer
type Limit = Float


r :: Vect -> Float
r v = norm v

theta :: Vect -> Float
theta vec@(x, y, z) = acos(z / (norm vec))

phi :: Vect -> Float
phi (x, y, z) = atan(y / x)

v_n :: Float -> Vect -> Vect
v_n n v = (x_new, y_new, z_new)
  where r_n = (r v) ** n
        x_new = r_n * sin(n*(theta v)) * cos(n*(phi v))
        y_new = r_n * sin(n*(theta v)) * sin(n*(phi v))
        z_new = r_n * cos(n*(theta v))

{-
v_n :: Float -> Vect -> Vect
v_n n v@(x, y, z) = (x_new, y_new, z_new)
  where x_new = (3*z*z-x*x-y*y)*x*(x*x-3*y*y)*(1.0/(x*x+y*y))
        y_new = (3*z*z-x*x-y*y)*y*(3*x*x-y*y)*(1.0/(x*x+y*y))
        z_new = z*(z*z-3*x*x-3*y*y)
-}
v_next :: Float -> Vect -> Vect
v_next n v = v_n n v

addVec :: Vect -> Vect -> Vect
addVec (a, b, c) (d, e, f) = (a+d, b+e, c+f)


iterateBlb :: Float -> Vect -> Vect -> Vect
iterateBlb _ (0,0,0) c = c
iterateBlb n v c = addVec (v_next n v) c


bulb :: Float -> Iter -> Iter -> Limit -> Vect -> Vect -> Iter
bulb n i m l c v
  | (i < m) && ((norm v) < l) = bulb n (i+1) m l c v_new
  | otherwise = i
    where v_new = iterateBlb n v c

doBulb :: Float -> Iter -> Iter -> Limit -> Vect -> Iter
doBulb n i m l c = bulb n i m l c (0, 0, 0)


norm :: Vect -> Float
norm (x, y, z) = sqrt $ (x*x) + (y*y) + (z*z)
