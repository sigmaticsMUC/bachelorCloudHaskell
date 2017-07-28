module IOUtils.ColorMap(
  heightToRGB8
) where

import           Codec.Picture
--import           Data.Array.Repa

type RGB8 = (Float, Float, Float)

colorGradient = [ lime        -- 0    - 200
                , yellow      -- 200  - 500
                , orangebrown -- 500  - 1000
                , blackbrown  -- 1000 - 2000
                , redbrown    -- 2000 - 4000
                , brownred    -- 4000 - 5000
                , white ]     -- > 5000


heightToRGB8
  :: Float -- Max height
  -> (Float, Float, Float) -- ^ Height
  -> RGB8
--heightToRGB8 maxHeight (-1.0, _, _) = cyan
heightToRGB8 maxHeight (x, _, _) = calcRange x
  where
  {-
    calcRange s
      | s > 0 && s < 50      = intrplt a b (0, 50) s
      | s >= 50 && s < 200   = intrplt b c (50, 200) s
      | s >= 200 && s < 600  = intrplt c d (200 ,600) s
      | s >= 600 && s < 1000 = intrplt d e (600, 1000) s
      | s >= 1000 && s < 2000 = intrplt e f (1000, 2000) s
      | s >= 2000 && s < 3000 = intrplt f g (2000, 3000) s
      | s >= 3000 && s < 4000 = intrplt g h (3000, 4000) s
      | otherwise             = intrplt h f (4000, maxHeight) s
      -}
    calcRange s
      | s >= 1 && s < 1.05 = intrplt redbrown brownred (1, 1.05) s
      | s >= 1.05 && s < 1.25 = intrplt brownred cyan (1.05, 1.25) s
      | s >= 1.25 && s < 1.35 = intrplt cyan magenta (1.25, 1.35) s
      | s >= 1.35 && s < 1.45 = intrplt magenta blue (1.35, 1.45) s
      | s > 1.45 && s < 1.55  = intrplt lime yellow (1.45, 1.55) s
      | s > 1.55 && s < 1.65  = intrplt lime yellow (1.55, 1.65) s
      | s >= 1.65 && s < 1.7 = intrplt yellow orangebrown (1.65, 1.7) s
      | otherwise             = intrplt blue white (1.7, 2) s

intrplt
  :: RGB8
  -> RGB8
  -> (Float, Float)
  -> Float
  -> RGB8
intrplt c1 c2 minMax s = doubleToRGB (ir, ig, ib)
  where
    (r, g, b) = rgbToDouble c1
    (r', g', b') = rgbToDouble c2
    ir = linearMap minMax (r, r') s
    ig = linearMap minMax (g, g') s
    ib = linearMap minMax (b, b') s

mapTriple
  :: (a -> b)
  -> (a, a, a)
  -> (b, b, b)
mapTriple f (x, y, z) = (f x, f y, f z)

rgbToDouble
  :: RGB8
  -> (Float, Float, Float)
rgbToDouble = mapTriple f
  where f = id :: Float -> Float

doubleToRGB
  :: (Float, Float, Float)
  -> RGB8
doubleToRGB = mapTriple f . mapTriple round
  where
    f = fromIntegral :: Integer -> Float

linearMap
  :: Fractional a
  => (a, a)
  -> (a, a)
  -> a
  -> a
linearMap (a1, a2) (b1, b2) s = b1 + (s - a1) * (b2 - b1) / (a2 - a1)

-- gradient1
lime, yellow, orangebrown, blackbrown, redbrown, brownred, white, blue :: RGB8
lime = (163, 255, 0)
yellow = (255, 255, 0)
orangebrown = (150, 82, 32)
blackbrown = (85 , 58, 38)
redbrown = (150, 63, 32)
brownred = (120, 31, 25)
white = (255, 255, 255)
blue = (0, 0, 255)

-- einzugsgebiete
cyan, magenta, red :: RGB8
cyan = (0, 255, 255)
magenta = (255, 0, 255)
red = (255, 0, 0)

-- gradient2
a, b, c, d, e, f, g, h, i, j, k :: RGB8
a = (114, 164, 141) -- 0 50
b = (120, 172, 149) -- 50 200
c = (134, 184, 159) -- 200 600
d = (169, 192, 166) -- 600 1000
e = (212, 201, 180) -- 1000 2000
f = (212, 184, 163) -- 2000 3000
g = (212, 193, 179) -- 3000 4000
h = (212, 207, 204) -- 4000 5000
i = (220, 220, 220) -- 5000 6000
j = (235, 235, 237) -- 6000 7000
k = (245, 245, 245) -- > 7000
