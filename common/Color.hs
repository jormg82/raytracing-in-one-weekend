

module Color where


data Color = Color {r :: Double, g :: Double, b :: Double}
             deriving Show

quasi256 :: Double
quasi256 = 255.999

color :: Double -> Double -> Double -> Color
color = Color

showColor :: Color -> String
showColor (Color r g b) = show r' ++ " " ++ show g' ++ " " ++ show b'
  where
    r' = floor (r*quasi256)
    g' = floor (g*quasi256)
    b' = floor (b*quasi256)
