

module Color(
  Color,
  color,
  showColor
) where

import RTWeekend
import Vec3


type Color = Vec3

color :: Double -> Double -> Double -> Color
color = Vec3


showColor :: Int   -- Samples per pixel
          -> Color -- Color
          -> String
showColor samples col = show r ++ " " ++ show g ++ " " ++ show b
  where
    scale = 1.0 / fromIntegral samples
    col'  = col `mul` scale     
    r     = floor (256 * clamp (sqrt $ x col') 0 0.999) :: Int
    g     = floor (256 * clamp (sqrt $ y col') 0 0.999) :: Int
    b     = floor (256 * clamp (sqrt $ z col') 0 0.999) :: Int
