
module Material(
  Albedo,
  Material(..),
  lambertian,
  metal
) where

import Color(Color)

type Albedo = Color
type Fuzz = Double

data Material = Lambertian Albedo
              | Metal Albedo Fuzz
              deriving Show


lambertian :: Albedo -> Material
lambertian = Lambertian


metal :: Albedo -> Fuzz -> Material
metal a f | f < 0     = Metal a 0
          | f > 1     = Metal a 1
          | otherwise = Metal a f
