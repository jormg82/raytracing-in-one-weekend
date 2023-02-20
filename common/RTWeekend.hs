
module RTWeekend(
  inf,
  degreesToRadians,
  clamp,
  randomDouble,
  randomDoubleR
) where

import System.Random


inf :: Double
inf = 1/0

degreesToRadians :: Double -> Double
degreesToRadians d = d*pi/180.0


clamp :: Double -- Value
      -> Double -- Min
      -> Double -- Max
      -> Double
clamp val m n | val < m   = m
              | val > n   = n
              | otherwise = val


-- Devuelve un valor en [0, 1)
randomDouble :: IO Double
randomDouble = do
  a <- randomRIO (0, maxBound) :: IO Int
  return $ fromIntegral a / (fromIntegral (maxBound :: Int) + 1.0)


-- Devuelve un valor en [Min, Max)
randomDoubleR :: Double -- Min
              -> Double -- Max
              -> IO Double
randomDoubleR m n = do
  a <- randomDouble
  return $ m + (n-m)*a

