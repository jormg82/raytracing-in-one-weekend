

module PPM (
  ImageWidth,
  ImageHeight,
  MaxColor,
  ppm,
  showPPM
) where

import Color

import Data.List(intercalate)


type ImageWidth  = Int
type ImageHeight = Int
type MaxColor    = Int


data PPM = PPM ImageWidth ImageHeight MaxColor [Color]


ppm :: ImageWidth -> ImageHeight -> MaxColor -> [Color] -> PPM
ppm = PPM


showPPM :: PPM -> String
showPPM (PPM w h m cs) = "P3\n"
                      ++ show w ++ " " ++ show h ++ "\n"
                      ++ show m ++ "\n"
                      ++ colors ++ "\n"
  where
    colors = intercalate "\n" $ map showColor cs

