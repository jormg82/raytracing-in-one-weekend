
module Main where

import qualified Color as C
import qualified PPM as P

import Control.Monad(forM)


width :: P.ImageWidth
width  = 256

height :: P.ImageHeight
height = 256

maxColor :: P.MaxColor
maxColor = 255


main :: IO ()
main = do
  -- forM en lugar de traverse para que los bucles sean mas explicitos
  cdata <- do forM [height-1, height-2 .. 0]
                (\j -> do putStrLn $ "Scanlines remaining: " ++ show j
                          forM [0..width-1]
                            (\i -> do let w1 = fromIntegral $ width-1
                                          h1 = fromIntegral $ height-1
                                          r = fromIntegral i / w1
                                          g = fromIntegral j / h1
                                          b = 0.25
                                      return $ C.color r g b
                            )
                )
  let file = P.ppm width height maxColor $ concat cdata
  writeFile "image.ppm" $ P.showPPM file

