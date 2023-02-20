
module Main where

import qualified Camera as Cam
import Color
import qualified Hit
import Hittable(Hittable, sphere)
import qualified Material as M
import qualified Ray
import qualified RTWeekend as RT
import Scatter(scatter)
import qualified Vec3 as V

import Control.Monad(forM_, replicateM)
import System.IO(hPutStrLn, stderr)


-- Image
aspectRatio :: Double
aspectRatio = 16.0 / 9.0

imageWidth :: Int
--OJO!! reponer
--imageWidth  = 400
imageWidth = 400

imageHeight :: Int
imageHeight = floor $ fromIntegral imageWidth / aspectRatio

samplesPerPixel :: Int
-- OJO!! reponer
--samplesPerPixel = 100
samplesPerPixel = 100


maxDepth :: Int
--OJO!! reponer
--maxDepth = 50
maxDepth = 50


-- World
materialGround, materialCenter, materialLeft, materialRight :: M.Material
materialGround = M.lambertian $ color 0.8 0.8 0.0
materialCenter = M.lambertian $ color 0.7 0.3 0.3
materialLeft   = M.metal (color 0.8 0.8 0.8) 0.3
materialRight  = M.metal (color 0.8 0.6 0.2) 1


world :: [Hittable]
world = [sphere (V.point3 0 (-100.5) (-1)) 100 materialGround,
         sphere (V.point3 0 0 (-1)) 0.5 materialCenter,
         sphere (V.point3 (-1) 0 (-1.0)) 0.5 materialLeft,
         sphere (V.point3 1 0 (-1)) 0.5 materialRight]


-- Camera
viewportHeight, viewportWidth, focalLength :: Double
viewportHeight = 2.0
viewportWidth  = aspectRatio * viewportHeight
focalLength    = 1.0

orig :: V.Point3
orig = V.vec3 0 0 0

cam :: Cam.Camera
cam = Cam.camera aspectRatio viewportHeight focalLength orig



main :: IO ()
main = do
  -- File header
  putStrLn "P3"
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  forM_ [imageHeight-1, imageHeight-2 .. 0] processRow


processRow :: Int -> IO ()
processRow j = do
  hPutStrLn stderr $ "Scanlines remaining: " ++ show j
  forM_ [0..imageWidth-1] (processPixel j)


processPixel :: Int -> Int -> IO ()
processPixel j i = do
         -- al menos un sample!!
  col <- foldr1 V.add <$> replicateM samplesPerPixel (processSample j i)
  putStrLn $ showColor samplesPerPixel col


processSample :: Int -> Int -> IO Color
processSample j i = do
  r1 <- RT.randomDouble
  r2 <- RT.randomDouble
  let u = (fromIntegral i + r1) / fromIntegral (imageWidth-1)
      v = (fromIntegral j + r2) / fromIntegral (imageHeight-1)
      r = Cam.getRay cam u v
  rayColor r world maxDepth


rayColor :: Hit.Hit a => Ray.Ray
                      -> a
                      -> Int     -- Depth
                      -> IO Color
rayColor r a d
  | d <= 0    = return $ color 0 0 0
  | otherwise =
      case Hit.hit r 0.001 RT.inf a of
        Nothing -> do
          let unitDirection = V.unitVec (Ray.direction r)
              t             = 0.5 * (V.y unitDirection + 1.0)
          return $ V.mul (color 1 1 1) (1-t) `V.add` V.mul (color 0.5 0.7 1) t

        Just rec -> do
          scat <- scatter r rec
          case scat of
            Nothing                       -> return $ color 0 0 0
            Just (attenuation, scattered) -> (attenuation `V.prod`) <$>
                                             rayColor scattered a (d-1)

