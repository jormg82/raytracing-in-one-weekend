
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

import Control.Monad(forM, forM_, replicateM)
import Data.Maybe(catMaybes)
import System.IO(hPutStrLn, stderr)


-- Image
aspectRatio :: Double
--aspectRatio = 3/2
aspectRatio = 16/9

imageWidth :: Int
--imageWidth  = 1200
imageWidth = 1000

imageHeight :: Int
imageHeight = floor $ fromIntegral imageWidth / aspectRatio

samplesPerPixel :: Int
--samplesPerPixel = 500
samplesPerPixel = 500


maxDepth :: Int
--maxDepth = 50
maxDepth = 50


-- World
type World = [Hittable]


randomScene :: IO World
randomScene = do
  let groundMaterial = M.lambertian $ color 0.5 0.5 0.5
      material1      = M.dielectric 1.5
      material2      = M.lambertian $ color 0.4 0.2 0.1
      material3      = M.metal (color 0.7 0.6  0.5) 0
      w1             = [sphere (V.point3 0 (-1000) 0) 1000 groundMaterial,
                        sphere (V.point3 0 1 0) 1 material1,
                        sphere (V.point3 (-4) 1 0) 1 material2,
                        sphere (V.point3 4 1 0) 1 material3]
  w2 <- forM [(a, b) | a <- [(-11)..10], b <- [(-11)..10]] randomObject
  return $ w1 ++ catMaybes w2


randomObject :: (Int, Int) -> IO (Maybe Hittable)
randomObject (a, b) = do
  chooseMat <- RT.randomDouble
  centerx   <- RT.randomDouble
  centerz   <- RT.randomDouble
  let center = V.point3 (fromIntegral a+0.9*centerx)
                        0.2
                        (fromIntegral b+0.9*centerz)
      selector = V.len (center `V.diff` V.point3 4 0.2 0) > 0.9
  if selector then
    case (chooseMat < 0.8, chooseMat < 0.95) of

      -- Diffuse
      (True, _)      -> do albedo <- V.prod <$> V.random <*> V.random
                           let sphereMaterial = M.lambertian albedo
                           return $ Just $ sphere center 0.2 sphereMaterial

      -- Metal
      (False, True)  -> do albedo <- V.randomR 0.5 1
                           fuzz <- RT.randomDoubleR 0 0.5
                           let sphereMaterial = M.metal albedo fuzz
                           return $ Just $ sphere center 0.2 sphereMaterial
 
      -- Glass
      (False, False) -> do let sphereMaterial = M.dielectric 1.5
                           return $ Just $ sphere center 0.2 sphereMaterial

  else
    return Nothing


-- Camera
lookFrom, lookAt :: V.Point3
lookFrom = V.point3 13 2 3
lookAt   = V.point3 0 0 0

vup :: V.Vec3
vup = V.vec3 0 1 0

distToFocus :: Double
distToFocus = 10

aperture :: Double
aperture = 0.1

cam :: Cam.Camera
cam = Cam.camera lookFrom lookAt vup 20 aspectRatio aperture distToFocus



-- MAIN
main :: IO ()
main = do
  -- File header
  putStrLn "P3"
  putStrLn $ show imageWidth ++ " " ++ show imageHeight
  putStrLn "255"
  world <- randomScene
  forM_ [imageHeight-1, imageHeight-2 .. 0] (processRow world)


processRow :: World -> Int -> IO ()
processRow w j = do
  hPutStrLn stderr $ "Scanlines remaining: " ++ show j
  forM_ [0..imageWidth-1] (processPixel w j)


processPixel :: World -> Int -> Int -> IO ()
processPixel w j i = do
         -- al menos un sample!!
  col <- foldr1 V.add <$> replicateM samplesPerPixel (processSample w j i)
  putStrLn $ showColor samplesPerPixel col


processSample :: World -> Int -> Int -> IO Color
processSample w j i = do
  r1 <- RT.randomDouble
  r2 <- RT.randomDouble
  let u = (fromIntegral i + r1) / fromIntegral (imageWidth-1)
      v = (fromIntegral j + r2) / fromIntegral (imageHeight-1)
  r <- Cam.getRay cam u v
  rayColor r w maxDepth


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


