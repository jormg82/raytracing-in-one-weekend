
module Hit(
  HitRecord(..),
  hitRecord,
  Hit(..),
  faceNormal
) where

import Material(Material)
import qualified Ray
import qualified Vec3 as V

import Data.Maybe(catMaybes, listToMaybe)
import Data.List(sortOn)


data HitRecord = HitRecord
  {
    phit      :: V.Point3,
    normal    :: V.Vec3,
    material  :: Material,
    thit      :: Double,
    frontFace :: Bool
  }
  deriving Show


hitRecord :: V.Point3  -- Point
          -> V.Vec3    -- Normal
          -> Material  -- Material
          -> Double    -- Parameter t
          -> Bool      -- Front face?
          -> HitRecord
hitRecord = HitRecord


class Hit a where
  hit :: Ray.Ray
      -> Double  -- Min
      -> Double  -- Max
      -> a
      -> Maybe HitRecord


instance Hit a => Hit [a] where
  hit r m n = listToMaybe . sortOn thit . catMaybes . map (hit r m n)


faceNormal :: Ray.Ray -> V.Vec3 -> Bool
faceNormal r outwardNormal = Ray.direction r `V.dot` outwardNormal < 0

