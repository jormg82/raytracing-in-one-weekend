

module Hittable(
  Hittable(..),
  sphere
) where

import qualified Hit
import qualified Material as M
import qualified Ray as R 
import qualified Vec3 as V


data Hittable = Sphere V.Point3 Double M.Material -- Center, radius, material
                deriving Show


sphere :: V.Point3 -> Double -> M.Material -> Hittable
sphere = Sphere


instance Hit.Hit Hittable where
  hit r m n (Sphere center radius mat)
    | discr < 0                = Nothing
    | m <= root && root <= n   = Just $ makeHittable root
    | m <= root' && root' <= n = Just $ makeHittable root'
    | otherwise                = Nothing
    where
      oc     = R.origin r `V.diff` center
      a      = V.lenSquared $ R.direction r
      halfB  = oc `V.dot` R.direction r
      c      = V.lenSquared oc - radius*radius
      discr  = halfB*halfB - a*c
      sqrtd  = sqrt discr
      root   = ((-halfB) - sqrtd) / a
      root'  = ((-halfB) + sqrtd) / a
      makeHittable :: Double -> Hit.HitRecord
      makeHittable hroot =
        let point = R.at r hroot
            outwardNorm = V.divi (point `V.diff` center) radius
            fface = Hit.faceNormal r outwardNorm
            outwardNorm' = if fface then outwardNorm else V.neg outwardNorm
        in Hit.hitRecord point outwardNorm' mat hroot fface

