

module Ray(
  Ray(..),
  ray,
  at
) where


import qualified Vec3 as V


data Ray = Ray {origin :: V.Point3, direction :: V.Vec3}
           deriving Show

ray :: V.Point3 -> V.Vec3 -> Ray
ray = Ray

at :: Ray -> Double -> V.Point3
at r = V.add (origin r) . V.mul (direction r)
