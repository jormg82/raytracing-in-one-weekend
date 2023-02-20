

module Camera (
  Camera(),
  camera,
  getRay 
) where

import qualified Ray
import qualified Vec3 as V

data Camera = Camera
  {
    origin          :: V.Point3,
    lowerLeftCorner :: V.Point3,
    horizontal      :: V.Vec3,
    vertical        :: V.Vec3
  }
  deriving Show



camera :: Double -- Aspect ratio
       -> Double -- Viewport height
       -> Double -- Focal length
       -> V.Point3 -- Origin
       -> Camera
camera ar vh fl ori =
  Camera{origin=ori, lowerLeftCorner=corner, horizontal=hor, vertical=ver}
  where
    vw     = ar * vh
    hor    = V.vec3 vw 0 0
    ver    = V.vec3 0 vh 0
    corner = ori `V.diff` V.divi hor 2 `V.diff`
             V.divi ver 2 `V.diff` V.vec3 0 0 fl


getRay :: Camera -- Camera
       -> Double -- Horizontal u
       -> Double -- Vertical v
       -> Ray.Ray
getRay cam u v = Ray.ray (origin cam) (lowerLeftCorner cam `V.add`
                                       V.mul (horizontal cam) u `V.add`
                                       V.mul (vertical cam) v `V.diff`
                                       origin cam)

