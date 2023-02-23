

module Camera (
  Camera(),
  camera,
  getRay 
) where

import qualified Ray
import RTWeekend(degreesToRadians)
import qualified Vec3 as V

data Camera = Camera
  {
    origin          :: V.Point3,
    lowerLeftCorner :: V.Point3,
    horizontal      :: V.Vec3,
    vertical        :: V.Vec3,
    u               :: V.Vec3,
    v               :: V.Vec3,
    w               :: V.Vec3,
    lensRadius      :: Double
  }
  deriving Show



camera :: V.Point3 -- Look from
       -> V.Point3 -- Look at
       -> V.Vec3   -- Vup
       -> Double   -- FOV
       -> Double   -- Aspect ratio
       -> Double   -- Aperture
       -> Double   -- Focus dist
       -> Camera
camera lookFrom lookAt vup vfov ar aperture focusDist =
  Camera{origin=lookFrom,
         lowerLeftCorner=corner,
         horizontal=hor,
         vertical=ver,
         u=u',
         v=v',
         w=w',
         lensRadius=aperture/2
        }
  where
    theta  = degreesToRadians vfov
    h      = tan (theta/2)
    vh     = 2 * h
    vw     = ar * vh
    w'     = V.unitVec $ lookFrom `V.diff` lookAt
    u'     = V.unitVec $ vup `V.cross` w'
    v'     = w' `V.cross` u'
    hor    = u' `V.mul` (vw*focusDist)
    ver    = v' `V.mul` (vh*focusDist)
    corner = lookFrom `V.diff` V.divi hor 2 `V.diff` V.divi ver 2
             `V.diff` V.mul w' focusDist



getRay :: Camera -- Camera
       -> Double -- Horizontal s
       -> Double -- Vertical t
       -> IO Ray.Ray
getRay cam s t = do
  riud <- V.randomInUnitDisk
  let rd     = riud `V.mul` lensRadius cam
      offset = V.mul (u cam) (V.x rd) `V.add` V.mul (v cam) (V.y rd)
  return $ Ray.ray (origin cam `V.add` offset)
                   (lowerLeftCorner cam `V.add`
                    V.mul (horizontal cam) s `V.add`
                    V.mul (vertical cam) t `V.diff`
                    origin cam `V.diff` offset)

