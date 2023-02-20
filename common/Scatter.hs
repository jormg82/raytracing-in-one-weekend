
module Scatter(scatter) where

import Color(Color)
import qualified Hit as H
import qualified Material as M
import qualified Ray as R
import qualified Vec3 as V



scatter :: R.Ray -> H.HitRecord -> IO (Maybe (Color, R.Ray))
scatter R.Ray{R.direction=inDir}
        H.HitRecord{H.phit=p, H.normal=norm, H.material=mat} =

  case mat of

    M.Lambertian albedo -> do
      uv <- V.randomUnitVector 
      let scatterDirection  = norm `V.add` uv
          scatterDirection' = if V.nearZero scatterDirection
                              then norm else scatterDirection
          scattered         = R.ray p scatterDirection'
      return $ Just (albedo, scattered) 

    M.Metal albedo fuzz -> do
      vr <- V.randomInUnitSphere
      let reflected  = V.reflect (V.unitVec inDir) norm
          reflected' = reflected `V.add` V.mul vr fuzz
          scattered  = R.ray p reflected'
      return $ if reflected' `V.dot` norm > 0
               then Just (albedo, scattered) else Nothing

