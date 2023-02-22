
module Scatter(scatter) where

import Color(Color, color)
import qualified Hit as H
import qualified Material as M
import qualified Ray as R
import RTWeekend(randomDouble)
import qualified Vec3 as V



scatter :: R.Ray -> H.HitRecord -> IO (Maybe (Color, R.Ray))
scatter R.Ray{R.direction=inDir} H.HitRecord{H.phit=p,
                                             H.normal=norm,
                                             H.material=mat,
                                             H.frontFace=frontFace} =

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

    M.Dielectric ir -> do
      rd <- randomDouble
      let attenuation     = color 1 1 1
          refractionRatio = if frontFace then 1/ir else ir
          unitDirection   = V.unitVec inDir
          cosTheta        = min (V.neg unitDirection `V.dot` norm) 1
          sinTheta        = sqrt (1-cosTheta*cosTheta)
          cannotRefract   = refractionRatio * sinTheta > 1;
          direction       = if cannotRefract ||
                               reflectance cosTheta refractionRatio > rd
                            then V.reflect unitDirection norm
                            else V.refract unitDirection norm refractionRatio
          scattered       = R.ray p direction
      return $ Just (attenuation, scattered)


reflectance :: Double -- cosine
            -> Double -- refraction index
            -> Double
reflectance cosine refIdx = r0' + (1-r0') * (1-cosine)**5
                            where
                              r0 = (1-refIdx) / (1+refIdx);
                              r0' = r0*r0;

