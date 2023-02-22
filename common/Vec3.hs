
module Vec3(
  Vec3(..),
  Point3,
  vec3, point3,
  eps,
  neg,
  len, lenSquared,
  nearZero,
  add, diff, mul, divi,
  dot, prod, cross,
  unitVec,
  random, randomR,
  randomInUnitSphere,
  randomUnitVector,
  reflect, refract
) where

import qualified RTWeekend as RT

import Control.Monad.Loops(iterateUntil)


data Vec3 = Vec3 { x :: Double, y :: Double, z :: Double }
            deriving Show

type Point3 = Vec3


eps :: Double
eps = 1.0e-8

vec3 :: Double -> Double -> Double -> Vec3
vec3 = Vec3

point3 :: Double -> Double -> Double -> Point3
point3 = Vec3

neg :: Vec3 -> Vec3
neg (Vec3 a b c) = Vec3 (-a) (-b) (-c)

len :: Vec3 -> Double
len = sqrt . lenSquared

lenSquared :: Vec3 -> Double
lenSquared (Vec3 a b c) = a*a + b*b + c*c

nearZero :: Vec3 -> Bool
nearZero (Vec3 a b c) = abs a < eps && abs b < eps && abs c < eps

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 a b c) (Vec3 a' b' c') = Vec3 (a+a') (b+b') (c+c')

diff :: Vec3 -> Vec3 -> Vec3
diff (Vec3 a b c) (Vec3 a' b' c') = Vec3 (a-a') (b-b') (c-c')

mul :: Vec3  -> Double -> Vec3
mul (Vec3 a b c) t = Vec3 (a*t) (b*t) (c*t)

divi :: Vec3 -> Double -> Vec3
divi v t = mul v (1.0/t)

prod :: Vec3 -> Vec3 -> Vec3 
prod (Vec3 a b c) (Vec3 a' b' c') = Vec3 (a*a') (b*b') (c*c')

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 a b c) (Vec3 a' b' c') = a*a' + b*b' + c*c'

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 a' b' c') = Vec3 (b*c'-b'*c) (a'*c-a*c') (a*b'-a'*b)

unitVec :: Vec3 -> Vec3
unitVec v = divi v (len v)

random :: IO Vec3
random = vec3 <$> RT.randomDouble <*> RT.randomDouble <*> RT.randomDouble

randomR :: Double -> Double -> IO Vec3
randomR m n = let rd = RT.randomDoubleR m n
              in vec3 <$> rd <*> rd <*> rd

randomInUnitSphere :: IO Vec3
randomInUnitSphere = iterateUntil ((<1) . lenSquared) $ randomR (-1) 1

randomUnitVector :: IO Vec3
randomUnitVector = unitVec <$> randomInUnitSphere

reflect :: Vec3 -> Vec3 -> Vec3
reflect vec normal = vec `diff` mul normal (2 * dot vec normal)

refract :: Vec3
        -> Vec3
        -> Double
        -> Vec3
refract uv n etaiOveretat = rOutPerp `add` rOutParallel
  where
    cosTheta = min (neg uv `dot` n) 1
    rOutPerp = mul (uv `add` mul n cosTheta) etaiOveretat
    rOutParallel = mul n (-sqrt (abs $ 1-lenSquared rOutPerp))

{---------------------
vec3 refract(const vec3& uv, const vec3& n, double etai_over_etat) {
    auto cos_theta = fmin(dot(-uv, n), 1.0);
    vec3 r_out_perp =  etai_over_etat * (uv + cos_theta*n);
    vec3 r_out_parallel = -sqrt(fabs(1.0 - r_out_perp.length_squared())) * n;
    return r_out_perp + r_out_parallel;
}
---------------------}
