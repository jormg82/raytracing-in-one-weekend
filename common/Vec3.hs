
module Vec3 where


data Vec3 = Vec3 { x :: Double, y :: Double, z :: Double }
            deriving Show

type Point3 = Vec3


eps :: Double
eps = 1.0e-8

vec3 :: Double -> Double -> Double -> Vec3
vec3 = Vec3

neg :: Vec3 -> Vec3
neg (Vec3 a b c) = Vec3 (-a) (-b) (-c)

len :: Vec3 -> Double
len = sqrt . len_squared

len_squared :: Vec3 -> Double
len_squared (Vec3 a b c) = a*a + b*b + c*c

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

dot :: Vec3 -> Vec3 -> Double
dot (Vec3 a b c) (Vec3 a' b' c') = a*a' + b*b' + c*c'

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a b c) (Vec3 a' b' c') = Vec3 (b*c'-b'*c) (a'*c-a*c') (a*b'-a'*b)

unitVec :: Vec3 -> Vec3
unitVec v = divi v (len v)

