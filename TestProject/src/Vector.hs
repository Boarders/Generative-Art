module Vector where

import           Linear.V2     (V2(..))
import           Linear.Matrix (M22, (!*))


rotationMatrix :: Double -> M22 Double
rotationMatrix theta = V2 (V2 (cos theta) (sin theta)) (V2 (- sin theta) (cos theta))

applyMatrix :: M22 Double -> V2 Double -> V2 Double
applyMatrix  = (!*)

-- This is simply to avoid dependence on lens as this is all we need
xProj :: V2 a -> a
xProj (V2 x _) = x

yProj :: V2 a -> a
yProj (V2 _ y) = y

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)
