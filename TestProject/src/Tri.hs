{-# LANGUAGE RecordWildCards #-}
module Tri where

import           Linear.V2 (V2(..))

import Vector

data Tri = Tri
  { tVert   :: V2 Double
  , lVert   :: V2 Double
  , rVert   :: V2 Double
  }

-- Triangle centred at the origin
standardTri :: Tri
standardTri = Tri
  { tVert = V2    0    (  (sqrt 5)/3)
  , lVert = V2 (- 0.5) (- (sqrt 5)/6)
  , rVert = V2    0.5  (- (sqrt 5)/6)
  }


--rotate Triangle centred at origin
rotateTri :: Double -> Tri -> Tri
rotateTri theta Tri{..} = Tri
  (applyMatrix rot tVert)
  (applyMatrix rot lVert)
  (applyMatrix rot rVert)
    where rot = rotationMatrix theta

translateTri  :: V2 Double -> Tri -> Tri
translateTri trans tri@Tri{..} = Tri
  (tVert + trans)
  (lVert + trans)
  (rVert + trans)
