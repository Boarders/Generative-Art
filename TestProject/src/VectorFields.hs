module VectorFields where

import           Data.Semigroup ((<>))
import           Linear.V2      (V2(V2))
import           Data.Complex   (Complex((:+)), polar)

import Tri

type Holomorphic = (Complex Double) -> (Complex Double)

holoEx :: Holomorphic
holoEx = id

vectToComplex :: V2 Double -> Complex Double
vectToComplex (V2 x y) = x :+ y

-- Returns the angle of the vector in vector field at the given point.
angleAtVect :: V2 Double -> Holomorphic -> Double
angleAtVect vec func = snd.polar $ func complexPt
  where complexPt = vectToComplex vec

translateHolo :: V2 Double -> Holomorphic
translateHolo  (V2 x y)  = \(x' :+ y') -> ((x + x') :+ (y + y'))
