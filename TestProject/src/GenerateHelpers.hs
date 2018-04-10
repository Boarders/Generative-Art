module GenerateHelpers where

import           Control.Arrow             ((&&&))
import           Control.Monad.Random      (RandT)
import           Control.Monad.Reader      (ReaderT, asks)
import           System.Random             (StdGen)
import           Control.Monad.Trans.Class (lift)
import           Data.Colour.RGBSpace      (RGB(RGB), channelRed, channelGreen, channelBlue)
import           Data.Colour.RGBSpace.HSV  (hsv)
import           Data.Foldable             (for_)
import           Data.List                 (nub)
import           Data.Semigroup            ((<>))
import           Graphics.Rendering.Cairo
import           Linear.V2                 (V2(..))

import           Colours
import           VectorFields
import           Vector
import           Tri

data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Double
  }


type Generate a = RandT StdGen (ReaderT World Render) a

cairo :: Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

getCentre :: Generate (V2 Double)
getCentre = do
  (w, h) <- getSize @Double
  return (V2 (w/2) (h/2))

fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

renderClosedPath :: [V2 Double] -> Render ()
renderClosedPath ((V2 x y): vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
  closePath
renderClosedPath [] = pure ()

renderTri :: Tri -> Render ()
renderTri Tri{..} = renderClosedPath [tVert, lVert, rVert]

normaliseVectorField :: Holomorphic -> Generate Holomorphic
normaliseVectorField func = do
  (w, h) <- getSize @Double
  let cent = V2 (w/2) (h/2)
  let translateToCent   = translateHolo cent
  let translateFromCent = translateHolo (negate cent)
  let newFunc = translateToCent. func
  return newFunc
