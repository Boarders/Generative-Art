module Main where

import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.Random       (runRandT)
import           System.Random              (mkStdGen)
import           Control.Monad.Random.Class (MonadRandom(getRandomR), uniform, weighted)
import           Data.Foldable              (for_)
import           Data.Semigroup             ((<>))
import           Data.Time.Clock.POSIX      (getPOSIXTime)
import           Graphics.Rendering.Cairo
import           Linear.V2                  (V2(..))

import           Colours
import           VectorFields
import           Vector
import           Tri
import           GenerateHelpers

main :: IO ()
main = do
  seed <- round. (*17) <$> getPOSIXTime
  let
    stdGen      = mkStdGen seed
    width       = 60
    height      = 60
    scaleAmount = 20

    scaledWidth  = round $ fromIntegral width * scaleAmount
    scaledHeight = round $ fromIntegral height * scaleAmount

  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  let world = World width height seed scaleAmount

  renderWith surface . flip runReaderT world . flip runRandT stdGen
      $ do
        cairo $ scale scaleAmount scaleAmount
        renderSketch

  putStrLn "Generating ```art'''!"
  surfaceWriteToPNG surface $
    "images/example_sketch/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG surface "images/example_sketch/latest.png"

generateTri :: Int -> Holomorphic -> Generate [Tri]
generateTri reps func = do
  (w, h) <- getSize @Double
  cent   <- getCentre
  case reps of
    0 -> return []
    n -> do
      v <- V2 <$> getRandomR (2 , w - 2) <*> getRandomR (2 , h - 2)
      let vNorm = v - cent
      case func (vectToComplex vNorm) of
        0   -> generateTri reps func
        val -> do
          let theta = angleAtVect vNorm func
          let rot   = rotateTri theta
          let trans = translateTri v
          let tri = trans.rot $ standardTri
          restTris <- generateTri (reps - 1) func
          return (tri : restTris)

genTris ::  Generate [Tri]
genTris =  generateTri 1500  holoEx

renderSketch :: Generate ()
renderSketch = do
  fillScreen canvas 1

  cairo $ setLineWidth 0.15

  tris <- genTris
  (w, h) <- getSize @Double
  let cent = V2 (w/2) (h/2)

  for_ tris $ \tri -> do
    strokeOrFill <- weighted [(fill, 1), (stroke, 0)]
    color <- uniform
             [ aqua
             , vividTangerine
             , englishVermillion
             , indigo
             ]

    cairo $  do
      renderTri tri
      color 1 *> strokeOrFill
