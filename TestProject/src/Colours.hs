module Colours where

import           Data.Colour.RGBSpace      (RGB(RGB), channelRed, channelGreen, channelBlue)
import           Data.Colour.RGBSpace.HSV  (hsv)
import           Graphics.Rendering.Cairo  


hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

canvas :: Double -> Render ()
canvas = hsva 180 0.22 0.90

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

fieryOrange :: Double -> Render ()
fieryOrange = hsva 30 0.90 0.87

oliveGreen :: Double -> Render ()
oliveGreen = hsva 96 0.60 0.63

indigo :: Double -> Render ()
indigo = hsva 253 0.69 0.63

lemon :: Double -> Render ()
lemon = hsva 62 0.78 0.93

rose :: Double -> Render ()
rose = hsva 330 1 1

aqua :: Double -> Render ()
aqua = hsva 202 0.71 0.87

intense :: [RGB Double]
intense = undefined

medium :: [RGB Double]
medium = undefined

soft :: [RGB Double]
soft = undefined
