module LawnMower
    ( LawnMower(..)
    , initialLawnMowers
    , updateMowers
    , renderLawnMower
    , activateMower
    ) where

import Graphics.Gloss
import GameTypes

initialLawnMowers :: [LawnMower]
initialLawnMowers = [LawnMower l (-350) False 800 | l <- [0..4]]

updateMowers :: Float -> [LawnMower] -> [LawnMower]
updateMowers dt = map update
    where
        update m@(LawnMower _ x active speed)
            | active = m { lawnPos = x + speed * dt }
            | otherwise = m

renderLawnMower :: LawnMower -> Picture
renderLawnMower (LawnMower lane pos active _) =
    Translate pos (fromIntegral (floor lane :: Int) * 66.6 - 2 * 66.6) $
    Color (if active then makeColor 0.9 0.9 0.1 1.0 else makeColor 0.8 0.1 0.1 1.0) $
    rectangleSolid 50 30

activateSingleMower :: Int -> [LawnMower] -> [LawnMower]
activateSingleMower idx mowers =
  let (before, m:after) = splitAt idx mowers
  in before ++ [m { isActive = True, lawnSpeed = 800 }] ++ after