module LawnMower
    ( LawnMower(..)
    , initialLawnMowers
    , activateMowers
    , updateMowers
    , renderLawnMower
    ) where

import Graphics.Gloss
import GameTypes
import qualified Collision as C

initialLawnMowers :: [LawnMower]
initialLawnMowers = [LawnMower l (-350) False 800 | l <- [0..4]]



activateMowers :: [Zombie] -> [LawnMower] -> [LawnMower]
activateMowers zombies mowers =
    [ if not (isActive m) && any (\z -> posLane (zombiePos z) == lawnLane m && C.checkCollision z m) zombies
      then m { isActive = True }
      else m
    | m <- mowers ]

updateMowers :: Float -> [LawnMower] -> [LawnMower]
updateMowers dt = map update
    where
        update m@(LawnMower _ x active speed)
            | active = m { lawnPos = x + speed * dt }
            | otherwise = m

renderLawnMower :: LawnMower -> Picture
renderLawnMower (LawnMower lane pos active _) =
    Translate pos (fromIntegral (floor lane) * 66.6 - 2 * 66.6) $
    Color (if active then makeColor 0.9 0.9 0.1 1.0 else makeColor 0.8 0.1 0.1 1.0) $
    rectangleSolid 50 30