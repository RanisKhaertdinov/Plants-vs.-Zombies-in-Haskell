module Plant where

import Graphics.Gloss
import Data.Fixed (mod')
import GameTypes

data Plant  -- (dx, dy) health
    = Plant PlantType (Float, Float) Int

data PlantType
    = Sunflower
    | Peashooter


generatePlant :: Plant -> Picture 
generatePlant (Plant Sunflower (x, y) health) = 
    if health > 0 
    then Translate x y $ Color (makeColor 1.0 1.0 1.0 1) $ circleSolid 30
    else blank
generatePlant (Plant Peashooter (x, y) health) = 
    if health > 0 
    then Translate x y $ Color (makeColor 0.0 1.0 1.0 1) $ circleSolid 30
    else blank


generateBullet :: Plant -> Float -> GameState-> Picture
generateBullet plant@(Plant Peashooter (x, y) _) time gameMod =
    case gameMod of
        Playing _ ->
            let bulSpeed = 200
                distance = 600 - x
                interval = 1.5
                bulletTimes = [time, time - interval, time - 2 * interval]
                bulletPics = [ let t = mod' (bulSpeed * t') distance
                                   bulX = x + t
                               in if t' >= 0
                                  then Translate bulX y $ Color (makeColor 0.0 1.0 1.0 1) $ circleSolid 10
                                  else blank
                             | t' <- bulletTimes
                           ]
            in Pictures bulletPics
        GameOver -> blank
generateBullet _ _ _ = blank


