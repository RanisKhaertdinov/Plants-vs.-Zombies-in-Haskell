module Bullet where


import Graphics.Gloss
import Plant
import GameStates
import Data.Fixed (mod')


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