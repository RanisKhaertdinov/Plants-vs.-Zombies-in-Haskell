{-# LANGUAGE PartialTypeSignatures #-}
module Bullet where

import Graphics.Gloss
    ( Picture, blank, Color, circleSolid, translate, color
    , Picture(Pictures), Picture(Translate)
    )
import Plant (Plant(..), PlantType(..))
import GameStates (GameState(..))
import Data.Fixed (mod')

generateBullet :: Plant -> Float -> GameState-> _ -> Picture
generateBullet plant@(Plant Peashooter (x, y) _) time gameMod green =
    case gameMod of
        Playing _ ->
            let bulSpeed = 200
                distance = 600 - x
                interval = 1.5
                bulletTimes = [time, time - interval, time - 2 * interval]
                bulletPics = [ let t = mod' (bulSpeed * t') distance
                                   bulX = x + t
                               in if t' >= 0
                                  then translate bulX y $ color green $ circleSolid 10
                                  else blank
                             | t' <- bulletTimes
                           ]
            in Pictures bulletPics
        GameOver -> blank
        SelectingPlant _ -> blank
generateBullet _ _ _ green = blank