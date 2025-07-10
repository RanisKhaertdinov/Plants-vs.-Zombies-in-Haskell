{-# LANGUAGE PartialTypeSignatures #-}
module LittleSun where

import Graphics.Gloss
    ( Picture, blank, Color, circleSolid, translate, color
    , Picture(Pictures), Picture(Translate)
    )
import Plant (Plant(..), PlantType(..))
import GameStates (GameState(..))

generateSun :: Plant -> Float -> GameState -> _ -> Picture
generateSun (Plant Sunflower (x, y) _) time gameMod yellow =
    case gameMod of
        Playing _ ->
            let distance = 70
                interval = 3
                angles = [0, 2 * pi / 3, 4 * pi / 3]
                sunPics = [
                    let t' = time - fromIntegral i * interval
                        in if t' >= 0
                            then
                                let progress = min 1 t'
                                    sunX = x + progress * distance * cos angle
                                    sunY = y + progress * distance * sin angle
                                in translate sunX sunY $ color yellow $ circleSolid 20
                            else blank
                    | (i, angle) <- zip [0..] angles
                  ]
            in Pictures sunPics
        GameOver -> blank
        SelectingPlant _ -> blank
generateSun _ _ _ yellow = blank