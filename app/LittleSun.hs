module LittleSun where

import Graphics.Gloss
import Plant
import GameStates
import Data.Fixed (mod')

generateSun :: Plant -> Float -> GameState -> (Picture, Int)
generateSun plant time gameMod = case plant of
    (Plant Sunflower (x, y) _) ->
        case gameMod of
            Playing {} ->
                let distance = 70
                    interval = 3
                    angles = [0, 2 * pi / 3, 4 * pi / 3]
                    sunPics = [
                        let t' = time - fromIntegral (i::Int) * interval
                        in if t' >= 0 && t' < interval
                            then let progress = min 1 (t' / interval)
                                 in Translate (x + progress * distance * cos angle)
                                             (y + progress * distance * sin angle)
                                             (Color yellow $ circleSolid 20)
                            else blank
                        | (i, angle) <- zip [0..2] angles
                      ]
                  
                    generatedSun = if mod' time interval < 0.1 then 25 else 0
                in (Pictures sunPics, generatedSun)
            _ -> (blank, 0)
    _ -> (blank, 0)