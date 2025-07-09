module LittleSun where

import Graphics.Gloss
import Plant
import GameStates
import Data.Fixed (mod')



generateSun :: Plant -> Float -> GameState -> Picture
generateSun (Plant Sunflower (x, y) _ ) time gameMod =
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
                                in Translate sunX sunY $ Color yellow $ circleSolid 20
                            else blank
                    | (i, angle) <- zip [0..] angles
                  ]
            in Pictures sunPics
        GameOver -> blank
generateSun _ _ _ = blank
