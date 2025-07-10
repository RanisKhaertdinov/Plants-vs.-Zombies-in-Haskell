module PlantCards  where

import Graphics.Gloss
import Plant

renderPlantCards :: [PlantCard] -> Picture
renderPlantCards cards = pictures
    [ translate xPos 250 $ pictures
        [ color (makeColor 0.2 0.8 0.2 0.7) $ rectangleSolid 80 100
        , color black $ translate (-30) (-40) $ scale 0.2 0.2 $ text (show (cost card))
        , case cardType card of
            Sunflower -> color yellow $ translate 0 10 $ circleSolid 20
            Peashooter -> color green $ translate 0 10 $ circleSolid 20
            WallNut -> color (makeColor 0.5 0.3 0.1 1.0) $ translate 0 10 $ rectangleSolid 30 50
        ]
    | (card, idx) <- zip cards [(0 :: Int)..]
    , let xPos = -300 + fromIntegral idx * 120
    ]
