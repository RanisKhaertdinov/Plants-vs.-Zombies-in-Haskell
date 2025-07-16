module PlantCards  where

import Graphics.Gloss
import Plant

-- | Render all plant cards as selectable UI elements, showing cost and availability.
--   Highlights cards the player can afford.
renderPlantCards :: Int -> [PlantCard] -> Picture
renderPlantCards currentSun cards = pictures
    [ translate xPos 300 $ pictures
        [ color (if currentSun >= cost card
            then makeColor 0.2 0.8 0.2 0.7  -- Green if affordable
            else makeColor 0.8 0.2 0.2 0.7) -- Red if not
            $ rectangleSolid 80 100
        , case cardType card of
            Sunflower -> color yellow $ translate 0 10 $ prettySunflower
            Peashooter -> color green $ translate 0 10 $ prettyPeashooter
            WallNut -> color (makeColor 0.5 0.3 0.1 1.0) $ translate 0 10 $ prettyWallNut
        , color black $ translate (-30) (-40) $ scale 0.2 0.2 $ text (show (cost card))
        ]
    | (card, idx) <- zip cards [(0 :: Int)..]
    , let xPos = -300 + fromIntegral idx * 120
    ]
