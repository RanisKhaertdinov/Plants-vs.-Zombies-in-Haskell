module Main where

import Graphics.Gloss hiding (green, yellow)
import Graphics.Gloss.Interface.IO.Game
    ( Event(..)
    , Key(..)
    , KeyState(..)
    , MouseButton(..)
    )
import GameMap
import Zombie
import Plant
    ( PlantType(..)
    , Plant(..)
    , PlantCard(..)
    , availableCards
    , generatePlant
    )
import GameStates
import Bullet
import LittleSun
import PlantCards (renderPlantCards)

myGreen :: Color
myGreen = makeColor 0 1 0 1
myYellow :: Color
myYellow = makeColor 1 1 0 1

criticalX :: Float
criticalX = -400

main :: IO ()
main = do
    map <- generateMap
    animate (InWindow "PvZ" (800, 600) (50, 50)) black (frame map)

frame :: Picture -> Float -> Picture
frame mapPic time =
    let x = 600 - 100 * time
        zombie = if x > criticalX
                 then Translate x 0 generateZombie
                 else Translate criticalX 0 generateZombie
        gameState = if x <= criticalX
                   then GameOver
                   else Playing time
        plant1 = generatePlant (Plant Sunflower (-200, 100) 100)
        plant2 = generatePlant (Plant Peashooter (-200, 0) 200)
        bullet = generateBullet (Plant Peashooter (-200, 0) 200) time gameState myGreen
        sun = generateSun (Plant Sunflower (-200, 100) 100) time gameState myYellow
        cards = renderPlantCards availableCards
    in case gameState of
         GameOver -> Pictures [mapPic, zombie, plant1, plant2, bullet, sun, cards, gameOverText]
         Playing _ -> Pictures [mapPic, zombie, plant1, plant2, bullet, sun, cards]

gameOverText :: Picture
gameOverText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "Game Over!"

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) gs
    | y > 200 && y < 350 =
        let idx = floor ((x + 350) / 120)
        in if idx >= 0 && idx < length availableCards
           then SelectingPlant (cardType (availableCards !! idx))
           else gs
    | otherwise = gs
handleEvent _ gs = gs