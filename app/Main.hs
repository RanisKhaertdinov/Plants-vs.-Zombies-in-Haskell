module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameMap
import Zombie
import Plant 
import GameStates
import Bullet
import LittleSun
import PlantCards 

-- | Критическая точка, при достижении которой игра заканчивается
criticalX :: Float
criticalX = -400

baseZombies = [
    (Zombie (Position 333 0 50 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1))
    , (Zombie (Position 266 1 50 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1))
    , (Zombie (Position 200 2 50 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1))
    , (Zombie (Position 266 3 50 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1))
    , (Zombie (Position 333 4 50 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1))
    ]

main :: IO ()
main = do
    map <- generateMap
    animate (InWindow "PvZ" (800, 600) (50, 50)) black (frame map)

frame :: Picture -> Float -> Picture
frame mapPic time =
    let x = time
        -- zombie1 = Translate x 0 (generateZombie (Zombie (Position 0 0 (0, 0) (0, 0)) 10 (Coloring 1 1 1 1)))
        -- zombie2 = if x > criticalX
        --          then Translate x 66.6 (generateZombie (Zombie (Position 0 0 (ZHitbox (0, 0) (0, 0))) 10 (Coloring 1 1 1 1)))
        --          else Translate criticalX 0 (generateZombie (Zombie (Position 0 0 (ZHitbox (0, 0) (0, 0))) 0 (Coloring 1 1 1 1)))
        zombies = updateAllZ baseZombies time
        picZ = animateAllZ zombies time

        gameState = if (checkFinish zombies criticalX)
                   then GameOver
                   else Playing time
        plant1 = generatePlant (Plant Sunflower (-200, 100) 100)
        plant2 = generatePlant (Plant Peashooter (-200, 0) 200) 
        bullet = generateBullet (Plant Peashooter (-200, 0) 200) time gameState
        sun = generateSun (Plant Sunflower (-200, 100) 100) time gameState
        cards = renderPlantCards availableCards
    in case gameState of
         GameOver -> Pictures ([mapPic, plant1, plant2, bullet, sun, cards, gameOverText]++picZ)
         Playing _ -> Pictures ([mapPic, plant1, plant2, bullet, sun, cards]++picZ)
         SelectingPlant _ -> Pictures [mapPic, cards]

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

