module Main where

import Graphics.Gloss
import GameMap
import Zombie
import Plant
import GameStates
import Bullet
import LittleSun

-- | Критическая точка, при достижении которой игра заканчивается
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
        bullet = generateBullet (Plant Peashooter (-200, 0) 200) time gameState
        sun = generateSun (Plant Sunflower (-200, 100) 100) time gameState
    in case gameState of
         GameOver -> Pictures [mapPic, zombie, plant1, plant2, bullet, sun, gameOverText]
         Playing _ -> Pictures [mapPic, zombie, plant1, plant2, bullet, sun]

gameOverText :: Picture
gameOverText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "Game Over!"
