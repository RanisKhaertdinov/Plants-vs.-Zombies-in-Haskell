module Main where

import Graphics.Gloss
import GameMap
import Zombie
import Plant
-- | Состояние игры
data GameState = Playing Float  -- Время игры
               | GameOver       -- Игра окончена

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
        plant = Translate (-200) 0 generatePlant
    in case gameState of
         GameOver -> Pictures [mapPic, zombie, plant, gameOverText]
         Playing _ -> Pictures [mapPic, zombie, plant]

gameOverText :: Picture
gameOverText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "Game Over!"
