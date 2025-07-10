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
    play (InWindow "PvZ" (800, 600) (50, 50)) black 60 (Playing [] 0) (frame map) handleEvent updateGame

frame :: Picture -> GameState -> Picture
frame mapPic gs =
    let zombies = case gs of
            Playing _ t -> updateAllZ baseZombies t
            SelectingPlant _ t _ -> updateAllZ baseZombies t
            GameOver -> updateAllZ baseZombies 0
        picZ = animateAllZ zombies (case gs of Playing _ t -> t; SelectingPlant _ t _ -> t; GameOver -> 0)
        plants = case gs of
            Playing ps _ -> ps
            SelectingPlant ps _ _ -> ps
            GameOver -> []
        plantPics = map generatePlant plants
        bulletPics = map (\p -> generateBullet p (case gs of Playing _ t -> t; SelectingPlant _ t _ -> t; GameOver -> 0) gs) plants
        sunPics = map (\p -> generateSun p (case gs of Playing _ t -> t; SelectingPlant _ t _ -> t; GameOver -> 0) gs) plants
        cards = renderPlantCards availableCards
    in case gs of
         GameOver -> Pictures ([mapPic, cards, gameOverText] ++ plantPics ++ bulletPics ++ sunPics ++ picZ)
         Playing _ _ -> Pictures ([mapPic, cards] ++ plantPics ++ bulletPics ++ sunPics ++ picZ)
         SelectingPlant _ _ _ -> Pictures ([mapPic, cards] ++ plantPics ++ bulletPics ++ sunPics ++ picZ)

gameOverText :: Picture
gameOverText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "Game Over!"

handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (Playing plants t)
    | y > 200 && y < 350 =  
        let idx = floor ((x + 350) / 120)
        in if idx >= 0 && idx < length availableCards
           then SelectingPlant plants t (cardType (availableCards !! idx))
           else Playing plants t

handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) (SelectingPlant plants t plantType)
    | y < 200 = 
        let newPlant = Plant plantType (x, y) 100
        in Playing (newPlant : plants) t
    | otherwise = SelectingPlant plants t plantType

handleEvent _ gs = gs

updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t) = Playing plants (t + dt)
updateGame dt (SelectingPlant plants t plantType) = SelectingPlant plants (t + dt) plantType
updateGame _ (GameOver) = GameOver
updateGame _ gs = gs