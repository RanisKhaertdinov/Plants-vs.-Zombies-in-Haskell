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
    play (InWindow "PvZ" (800, 600) (50, 50)) black 60 (Playing [] 0 500) (frame map) handleEvent updateGame

frame :: Picture -> GameState -> Picture
frame mapPic gs = Pictures allPictures
  where
    currentTime = case gs of
      Playing _ t _ -> t
      SelectingPlant _ t _ _ -> t
      GameOver -> 0

    plants = case gs of
      Playing ps _ _ -> ps
      SelectingPlant ps _ _ _ -> ps
      GameOver -> []

    sunResults = map (\p -> generateSun p currentTime gs) plants
    sunPics = map fst sunResults
    totalSunGenerated = sum (map snd sunResults)

    currentSun = case gs of
      Playing _ _ sun -> sun + totalSunGenerated
      SelectingPlant _ _ _ sun -> sun + totalSunGenerated
      GameOver -> 0

    zombies = updateAllZ baseZombies currentTime
    picZ = animateAllZ zombies currentTime

    plantPics = map generatePlant plants
    bulletPics = map (\p -> generateBullet p currentTime gs) plants

    cards = renderPlantCards currentSun availableCards

    sunDisplay = Translate 300 250 $ Pictures
      [ Color yellow $ circleSolid 20  -- Иконка солнца
      , Color yellow $ Translate 30 (-7) $ Scale 0.3 0.3 $ Text (show currentSun)  -- Количество
      ]

    allPictures = case gs of
      GameOver ->
        [ mapPic
        , sunDisplay
        , cards
        , gameOverText
        ] ++ plantPics ++ bulletPics ++ sunPics ++ picZ
      _ ->
        [ mapPic
        , sunDisplay
        , cards
        ] ++ plantPics ++ bulletPics ++ sunPics ++ picZ

gameOverText :: Picture
gameOverText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "Game Over!"

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
    case state of
        Playing plants t sun
            | x > 250 && x < 300 && y > 220 && y < 280 ->
                let sunResults = map (\p -> generateSun p t state) plants
                    collectedSun = sum (map snd sunResults)
                in Playing plants t (sun + collectedSun)

            | y > 200 && y < 350 ->
                let idx = floor ((x + 350) / 120)
                in if idx >= 0 && idx < length availableCards
                   then let card = availableCards !! idx
                        in if sun >= cost card
                           then SelectingPlant plants t (cardType card) sun
                           else state
                   else state
            | otherwise -> state

        SelectingPlant plants t plantType sun
            | y < 200 ->
                let newPlant = Plant plantType (x, y) 100
                    card = head $ filter (\c -> cardType c == plantType) availableCards
                    newSun = sun - cost card
                in Playing (newPlant : plants) t newSun

            | otherwise -> Playing plants t sun
        _ -> state

handleEvent _ state = state

handlePlantSelection :: Float -> Float -> GameState -> GameState
handlePlantSelection x y (Playing plants t sun)
    | y > 200 && y < 350 = -- Клик по карточкам
        let idx = floor ((x + 350) / 120)
        in if idx >= 0 && idx < length availableCards
           then let card = availableCards !! idx
                in if sun >= cost card
                   then SelectingPlant plants t (cardType card) sun
                   else Playing plants t sun
           else Playing plants t sun
    | otherwise = Playing plants t sun

updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t sun) = Playing plants (t + dt) sun
updateGame dt (SelectingPlant plants t plantType sun) = SelectingPlant plants (t + dt) plantType sun
updateGame _ (GameOver) = GameOver
updateGame _ gs = gs