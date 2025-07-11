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

sunInterval :: Float
sunInterval = 3

baseZombies = [
      Zombie (Position 333 0 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1)
    , Zombie (Position 266 1 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1)
    , Zombie (Position 200 2 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1)
    , Zombie (Position 266 3 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1)
    , Zombie (Position 333 4 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1)
    ]

main :: IO ()
main = do
    map <- generateMap
    play (InWindow "PvZ" (800, 600) (50, 50)) black 60 (Playing [] 0 500 [] []) (frame map) handleEvent updateGame

frame :: Picture -> GameState -> Picture
frame mapPic gs = Pictures allPictures
  where
    currentTime = case gs of
      Playing _ t _ _ _ -> t
      SelectingPlant _ t _ _ _ _ -> t
      GameOver -> 0 

    plants = case gs of
      Playing ps _ _ _ _ -> ps
      SelectingPlant ps _ _ _ _ _ -> ps
      GameOver -> []

    suns = case gs of
      Playing _ _ _ suns _ -> suns
      SelectingPlant _ _ _ _ suns _ -> suns
      GameOver -> []

    currentSun = case gs of
      Playing _ _ sun _ _ -> sun
      SelectingPlant _ _ _ sun _ _ -> sun
      GameOver -> 0

    zombies = updateAllZ baseZombies currentTime
    picZ = animateAllZ zombies currentTime

    plantPics = map generatePlant plants
    bulletPics = map (\p -> generateBullet p currentTime gs) plants

    sunPics = map renderSun suns

    cards = renderPlantCards currentSun availableCards

    sunDisplay = Translate 300 250 $ Pictures
      [ Color yellow $ circleSolid 20 
      , Color yellow $ Translate 30 (-7) $ Scale 0.3 0.3 $ Text (show currentSun)  
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
        Playing plants t sun suns sunTimers
            | y < 200 ->
                let clickedSuns = filter (\s -> isSunClicked s (x, y)) suns
                    remainingSuns = filter (\s -> not (isSunClicked s (x, y))) suns
                    collectedValue = sum (map value clickedSuns)
                in Playing plants t (sun + collectedValue) remainingSuns sunTimers
            | y > 200 && y < 350 ->
                let idx = floor ((x + 350) / 120)
                in if idx >= 0 && idx < length availableCards
                   then let card = availableCards !! idx
                        in if sun >= cost card
                           then SelectingPlant plants t (cardType card) sun suns sunTimers
                           else state
                   else state
            | otherwise -> state
        SelectingPlant plants t plantType sun suns sunTimers
            | y < 200 ->
                let newPlant = Plant plantType (x, y) 100
                    card = head $ filter (\c -> cardType c == plantType) availableCards
                    newSun = sun - cost card
                in Playing (newPlant : plants) t newSun suns sunTimers
            | otherwise -> Playing plants t sun suns sunTimers
        _ -> state

handleEvent _ state = state

updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t sun suns sunTimers) = 
    let newTime = t + dt
        zombies = updateAllZ baseZombies newTime
        sunflowerPlants = [p | p@(Plant Sunflower (x, y) _) <- plants]
        plantsWithTimers = [ (p, lastSunTime p) | p <- sunflowerPlants]
        generatedSuns = generateSun plantsWithTimers newTime suns
        allSuns = updateSuns dt (suns ++ generatedSuns)
        lastSunTime (Plant Sunflower (x,y) _ ) =
            case lookup (x, y) sunTimers of
                Just tm -> tm
                Nothing -> 0
        updatedTimers = foldl (\acc (Plant Sunflower (x, y) _, _) -> updateTimer (x, y) newTime acc) 
                            sunTimers (filter (\(_, lastT) -> newTime - lastT >= 10) plantsWithTimers)
        updateTimer pos t [] = [(pos, t)]
        updateTimer pos t ((p, oldT):xs)
            | p == pos = (p, t):xs
            | otherwise = (p, oldT) : updateTimer pos t xs
    in if checkFinish zombies criticalX
       then GameOver
       else Playing plants newTime sun allSuns updatedTimers

updateGame dt (SelectingPlant plants t plantType sun suns sunTimers) = 
    let newTime = t + dt
        zombies = updateAllZ baseZombies newTime
    in if checkFinish zombies criticalX
       then GameOver
       else SelectingPlant plants newTime plantType sun suns sunTimers

updateGame _ GameOver = GameOver
