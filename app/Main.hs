module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameMap
import Plant
import GameStates
import Bullet
import LittleSun
import PlantCards
import LawnMower (LawnMower(..), initialLawnMowers, renderLawnMower, updateMowers, activateMower)
import qualified Collision as C
import Data.List (foldl', find, lookup)
import Data.Maybe (listToMaybe)
import GameTypes (Position(..), Zombie(..), Coloring(..), posLane, zombiePos)
import qualified Zombie as Z
import Debug.Trace

-- Константы игры
criticalX :: Float
criticalX = -400  -- Left edge of the field (house)

sunInterval :: Float
sunInterval = 3

-- Для отладки: использовать singleZombie для проверки коллизий
debugMode :: Bool
debugMode = False -- Установите False для полного V-образного строя

baseZombies :: [Z.Zombie]
baseZombies = if debugMode
  then [Z.Zombie (Position 400 2 40 (400, 0) (30, 30)) 10 (Coloring 1 1 1 1)]  -- Один зомби в lane 2
  else [
    -- Leader at front (center, lane 2)
    Z.Zombie (Position 400 2 40 (400, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    -- Second row, slightly behind (lanes 1 and 3)
    Z.Zombie (Position 425 1 40 (425, -66.6) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 425 3 40 (425, 66.6) (30, 30)) 10 (Coloring 1 1 1 1),
    -- Third row, further behind (lanes 0 and 4)
    Z.Zombie (Position 450 0 40 (450, -133.2) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 450 4 40 (450, 133.2) (30, 30)) 10 (Coloring 1 1 1 1)
  ]

main :: IO ()
main = do
    map <- generateMap
    play (InWindow "PvZ" (800, 600) (50, 50)) black 60
        (Playing [] 0 500 [] [] initialLawnMowers baseZombies)
        (\gs -> Pictures [map, renderGameState gs])
        handleEvent
        updateGame

renderGameState :: GameState -> Picture
renderGameState gs = Pictures $ allPictures
  where
    currentTime = case gs of
      Playing _ t _ _ _ _ _ -> t
      SelectingPlant _ t _ _ _ _ _ _ -> t
      GameOver -> 0

    plants = case gs of
      Playing ps _ _ _ _ _ _ -> ps
      SelectingPlant ps _ _ _ _ _ _ _ -> ps
      GameOver -> []

    suns = case gs of
      Playing _ _ _ suns _ _ _ -> suns
      SelectingPlant _ _ _ _ suns _ _ _ -> suns
      GameOver -> []

    currentSun = case gs of
      Playing _ _ sun _ _ _ _ -> sun
      SelectingPlant _ _ _ sun _ _ _ _ -> sun
      GameOver -> 0

    zombies = case gs of
      Playing _ _ _ _ _ _ zs -> zs
      SelectingPlant _ _ _ _ _ _ _ zs -> zs
      GameOver -> []

    picZ = Z.animateAllZ zombies ++
           [Translate (-300) 200 $ Color white $ Scale 0.2 0.2 $ Text $ show (map (\z -> (fst (posCoord (zombiePos z)), posLane (zombiePos z), zombieHealth z)) zombies)]

    plantPics = map generatePlant plants
    bulletPics = map (\p -> generateBullet p currentTime gs) plants
    sunPics = map renderSun suns
    cards = renderPlantCards currentSun availableCards

    sunDisplay = Translate 300 250 $ Pictures
      [ Color yellow $ circleSolid 20
      , Color yellow $ Translate 30 (-7) $ Scale 0.3 0.3 $ Text (show currentSun)
      ]

    lawnMowers = case gs of
      Playing _ _ _ _ _ mowers _ -> mowers
      SelectingPlant _ _ _ _ _ _ mowers _ -> mowers
      GameOver -> []

    lawnMowerPics = map renderLawnMower lawnMowers

    allPictures = case gs of
      GameOver ->
        [ sunDisplay
        , cards
        , gameOverText
        ] ++ plantPics ++ bulletPics ++ sunPics ++ picZ ++ lawnMowerPics
      _ ->
        [ sunDisplay
        , cards
        ] ++ plantPics ++ bulletPics ++ sunPics ++ picZ ++ lawnMowerPics

gameOverText :: Picture
gameOverText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "Game Over!"

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
    case state of
        Playing plants t sun suns sunTimers mowers zombies
            | y < 200 ->
                let clickedSuns = filter (\s -> isSunClicked s (x, y)) suns
                    remainingSuns = filter (\s -> not (isSunClicked s (x, y))) suns
                    collectedValue = sum (map value clickedSuns)
                in Playing plants t (sun + collectedValue) remainingSuns sunTimers mowers zombies
            | y > 200 && y < 350 ->
                let idx = floor ((x + 350) / 120)
                in if idx >= 0 && idx < length availableCards
                   then let card = availableCards !! idx
                        in if sun >= cost card
                           then SelectingPlant plants t (cardType card) sun suns sunTimers mowers zombies
                           else state
                   else state
            | otherwise -> state

        SelectingPlant plants t plantType sun suns sunTimers mowers zombies
            | y < 200 ->
                let newPlant = Plant plantType (x, y) 100
                    card = head $ filter (\c -> cardType c == plantType) availableCards
                    newSun = sun - cost card
                in Playing (newPlant : plants) t newSun suns sunTimers mowers zombies
            | y >= 200 -> Playing plants t sun suns sunTimers mowers zombies
            | otherwise -> state

        _ -> state
handleEvent _ state = state

updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t sun suns sunTimers mowers zombies) =
  let newTime = t + dt
      -- Update zombies before collision
      updatedZombies = Z.updateAllZ zombies newTime
      -- Process collisions, killing all colliding zombies
      (activatedMowers, collidedZombies) = processCollisions mowers updatedZombies
      -- Remove dead zombies
      finalZombies = Z.clearDead collidedZombies

      movedMowers = updateMowers dt activatedMowers

      -- Генерация солнц
      sunflowerPlants = [p | p@(Plant Sunflower (x, y) _) <- plants]
      plantsWithTimers = [(p, lastSunTime p) | p <- sunflowerPlants]
      generatedSuns = generateSun plantsWithTimers newTime suns
      allSuns = updateSuns dt (suns ++ generatedSuns)

      lastSunTime (Plant Sunflower (x,y) _) =
        case lookup (x, y) sunTimers of
          Just tm -> tm
          Nothing -> 0

      updatedTimers = foldl' updateTimer sunTimers
        [ (x,y) | (Plant Sunflower (x,y) _, lastT) <- plantsWithTimers
                , newTime - lastT >= 10 ]

      updateTimer acc pos = (pos, newTime) : filter ((/= pos) . fst) acc
  in if Z.checkFinish finalZombies criticalX
     then GameOver
     else Playing plants newTime sun allSuns updatedTimers movedMowers finalZombies
  where
    processCollisions ms zs =
      let collisions = [(i, z) | (m, i) <- zip ms [0..], z <- zs, not (isActive m), isColliding z m]
          updatedMowers = foldl' (\ms' (i, _) -> activateMower i ms') ms collisions
          updatedZombies = foldl' (\zs' (_, z) -> map (\z' -> if z' == z then trace ("Killing zombie at " ++ show (posCoord (zombiePos z'))) (Z.hitZombie z' (zombieHealth z')) else z') zs') zs collisions
      in (updatedMowers, Z.clearDead updatedZombies)

    isColliding z m =
      not (isActive m) &&
      lawnLane m == posLane (zombiePos z) &&
      C.checkCollision z m

updateGame dt (SelectingPlant plants t plantType sun suns sunTimers mowers zombies) =
  let newTime = t + dt
      updatedZombies = Z.updateAllZ zombies newTime
      (newMowers, collidedZombies) = processCollisions mowers updatedZombies
      finalZombies = Z.clearDead collidedZombies
      movedMowers = updateMowers dt newMowers
  in if Z.checkFinish finalZombies criticalX
     then GameOver
     else SelectingPlant plants newTime plantType sun suns sunTimers movedMowers finalZombies
  where
    processCollisions ms zs =
      let collisions = [(i, z) | (m, i) <- zip ms [0..], z <- zs, not (isActive m), isColliding z m]
          updatedMowers = foldl' (\ms' (i, _) -> activateMower i ms') ms collisions
          updatedZombies = foldl' (\zs' (_, z) -> map (\z' -> if z' == z then trace ("Killing zombie at " ++ show (posCoord (zombiePos z'))) (Z.hitZombie z' (zombieHealth z')) else z') zs') zs collisions
      in (updatedMowers, Z.clearDead updatedZombies)

    isColliding z m =
      not (isActive m) &&
      lawnLane m == posLane (zombiePos z) &&
      C.checkCollision z m

updateGame _ GameOver = GameOver