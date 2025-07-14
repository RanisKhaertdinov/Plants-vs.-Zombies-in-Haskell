module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import GameMap
import Plant
import GameStates
import qualified Bullet as B
import LittleSun
import PlantCards
import LawnMower (LawnMower(..), initialLawnMowers, renderLawnMower, updateMowers, activateMower)
import qualified Collision as C
import Data.List (foldl', find, lookup)
import Data.Maybe (listToMaybe)
import GameTypes (Position(..), Zombie(..), Bullet(..), Coloring(..), posLane, zombiePos)
import qualified Zombie as Z

-- Константы игры
criticalX :: Float
criticalX = -400  -- Left edge of the field (house)

sunInterval :: Float
sunInterval = 3

debugMode :: Bool
debugMode = False

baseZombies :: [Z.Zombie]
baseZombies = if debugMode
  then [Z.Zombie (Position 400 2 40 (400, 0) (30, 30)) 10 (Coloring 1 1 1 1)]  -- Один зомби в lane 2
  else [
    -- Lane 0: Two zombies
    Z.Zombie (Position 450 0 40 (450, -133.2) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 500 0 40 (500, -133.2) (30, 30)) 10 (Coloring 1 1 1 1),
    -- Lane 1: Two zombies
    Z.Zombie (Position 425 1 40 (425, -66.6) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 475 1 40 (475, -66.6) (30, 30)) 10 (Coloring 1 1 1 1),
    -- Lane 2: Two zombies
    Z.Zombie (Position 400 2 40 (400, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 450 2 40 (450, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    -- Lane 3: Two zombies
    Z.Zombie (Position 425 3 40 (425, 66.6) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 475 3 40 (475, 66.6) (30, 30)) 10 (Coloring 1 1 1 1),
    -- Lane 4: Two zombies
    Z.Zombie (Position 450 4 40 (450, 133.2) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 500 4 40 (500, 133.2) (30, 30)) 10 (Coloring 1 1 1 1)
  ]

-- Grid definitions for plant placement
gridX :: [Float]
gridX = [-360, -280, -200, -120, -40, 40, 120, 200, 280]  -- 9 columns

gridY :: [Float]
gridY = [-133.2, -66.6, 0, 66.6, 133.2]  -- 5 rows (lanes)

snapToGrid :: (Float, Float) -> (Float, Float)
snapToGrid (x, y) =
  let closestX = head $ foldl' (\acc gx -> if abs (gx - x) < abs (head acc - x) then [gx] else acc) [head gridX] gridX
      closestY = head $ foldl' (\acc gy -> if abs (gy - y) < abs (head acc - y) then [gy] else acc) [head gridY] gridY
  in (closestX, closestY)

isCellOccupied :: [Plant] -> (Float, Float) -> Bool
isCellOccupied plants (x, y) =
  any (\(Plant _ (px, py) _) -> abs (px - x) < 1 && abs (py - y) < 1) plants

main :: IO ()
main = do
    map <- generateMap
    play (InWindow "PvZ" (800, 600) (50, 50)) black 60
        (Playing [] 0 500 [] [] initialLawnMowers baseZombies [])
        (\gs -> Pictures [map, renderGameState gs])
        handleEvent
        updateGame

renderGameState :: GameState -> Picture
renderGameState gs = Pictures $ allPictures
  where
    currentTime = case gs of
      Playing _ t _ _ _ _ _ _ -> t
      SelectingPlant _ t _ _ _ _ _ _ _ -> t
      GameOver -> 0

    plants = case gs of
      Playing ps _ _ _ _ _ _ _ -> ps
      SelectingPlant ps _ _ _ _ _ _ _ _ -> ps
      GameOver -> []

    suns = case gs of
      Playing _ _ _ suns _ _ _ _ -> suns
      SelectingPlant _ _ _ _ suns _ _ _ _ -> suns
      GameOver -> []

    currentSun = case gs of
      Playing _ _ sun _ _ _ _ _ -> sun
      SelectingPlant _ _ _ sun _ _ _ _ _ -> sun
      GameOver -> 0

    zombies = case gs of
      Playing _ _ _ _ _ _ zs _ -> zs
      SelectingPlant _ _ _ _ _ _ _ zs _ -> zs
      GameOver -> []
    
    bullets = case gs of
      Playing _ _ _ _ _ _ _ bs -> bs
      SelectingPlant _ _ _ _ _ _ _ _ bs -> bs
      GameOver -> []

    picZ = Z.animateAllZ zombies

    plantPics = map generatePlant plants
    bulletPics = B.animateAllB bullets
    -- bulletPics = map (\p -> generateBullet p currentTime gs) plants
    sunPics = map renderSun suns
    cards = renderPlantCards currentSun availableCards

    sunDisplay = Translate 300 300 $ Pictures
      [ Color yellow $ circleSolid 20
      , Color yellow $ Translate 30 (-7) $ Scale 0.3 0.3 $ Text (show currentSun)
      ]

    lawnMowers = case gs of
      Playing _ _ _ _ _ mowers _ _ -> mowers
      SelectingPlant _ _ _ _ _ _ mowers _ _ -> mowers
      GameOver -> []

    lawnMowerPics = map (renderLawnMower currentTime) lawnMowers

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
        Playing plants t sun suns sunTimers mowers zombies bullets
            | y < 200 ->
                let clickedSuns = filter (\s -> isSunClicked s (x, y)) suns
                    remainingSuns = filter (\s -> not (isSunClicked s (x, y))) suns
                    collectedValue = sum (map value clickedSuns)
                in Playing plants t (sun + collectedValue) remainingSuns sunTimers mowers zombies bullets
            | y > 200 && y < 350 ->
                let idx = floor ((x + 350) / 120)
                in if idx >= 0 && idx < length availableCards
                   then let card = availableCards !! idx
                        in if sun >= cost card
                           then SelectingPlant plants t (cardType card) sun suns sunTimers mowers zombies bullets
                           else state
                   else state
            | otherwise -> state

        SelectingPlant plants t plantType sun suns sunTimers mowers zombies bullets
            | y < 200 ->
                let (gridX, gridY) = snapToGrid (x, y)
                in if isCellOccupied plants (gridX, gridY)
                   then SelectingPlant plants t plantType sun suns sunTimers mowers zombies bullets  -- Cell occupied, stay in SelectingPlant
                   else let newPlant = Plant plantType (gridX, gridY) 100
                            card = head $ filter (\c -> cardType c == plantType) availableCards
                            newSun = sun - cost card
                            newSunTimers = if plantType == Sunflower
                                           then ((gridX, gridY), t) : sunTimers
                                           else sunTimers
                        in Playing (newPlant : plants) t newSun (suns ++ generateSun [(newPlant, t)] t suns) newSunTimers mowers zombies bullets
            | y >= 200 -> Playing plants t sun suns sunTimers mowers zombies bullets
            | otherwise -> state

        _ -> state
handleEvent _ state = state

updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t sun suns sunTimers mowers zombies bullets) =
  let newTime = t + dt
      -- Update zombies before collision
      updatedZombies = Z.updateAllZ zombies newTime
      -- Process collisions, killing all colliding zombies in the same lane
      (activatedMowers, collidedZombies) = processCollisions mowers updatedZombies
      -- Remove dead zombies
      prefinalZombies = Z.clearDead collidedZombies

      finalZombies = Z.clearDead(B.hitAllZAllB prefinalZombies (B.updateAllB bullets dt))
      nbullets = B.exhaustBullets (B.conjureAll plants newTime (B.updateAllB bullets dt)) zombies

      movedMowers = updateMowers dt activatedMowers

      -- Генерация солнц
      sunflowerPlants = [p | p@(Plant Sunflower (x, y) _) <- plants]
      plantsWithTimers = [(p, lastSunTime p) | p <- sunflowerPlants]
      generatedSuns = generateSun plantsWithTimers newTime suns
      allSuns = updateSuns dt (suns ++ generatedSuns)

      lastSunTime (Plant Sunflower (x,y) _) =
        case lookup (x, y) sunTimers of
          Just tm -> tm
          Nothing -> -1000  -- Allow immediate sun spawn for new sunflowers

      updatedTimers = foldl' updateTimer sunTimers
        [ (x,y) | (Plant Sunflower (x,y) _, lastT) <- plantsWithTimers
                , newTime - lastT >= 10 ]

      updateTimer acc pos = (pos, newTime) : filter ((/= pos) . fst) acc
  in if Z.checkFinish finalZombies criticalX
     then GameOver
     else Playing plants newTime sun allSuns updatedTimers movedMowers finalZombies nbullets
  where
    processCollisions ms zs =
      let -- Find lanes where any zombie collides with a lawnmower
          collidingLanes = [lawnLane m | (m, i) <- zip ms [0..], any (\z -> C.checkCollision z m && abs (posLane (zombiePos z) - lawnLane m) < 0.1) zs, not (isActive m)]
          -- Activate mowers in colliding lanes
          updatedMowers = foldl' (\ms' i -> activateMower i ms') ms [i | (m, i) <- zip ms [0..], lawnLane m `elem` collidingLanes]
          -- Kill all zombies in colliding lanes
          updatedZombies = map (\z -> if posLane (zombiePos z) `elem` collidingLanes
                                     then Z.hitZombie z (zombieHealth z)
                                     else z) zs
      in (updatedMowers, Z.clearDead updatedZombies)

    isColliding z m = C.checkCollision z m && abs (posLane (zombiePos z) - lawnLane m) < 0.1

updateGame dt (SelectingPlant plants t plantType sun suns sunTimers mowers zombies bullets) =
  let newTime = t + dt
      -- Update zombies before collision
      updatedZombies = Z.updateAllZ zombies newTime
      -- Process collisions, killing all colliding zombies in the same lane
      (newMowers, collidedZombies) = processCollisions mowers updatedZombies
      finalZombies = Z.clearDead collidedZombies
      movedMowers = updateMowers dt newMowers
      -- Update suns and sun timers
      sunflowerPlants = [p | p@(Plant Sunflower (x, y) _) <- plants]
      plantsWithTimers = [(p, lastSunTime p) | p <- sunflowerPlants]
      generatedSuns = generateSun plantsWithTimers newTime suns
      allSuns = updateSuns dt (suns ++ generatedSuns)
      lastSunTime (Plant Sunflower (x,y) _) =
        case lookup (x, y) sunTimers of
          Just tm -> tm
          Nothing -> -1000
      updatedTimers = foldl' updateTimer sunTimers
        [ (x,y) | (Plant Sunflower (x,y) _, lastT) <- plantsWithTimers
                , newTime - lastT >= 10 ]
      updateTimer acc pos = (pos, newTime) : filter ((/= pos) . fst) acc
  in if Z.checkFinish finalZombies criticalX
     then GameOver
     else SelectingPlant plants newTime plantType sun allSuns updatedTimers movedMowers finalZombies bullets
  where
    processCollisions ms zs =
      let -- Find lanes where any zombie collides with a lawnmower
          collidingLanes = [lawnLane m | (m, i) <- zip ms [0..], any (\z -> C.checkCollision z m && abs (posLane (zombiePos z) - lawnLane m) < 0.1) zs, not (isActive m)]
          -- Activate mowers in colliding lanes
          updatedMowers = foldl' (\ms' i -> activateMower i ms') ms [i | (m, i) <- zip ms [0..], lawnLane m `elem` collidingLanes]
          -- Kill all zombies in colliding lanes
          updatedZombies = map (\z -> if posLane (zombiePos z) `elem` collidingLanes
                                     then Z.hitZombie z (zombieHealth z)
                                     else z) zs
      in (updatedMowers, Z.clearDead updatedZombies)

    isColliding z m = C.checkCollision z m && abs (posLane (zombiePos z) - lawnLane m) < 0.1

updateGame _ GameOver = GameOver