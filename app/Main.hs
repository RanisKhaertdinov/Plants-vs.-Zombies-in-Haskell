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
import Data.List (foldl', find, findIndex, lookup)
import Data.Maybe (listToMaybe)
import GameTypes (Position(..), Zombie(..), Bullet(..), Coloring(..), posLane, zombiePos)
import qualified Zombie as Z
import System.Random (mkStdGen, randomRs)

-- Константы игры
criticalX :: Float
criticalX = -350  -- Left edge of the field (house)

sunInterval :: Float
sunInterval = 3

debugMode :: Bool
debugMode = False

baseZombies :: [Z.Zombie]
baseZombies =
  [ Z.Zombie (Position x lane speed (x, gridY !! laneIdx) (30, 30)) hp (Coloring 1 1 1 1)
  | (x, laneIdx, speed, hp) <-
      [ (900, 0, 40, 10), (950, 1, 35, 10), (1000, 2, 45, 10), (1050, 3, 38, 10), (1100, 4, 42, 10)
      , (1200, 0, 40, 10), (1250, 1, 35, 10), (1300, 2, 45, 10), (1350, 3, 38, 10), (1400, 4, 42, 10)
      , (1600, 0, 45, 15), (1650, 1, 45, 15), (1700, 2, 45, 15), (1750, 3, 45, 15), (1800, 4, 45, 15)
      ]
  , let lane = fromIntegral laneIdx
  ]
wave2 = [ Z.Zombie (Position x lane speed (x, gridY !! laneIdx) (30, 30)) hp (Coloring 1 1 1 1)
  | (x, laneIdx, speed, hp) <-
      [ (900, 0, 40, 10), (950, 1, 35, 10), (1000, 2, 45, 10), (1050, 3, 38, 10), (1100, 4, 42, 10)
      , (1200, 0, 40, 10), (1250, 1, 35, 10), (1300, 2, 45, 10), (1350, 3, 38, 10), (1400, 4, 42, 10)
      , (1600, 0, 45, 15), (1650, 1, 45, 15), (1700, 2, 45, 15), (1750, 3, 45, 15), (1800, 4, 45, 15)
      ]
  , let lane = fromIntegral laneIdx
  ]

finalWave = 2
newWave w 
  | w < finalWave = wave2
  | otherwise = []

-- Grid definitions for plant placement
-- Constants for grid layout
numCols :: Int
numCols = 9

numRows :: Int
numRows = 5

colWidth :: Float
colWidth = 80  -- Distance between columns (approximate, based on gridX)

laneHeight :: Float
laneHeight = 66.6  -- Distance between rows (lanes)

gridX :: [Float]
gridX = [-420, -280, -200, -120, -40, 40, 120, 200, 280]  -- 9 columns

gridY :: [Float]
gridY = [fromIntegral i * laneHeight - (laneHeight * 2) | i <- [0..numRows-1]]  -- 5 rows (lanes)

snapToGrid :: (Float, Float) -> (Float, Float)
snapToGrid (x, y) =
  let closestX = head $ foldl' (\acc gx -> if abs (gx - x) < abs (head acc - x) then [gx] else acc) [head gridX] gridX
      closestY = head $ foldl' (\acc gy -> if abs (gy - y) < abs (head acc - y) then [gy] else acc) [head gridY] gridY
  in (closestX, closestY)

isCellOccupied :: [Plant] -> (Float, Float) -> Bool
isCellOccupied plants (x, y) =
  any (\(Plant _ (px, py) _) -> abs (px - x) < 1 && abs (py - y) < 1) plants

findPlantToBite :: Z.Zombie -> [Plant] -> Maybe Int
findPlantToBite (Z.Zombie (Position _ lane _ (zx, _) (w, _)) _ _) plants =
  let laneIdx = round lane
      isTouching (Plant _ (px, py) health) =
        health > 0.0 && abs (py - gridY !! laneIdx) < 1 && (zx - px) < (w/2 + 20) && (zx - px) > 0
  in findIndex isTouching plants

bitePlantsByZombies :: [Plant] -> [Z.Zombie] -> Float -> Float -> ([Plant], [Z.Zombie])
bitePlantsByZombies plants zombies dt newTime =
  let damagePerSecond = 20.0 in
  foldl' (\(ps, zs) z ->
    case findPlantToBite z ps of
      Just idx ->
        let (before, Plant t pos h:after) = splitAt idx ps
            newHealth = h - (damagePerSecond * dt)
            newPlant = Plant t pos (max 0.0 newHealth)
        in (before ++ [newPlant] ++ after, zs ++ [z]) -- зомби не двигается
      Nothing ->
        (ps, zs ++ [Z.updateZombieStep z dt]) -- зомби двигается
  ) (plants, []) zombies

main :: IO ()
main = do
    map <- generateMap
    play (InWindow "PvZ" (1000, 800) (50, 50)) black 60
        (Playing [] 0 Nothing 500 [] [] initialLawnMowers baseZombies [] 1)
        (\gs -> Pictures [map, renderGameState gs])
        handleEvent
        updateGame

extractCurrentTime :: GameState -> Float
extractCurrentTime (Playing _ t _ _ _ _ _ _ _ _) = t
extractCurrentTime _ = 0

extractPlants :: GameState -> [Plant]
extractPlants (Playing ps _ _ _ _ _ _ _ _ _) = ps
extractPlants _ = []

extractSuns :: GameState -> [Sun]
extractSuns (Playing _ _ _ _ suns _ _ _ _ _) = suns
extractSuns _ = []

extractCurrentSun :: GameState -> Int
extractCurrentSun (Playing _ _ _ sun _ _ _ _ _ _) = sun
extractCurrentSun _ = 0

extractZombies :: GameState -> [Zombie]
extractZombies (Playing _ _ _ _ _ _ _ zs _ _) = zs
extractZombies _ = []

extractBullets :: GameState -> [Bullet]
extractBullets (Playing _ _ _ _ _ _ _ _ bs _) = bs
extractBullets _ = []

extractLawnMowers :: GameState -> [LawnMower]
extractLawnMowers (Playing _ _ _ _ _ _ mowers _ _ _) = mowers
extractLawnMowers _ = []

renderGameObjects :: [Plant] -> [Bullet] -> [Sun] -> [Zombie] -> [LawnMower] -> Float -> [Picture]
renderGameObjects plants bullets suns zombies lawnmowers currentTime =
  let plantPics = map generatePlant plants
      bulletPics = B.animateAllB bullets
      sunPics = map renderSun suns
      zombiePics = Z.animateAllZ zombies
      lawnmowerPics = map (renderLawnMower currentTime) lawnmowers
  in plantPics ++ bulletPics ++ sunPics ++ zombiePics ++ lawnmowerPics


renderGameOverlay :: GameState -> [Picture]
renderGameOverlay GameOver = [gameOverText]
renderGameOverlay Win = [winText]
renderGameOverlay _ = []


renderGameState :: GameState -> Picture
renderGameState gs = Pictures $ allPictures
  where
    currentTime = extractCurrentTime gs
    plants = extractPlants gs
    suns = extractSuns gs
    currentSun = extractCurrentSun gs
    zombies = extractZombies gs
    bullets = extractBullets gs
    lawnMowers = extractLawnMowers gs
    cards = renderPlantCards currentSun availableCards

    sunDisplay = Translate 300 300 $ Pictures
      [ Color yellow $ circleSolid 20
      , Color yellow $ Translate 30 (-7) $ Scale 0.3 0.3 $ Text (show currentSun)
      ]
    
    overlay = renderGameOverlay gs
    objects = renderGameObjects plants bullets suns zombies lawnMowers currentTime
    allPictures = [sunDisplay, cards] ++ overlay ++ objects

    
gameOverText :: Picture
gameOverText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "Game Over!"
winText :: Picture
winText = Color red $ Translate 0 0 $ Scale 0.5 0.5 $ Text "You win!"

-- Helper for handling plant placement
-- Handles placing a plant on the grid if the cell is not occupied and the player has selected a plant type.
handlePlantPlacement :: Float -> Float -> GameState -> GameState
handlePlantPlacement x y (Playing plants t (Just plantType) sun suns sunTimers mowers zombies bullets wave) =
    let (gridX, gridY) = snapToGrid (x, y)
    in if isCellOccupied plants (gridX, gridY)
       then Playing plants t (Just plantType) sun suns sunTimers mowers zombies bullets wave  -- Cell occupied, stay in selection
       else let newPlant = Plant plantType (gridX, gridY) plantHealth
                plantHealth = case plantType of
                              Sunflower -> 100.0
                              Peashooter -> 100.0
                              WallNut -> 400.0
                card = head $ filter (\c -> cardType c == plantType) availableCards
                newSun = sun - cost card
                newSunTimers = if plantType == Sunflower
                               then ((gridX, gridY), t) : sunTimers
                               else sunTimers
            in Playing (newPlant : plants) t Nothing newSun (suns ++ generateSun [(newPlant, t)] t suns) newSunTimers mowers zombies bullets wave
handlePlantPlacement _ _ state = state

-- Helper for handling sun collection
-- Handles collecting suns when the player clicks on them.
handleSunCollection :: Float -> Float -> GameState -> GameState
handleSunCollection x y (Playing plants t mPlantType sun suns sunTimers mowers zombies bullets wave) =
    let clickedSuns = filter (\s -> isSunClicked s (x, y)) suns
        remainingSuns = filter (\s -> not (isSunClicked s (x, y))) suns
        collectedValue = sum (map value clickedSuns)
    in Playing plants t mPlantType (sun + collectedValue) remainingSuns sunTimers mowers zombies bullets wave
handleSunCollection _ _ state = state

-- Helper for handling card selection
-- Handles selecting a plant card if the player has enough sun.
handleCardSelection :: Float -> Float -> GameState -> GameState
handleCardSelection x y (Playing plants t mPlantType sun suns sunTimers mowers zombies bullets wave) =
    let idx = floor ((x + 350) / 120)
    in if idx >= 0 && idx < length availableCards
       then let card = availableCards !! idx
            in if sun >= cost card
               then Playing plants t (Just (cardType card)) sun suns sunTimers mowers zombies bullets wave
               else Playing plants t mPlantType sun suns sunTimers mowers zombies bullets wave
       else Playing plants t mPlantType sun suns sunTimers mowers zombies bullets wave
handleCardSelection _ _ state = state

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
    case state of
        Playing _ _ (Just _) _ _ _ _ _ _ _
            | y < 200 -> handlePlantPlacement x y state
            | y >= 200 -> case state of
                Playing plants t (Just plantType) sun suns sunTimers mowers zombies bullets wave ->
                    Playing plants t Nothing sun suns sunTimers mowers zombies bullets wave
                _ -> state
        Playing _ _ _ _ _ _ _ _ _ _
            | y < 200 -> handleSunCollection x y state
            | y > 200 && y < 350 -> handleCardSelection x y state
            | otherwise -> state
        _ -> state
handleEvent _ state = state

-- Helper for zombie actions (biting plants and moving)
-- Zombies either bite a plant in their lane or move forward if no plant is in range.
processZombieActions :: [Plant] -> [Z.Zombie] -> Float -> Float -> ([Plant], [Z.Zombie])
processZombieActions = bitePlantsByZombies

-- Helper for mower collisions
-- Checks for collisions between lawnmowers and zombies, activates mowers, and kills zombies in the same lane.
processMowerCollisions :: [LawnMower] -> [Z.Zombie] -> ([LawnMower], [Z.Zombie])
processMowerCollisions ms zs =
  let collidingLanes = [lawnLane m | (m, i) <- zip ms [0..], any (\z -> C.checkCollision z m && abs (posLane (zombiePos z) - lawnLane m) < 0.1) zs, not (isActive m)]
      updatedMowers = foldl' (\ms' i -> activateMower i ms') ms [i | (m, i) <- zip ms [0..], lawnLane m `elem` collidingLanes]
      updatedZombies = map (\z -> if (posLane (zombiePos z) `elem` collidingLanes) && (Z.onlawn z)
                                 then Z.hitZombie z (zombieHealth z)
                                 else z) zs
  in (updatedMowers, Z.clearDead updatedZombies)

-- Helper for bullet updates
-- Updates bullet positions, checks for collisions with zombies, conjures new bullets from Peashooters, and removes spent bullets.
processBulletUpdates :: [Z.Zombie] -> [Bullet] -> [Plant] -> Float -> Float -> ([Z.Zombie], [Bullet])
processBulletUpdates zombies bullets plants dt newTime =
  let updatedBullets = B.updateAllB bullets dt
      hitZombies = B.hitAllZAllB zombies updatedBullets
      conjuredBullets = B.conjureAll plants newTime updatedBullets
      exhaustedBullets = B.exhaustBullets conjuredBullets zombies
  in (hitZombies, exhaustedBullets)

-- Helper for sun generation
-- Generates new suns from Sunflowers at regular intervals and updates sun timers.
processSunGeneration :: [Plant] -> [(Float, Float, Float)] -> Float -> Float -> [Sun] -> ([Sun], [(Float, Float, Float)])
processSunGeneration alivePlants timers dt newTime suns =
  let sunflowerPlants = [p | p@(Plant Sunflower (x, y) _) <- alivePlants]
      plantsWithTimers = [(p, lastSunTime p) | p <- sunflowerPlants]
      generatedSuns = generateSun plantsWithTimers newTime suns
      allSuns = updateSuns dt (suns ++ generatedSuns)
      lastSunTime (Plant Sunflower (x,y) _) =
        case lookup (x, y) (map (\(a,b,c) -> ((a,b),c)) timers) of
          Just tm -> tm
          Nothing -> -1000
      updatedTimers = foldl' updateTimer timers
        [ (x,y,newTime) | (Plant Sunflower (x,y) _, lastT) <- plantsWithTimers
                        , newTime - lastT >= 10 ]
      updateTimer acc (x,y,nt) = (x,y,nt) : filter (\(x',y',_) -> (x',y') /= (x,y)) acc
  in (allSuns, updatedTimers)

-- Helper for wave management
-- Advances to the next wave if all zombies are cleared, or spawns new zombies for the next wave.
processWaveManagement :: [Z.Zombie] -> Int -> ([Z.Zombie], Int)
processWaveManagement zombies wave =
  let clearedZombies = Z.clearDead zombies
      nextWave = if null clearedZombies then wave + 1 else wave
      finalZombies = if null clearedZombies then newWave wave else clearedZombies
  in (finalZombies, nextWave)

-- Main game update function
-- Advances the game state by one frame: updates zombies, bullets, mowers, suns, and handles win/lose conditions.
updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t mPlantType sun suns sunTimers mowers zombies bullets wave) =
  let newTime = t + dt
      -- Zombies bite plants and move
      (plantsAfterBite, zombiesAfterBite) = processZombieActions plants zombies dt newTime
      alivePlants = filter (\(Plant _ _ h) -> h > 0.0) plantsAfterBite
      -- Mower collisions
      (activatedMowers, collidedZombies) = processMowerCollisions mowers zombiesAfterBite
      -- Bullet updates
      (zombiesAfterBullets, nbullets) = processBulletUpdates collidedZombies bullets alivePlants dt newTime
      -- Wave management
      (finalZombies, nwave) = processWaveManagement zombiesAfterBullets wave
      movedMowers = updateMowers dt activatedMowers
      -- Sun generation
      sunflowerTimers = [((x, y), lastSunTime (x, y)) | Plant Sunflower (x, y) _ <- alivePlants]
      lastSunTime (x, y) =
        case lookup (x, y) sunTimers of
          Just tm -> tm
          Nothing -> -1000
      (allSuns, updatedTimers) = processSunGeneration alivePlants (map (\((x,y),tm) -> (x,y,tm)) sunTimers) dt newTime suns
  in if Z.checkFinish finalZombies criticalX
     then GameOver
     else if isEmpty finalZombies
          then Win
          else Playing alivePlants newTime mPlantType sun allSuns (map (\(x,y,tm) -> ((x,y),tm)) updatedTimers) movedMowers finalZombies nbullets nwave
updateGame dt (GameOver) = GameOver
updateGame dt (Win) = Win

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False