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
import GameTypes (Position(..), Zombie(..), Coloring(..))
import LawnMower (LawnMower(..), initialLawnMowers, renderLawnMower, updateMowers, activateMowers)
import qualified Collision as C
import Data.List (partition)

-- Константы игры
criticalX :: Float
criticalX = -450
lawnMowerSpeed :: Float
lawnMowerSpeed = 300
lawnMowerStartX :: Float
lawnMowerStartX = -350

baseZombies :: [Zombie]
baseZombies = [
    Zombie (Position 333 0 50 (333, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Zombie (Position 266 1 50 (266, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Zombie (Position 200 2 50 (200, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Zombie (Position 266 3 50 (266, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Zombie (Position 333 4 50 (333, 0) (30, 30)) 10 (Coloring 1 1 1 1)
  ]

main :: IO ()
main = do
    map <- generateMap
    play (InWindow "PvZ" (800, 600) (50, 50)) black 60
        (Playing [] 0 500 initialLawnMowers)
        (\gs -> Pictures [map, renderGameState gs])
        handleEvent
        updateGame

-- Рендер игры
renderGameState :: GameState -> Picture
renderGameState gs = Pictures allPictures
  where
    currentTime = case gs of
              Playing _ t _ _ -> t
              SelectingPlant _ t _ _ _ -> t
              GameOver -> 0

    plants = case gs of
              Playing ps _ _ _ -> ps
              SelectingPlant ps _ _ _ _ -> ps
              GameOver -> []

    sunResults = map (\p -> generateSun p currentTime gs) plants
    sunPics = map fst sunResults
    totalSunGenerated = sum (map snd sunResults)

    currentSun = case gs of
          Playing _ _ sun _ -> sun + totalSunGenerated
          SelectingPlant _ _ _ sun _ -> sun + totalSunGenerated
          GameOver -> 0

    zombies = updateAllZ baseZombies currentTime lawnMowers
    picZ = animateAllZ zombies currentTime

    plantPics = map generatePlant plants
    bulletPics = map (\p -> generateBullet p currentTime gs) plants

    cards = renderPlantCards currentSun availableCards

    sunDisplay = Translate 300 250 $ Pictures
      [ Color yellow $ circleSolid 20
      , Color yellow $ Translate 30 (-7) $ Scale 0.3 0.3 $ Text (show currentSun)
      ]

    lawnMowers = case gs of
              Playing _ _ _ mowers -> mowers
              SelectingPlant _ _ _ _ mowers -> mowers
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

-- Обработка событий
handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state =
    case state of
        Playing plants t sun mowers
            | x > 250 && x < 350 && y > 220 && y < 280 ->
                let sunResults = map (\p -> generateSun p t state) plants
                    collectedSun = sum (map snd sunResults)
                in Playing plants t (sun + collectedSun) mowers
            | y > 200 && y < 350 ->
                let idx = floor ((x + 350) / 120)
                in if idx >= 0 && idx < length availableCards
                   then let card = availableCards !! idx
                        in if sun >= cost card
                           then SelectingPlant plants t (cardType card) sun mowers
                           else Playing plants t sun mowers
                   else Playing plants t sun mowers
            | otherwise -> Playing plants t sun mowers

        SelectingPlant plants t plantType sun mowers
            | y < 200 ->
                let newPlant = Plant plantType (x, y) 100
                    card = head $ filter (\c -> cardType c == plantType) availableCards
                    newSun = sun - cost card
                in Playing (newPlant : plants) t newSun mowers
            | otherwise -> Playing plants t sun mowers

        _ -> state
handleEvent _ state = state

-- Обновление игры
updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t sun mowers) =
    let newTime = t + dt
        updatedZombies = map (`updateZombie` dt) baseZombies
        (activeMowers, zombiesAfterCollision) = processCollisions mowers updatedZombies
        movedMowers = updateMowers dt activeMowers
        gameEnded = any (hasReachedCriticalPoint criticalX) zombiesAfterCollision
    in if gameEnded
       then GameOver
       else Playing plants newTime sun movedMowers
    where
        processCollisions :: [LawnMower] -> [Zombie] -> ([LawnMower], [Zombie])
        processCollisions mowers zombies =
            foldl processSingleMower (mowers, zombies) mowers

        processSingleMower :: ([LawnMower], [Zombie]) -> LawnMower -> ([LawnMower], [Zombie])
        processSingleMower (ms, zs) mower
            | isActive mower = (ms, zs)
            | otherwise =
                let (zombiesOnSameLane, otherZombies) = partition (\z -> posLane (zombiePos z) == lawnLane mower) zs
                    collidingZombies = filter (\z -> C.checkCollision z mower) zombiesOnSameLane
                in if not (null collidingZombies)
                   then (activateMower mower ms, otherZombies)
                   else (ms, zs)

        activateMower :: LawnMower -> [LawnMower] -> [LawnMower]
        activateMower mower mowers =
            let newMower = mower { isActive = True, lawnSpeed = lawnMowerSpeed }
            in newMower : filter (\m -> lawnLane m /= lawnLane mower) mowers

        hasReachedCriticalPoint :: Float -> Zombie -> Bool
        hasReachedCriticalPoint edge (Zombie pos _ _) =
            let (x, _) = posCoord pos
            in x <= edge

updateGame dt (SelectingPlant plants t plantType sun mowers) =
    let newTime = t + dt
        updatedZombies = map (`updateZombie` dt) baseZombies
        (activeMowers, remainingZombies) = processCollisions mowers updatedZombies
        movedMowers = updateMowers dt activeMowers
        gameEnded = any (hasReachedCriticalPoint criticalX) remainingZombies
    in if gameEnded
       then GameOver
       else SelectingPlant plants newTime plantType sun movedMowers
    where
        processCollisions :: [LawnMower] -> [Zombie] -> ([LawnMower], [Zombie])
        processCollisions mowers zombies =
            foldl processSingleMower (mowers, zombies) mowers

        processSingleMower :: ([LawnMower], [Zombie]) -> LawnMower -> ([LawnMower], [Zombie])
        processSingleMower (ms, zs) mower
            | isActive mower = (ms, zs)
            | otherwise =
                let (zombiesOnSameLane, otherZombies) = partition (\z -> posLane (zombiePos z) == lawnLane mower) zs
                    collidingZombies = filter (\z -> C.checkCollision z mower) zombiesOnSameLane
                in if not (null collidingZombies)
                   then (activateMower mower ms, otherZombies)
                   else (ms, zs)

        activateMower :: LawnMower -> [LawnMower] -> [LawnMower]
        activateMower mower mowers =
            let newMower = mower { isActive = True, lawnSpeed = lawnMowerSpeed }
            in newMower : filter (\m -> lawnLane m /= lawnLane mower) mowers

        hasReachedCriticalPoint :: Float -> Zombie -> Bool
        hasReachedCriticalPoint edge (Zombie pos _ _) =
            let (x, _) = posCoord pos
            in x <= edge

updateGame _ GameOver = GameOver