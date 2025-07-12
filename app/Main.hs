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
import Data.List (partition)
import GameTypes (Position(..), Zombie(..), Coloring(..), posLane, zombiePos)
import qualified Zombie as Z
import Data.List (foldl', find, lookup)

-- Константы игры
criticalX :: Float
criticalX = -400

sunInterval :: Float
sunInterval = 3

baseZombies :: [Z.Zombie]
baseZombies = [
    Z.Zombie (Position 333 0 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 266 1 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 200 2 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 266 3 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1),
    Z.Zombie (Position 333 4 10 (0, 0) (30, 30)) 10 (Coloring 1 1 1 1)
  ]

main :: IO ()
main = do
    map <- generateMap
    play (InWindow "PvZ" (800, 600) (50, 50)) black 60
        (Playing [] 0 500 [] [] initialLawnMowers)
        (\gs -> Pictures [map, renderGameState gs])
        handleEvent
        updateGame

renderGameState :: GameState -> Picture
renderGameState gs = Pictures $ allPictures
  where
    currentTime = case gs of
      Playing _ t _ _ _ _ -> t
      SelectingPlant _ t _ _ _ _ _ -> t
      GameOver -> 0

    plants = case gs of
      Playing ps _ _ _ _ _ -> ps
      SelectingPlant ps _ _ _ _ _ _ -> ps
      GameOver -> []

    suns = case gs of
      Playing _ _ _ suns _ _ -> suns
      SelectingPlant _ _ _ _ suns _ _ -> suns
      GameOver -> []

    currentSun = case gs of
      Playing _ _ sun _ _ _ -> sun
      SelectingPlant _ _ _ sun _ _ _ -> sun
      GameOver -> 0

    zombies = Z.updateAllZ baseZombies currentTime
    picZ = Z.animateAllZ zombies

    plantPics = map generatePlant plants
    bulletPics = map (\p -> generateBullet p currentTime gs) plants
    sunPics = map renderSun suns
    cards = renderPlantCards currentSun availableCards

    sunDisplay = Translate 300 250 $ Pictures
      [ Color yellow $ circleSolid 20
      , Color yellow $ Translate 30 (-7) $ Scale 0.3 0.3 $ Text (show currentSun)
      ]

    lawnMowers = case gs of
      Playing _ _ _ _ _ mowers -> mowers
      SelectingPlant _ _ _ _ _ _ mowers -> mowers
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
        Playing plants t sun suns sunTimers mowers
            | y < 200 ->
                let clickedSuns = filter (\s -> isSunClicked s (x, y)) suns
                    remainingSuns = filter (\s -> not (isSunClicked s (x, y))) suns
                    collectedValue = sum (map value clickedSuns)
                in Playing plants t (sun + collectedValue) remainingSuns sunTimers mowers
            | y > 200 && y < 350 ->
                let idx = floor ((x + 350) / 120)
                in if idx >= 0 && idx < length availableCards
                   then let card = availableCards !! idx
                        in if sun >= cost card
                           then SelectingPlant plants t (cardType card) sun suns sunTimers mowers
                           else state
                   else state
            | otherwise -> state

        SelectingPlant plants t plantType sun suns sunTimers mowers
            | y < 200 ->
                let newPlant = Plant plantType (x, y) 100
                    card = head $ filter (\c -> cardType c == plantType) availableCards
                    newSun = sun - cost card
                in Playing (newPlant : plants) t newSun suns sunTimers mowers
            | otherwise -> Playing plants t sun suns sunTimers mowers

        _ -> state
handleEvent _ state = state

updateGame :: Float -> GameState -> GameState
updateGame dt (Playing plants t sun suns sunTimers mowers) =
  let newTime = t + dt
      zombies = Z.updateAllZ baseZombies newTime

      -- Находим первую неактивную косилку, с которой столкнулся зомби
      (activatedMowers, remainingZombies) =
        case findFirstCollision mowers zombies of
          Nothing -> (mowers, zombies)  -- Нет столкновений
          Just (mowerIdx, zombiesOnLane) ->
            let updatedMowers = activateSingleMower mowerIdx mowers
            in (updatedMowers, filter (\z -> posLane (zombiePos z) /= lawnLane (mowers !! mowerIdx)) zombies)

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

    in if Z.checkFinish remainingZombies criticalX
       then GameOver
       else Playing plants newTime sun allSuns updatedTimers movedMowers
    where
        findFirstCollision :: [LawnMower] -> [Zombie] -> Maybe (Int, [Zombie])
        findFirstCollision ms zs =
          listToMaybe [(i, filter (\z -> posLane (zombiePos z) == lawnLane m) zs)
                      | (m, i) <- zip ms [0..]
                      , not (isActive m)
                      , any (\z -> abs (fst (posCoord (zombiePos z)) - lawnPos m < 25) zs]

        -- Активирует одну косилку по индексу
        activateSingleMower :: Int -> [LawnMower] -> [LawnMower]
        activateSingleMower idx mowers =
          let (before, m:after) = splitAt idx mowers
          in before ++ [m { isActive = True, lawnSpeed = 800 }] ++ after

  SelectingPlant plants t plantType sun suns sunTimers mowers ->
    let newTime = t + dt
        zombies = Z.updateAllZ baseZombies newTime
        (newMowers, remainingZombies) = processCollisions mowers zombies
        movedMowers = updateMowers dt newMowers
    in if Z.checkFinish remainingZombies criticalX
       then GameOver
       else SelectingPlant plants newTime plantType sun suns sunTimers movedMowers
    where
      processCollisions ms zs =
        foldl' handleSingleZombie (ms, zs) zs
        where
          handleSingleZombie (mowers, zombies) z =
            case find (isColliding z) mowers of
              Just m ->
                ( activateSingle m mowers
                , filter (\z' -> zombiePos z' /= zombiePos z) zombies
                )
              Nothing -> (mowers, zombies)

          isColliding z m =
            not (isActive m) &&
            lawnLane m == posLane (zombiePos z) &&
            C.checkCollision z m

          activateSingle target mowers =
            map (\m -> if m == target
                      then m { isActive = True, lawnSpeed = 800 }
                      else m) mowers

  GameOver -> GameOver  -- Явная обработка состояния GameOver