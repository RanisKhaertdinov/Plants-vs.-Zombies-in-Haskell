module GameTypes
    ( Lane, Health, Start, Speed, Coord, HitboxSize
    , Coloring(..)
    , Position(..)
    , Zombie(..)
    , LawnMower(..)
    ) where

type Lane = Float
type Health = Int
type Start = Float
type Speed = Float
type Coord = (Float, Float)
type HitboxSize = (Float, Float)

data Coloring = Coloring Float Float Float Float

data Position = Position
    { posStart :: Start
    , posLane :: Lane
    , posSpeed :: Speed
    , posCoord :: Coord
    , posHitbox :: HitboxSize
    }

data Zombie = Zombie
    { zombiePos :: Position
    , zombieHealth :: Health
    , zombieColoring :: Coloring
    }

data LawnMower = LawnMower
    { lawnLane :: Lane
    , lawnPos :: Float
    , isActive :: Bool
    , lawnSpeed :: Float
    } deriving (Show)