module GameTypes
    ( Position(..)
    , Zombie(..)
    , Coloring(..)
    , LawnMower(..)
    ) where

type Lane = Float
type Health = Int
type Start = Float
type Speed = Float
type Coord = (Float, Float)
type HitboxSize = (Float, Float)

data Coloring = Coloring Float Float Float Float

instance Eq Coloring where
    (Coloring r1 g1 b1 a1) == (Coloring r2 g2 b2 a2) =
        r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2

instance Show Coloring where
    show (Coloring r g b a) =
        "Coloring " ++ show r ++ " " ++ show g ++ " " ++ show b ++ " " ++ show a

data Position = Position
    { posStart :: Start
    , posLane :: Lane
    , posSpeed :: Speed
    , posCoord :: Coord
    , posHitbox :: HitboxSize
    } deriving (Eq, Show)

data LawnMower = LawnMower
    { lawnLane :: Lane
    , lawnPos :: Float
    , isActive :: Bool
    , lawnSpeed :: Float
    } deriving (Eq, Show)

data Zombie = Zombie
    { zombiePos :: Position
    , zombieHealth :: Health
    , zombieColoring :: Coloring
    } deriving (Eq, Show)
