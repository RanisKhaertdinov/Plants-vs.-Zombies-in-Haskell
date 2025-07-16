module GameTypes
    ( Position(..)
    , Zombie(..)
    , Coloring(..)
    , LawnMower(..)
    , Bullet (..)
    ) where

-- | Lane index (0-based, fractional for flexibility)
type Lane = Float
-- | Health value (integer)
type Health = Int
-- | Starting x-position for an entity
type Start = Float
-- | Movement speed (pixels per second)
type Speed = Float
-- | 2D coordinate (x, y)
type Coord = (Float, Float)
-- | Hitbox size (width, height)
type HitboxSize = (Float, Float)

-- | RGBA coloring for entities.
data Coloring = Coloring Float Float Float Float

instance Eq Coloring where
    (Coloring r1 g1 b1 a1) == (Coloring r2 g2 b2 a2) =
        r1 == r2 && g1 == g2 && b1 == b2 && a1 == a2

instance Show Coloring where
    show (Coloring r g b a) =
        "Coloring " ++ show r ++ " " ++ show g ++ " " ++ show b ++ " " ++ show a

-- | Position and hitbox for an entity.
data Position = Position
    { posStart :: Start         -- ^ Initial x-position
    , posLane :: Lane           -- ^ Lane index (row)
    , posSpeed :: Speed         -- ^ Movement speed
    , posCoord :: Coord         -- ^ Current (x, y) position
    , posHitbox :: HitboxSize   -- ^ Size of hitbox (width, height)
    } deriving (Eq, Show)

-- | Lawn mower state and position.
data LawnMower = LawnMower
    { lawnLane :: Lane      -- ^ Lane index
    , lawnPos :: Float      -- ^ Current x-position
    , isActive :: Bool      -- ^ Is the mower currently moving?
    , lawnSpeed :: Float    -- ^ Mower speed
    } deriving (Eq, Show)

-- | Zombie state, health, and color.
data Zombie = Zombie
    { zombiePos :: Position     -- ^ Position and hitbox
    , zombieHealth :: Health    -- ^ Current health
    , zombieColoring :: Coloring-- ^ Color for rendering
    } deriving (Eq, Show)

-- | Bullet state, damage, and color.
data Bullet = Bullet
    { bulletPos :: Position     -- ^ Position and hitbox
    , bulletDamage :: Health    -- ^ Damage dealt to zombies
    , bulletColoring :: Coloring-- ^ Color for rendering
    } deriving (Eq, Show)