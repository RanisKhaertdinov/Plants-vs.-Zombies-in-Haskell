 # Plants vs. Zombies in Haskell

A simple Plants vs. Zombies clone implemented in Haskell using the [Gloss](http://hackage.haskell.org/package/gloss) graphics library.

## Features
- Place different types of plants on the field
- Animated zombies moving towards your base
- Sunflowers generate visual sun animations
- Peashooters shoot bullets at zombies
- WallNuts block zombies
- Card-based plant selection system
- Sun collection system with wallet
- Game Over state when a zombie reaches the left edge

## Setup

### Prerequisites
- [Stack](https://docs.haskellstack.org/en/stable/README/) or [Cabal](https://www.haskell.org/cabal/)
- GHC (tested with 9.6.7)

### Installation
Clone the repository:
```sh
git clone <repo-url>
cd Plants-vs.-Zombies-in-Haskell
```

Install dependencies and build:
```sh
stack build
# or, with cabal:
cabal build
```

## Running the Game
With Stack:
```sh
stack run
```
With Cabal:
```sh
cabal run
```

## Controls
- **Left Mouse Button**
  - Click on a plant card (top of the screen) to select a plant type.
  - Then click on the field to place the selected plant at the clicked position.
  - Click in the sun collection area (top-right corner) to collect generated sun.
- **Game Over**: If a zombie reaches the left edge, the game ends and displays "Game Over!".

## Game Mechanics
- **Sun Collection**: Click in the sun display area (top-right) to collect sun and add it to your wallet
- **Plant Costs**: Each plant has a cost in sun (Sunflower: 50, Peashooter: 100, WallNut: 50)
- **Sun Generation**: Sunflowers generate sun every 3 seconds that can be collected
- **Zombie Defense**: Place plants strategically to prevent zombies from reaching the left edge

## Project Structure
- `app/Main.hs` — Main game loop, event handling, and rendering
- `app/Plant.hs` — Plant types, plant rendering
- `app/Zombie.hs` — Zombie types, movement, and game over logic
- `app/Bullet.hs` — Bullet logic and rendering
- `app/LittleSun.hs` — Sun generation and rendering
- `app/PlantCards.hs` — Plant card rendering and selection
- `app/GameStates.hs` — Game state definitions
- `app/GameMap.hs` — Map/background rendering
- `img/Frontyard.bmp` — Game background image

## Notes
- This is a simplified educational project and does not implement all features of the original PvZ game.
- The game uses a wallet system where you start with 500 sun and can collect more by clicking in the sun area.
- Contributions and suggestions are welcome!

## License
See [LICENSE](LICENSE).