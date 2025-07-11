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


## Notes
- This is a simplified educational project and does not implement all features of the original PvZ game.
- The game uses a wallet system where you start with 500 sun and can collect more by clicking in the sun area.
- Contributions and suggestions are welcome!

## License
See [LICENSE](LICENSE).
