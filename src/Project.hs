{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld

-- | Sample picture.
myPicture :: Picture
myPicture = colored blue (lettering "Hello, world!")

-- | Default entry point.
run :: IO ()
run = drawingOf myPicture
