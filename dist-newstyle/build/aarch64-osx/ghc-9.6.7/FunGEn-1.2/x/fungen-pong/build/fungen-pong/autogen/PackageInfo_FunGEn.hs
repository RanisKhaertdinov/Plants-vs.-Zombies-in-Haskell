{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_FunGEn (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "FunGEn"
version :: Version
version = Version [1,2] []

synopsis :: String
synopsis = "A lightweight, cross-platform, OpenGL-based game engine."
copyright :: String
copyright = "(C) 2002 Andre Furtado <awbf@cin.ufpe.br>"
homepage :: String
homepage = "https://github.com/haskell-game/fungen"
