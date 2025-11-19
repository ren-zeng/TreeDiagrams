{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_TreeDiagrams (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "TreeDiagrams"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "A minimal library for visualizing trees as SVGs."
copyright :: String
copyright = "2025 Zeng Ren"
homepage :: String
homepage = "https://github.com/ren-zeng/TreeDiagrams#readme"
