{-|
Module:      Glob.Import.Aeson
Description: The reexport of aeson
Copyright:   (C) Qinka 2017
License:     GPL-3
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of the aeson
-}

module Glob.Import.Aeson
       ( -- * the  reexported module
         module Data.Aeson
       , module Data.Aeson.TH
       ) where

import           Data.Aeson
import           Data.Aeson.TH
