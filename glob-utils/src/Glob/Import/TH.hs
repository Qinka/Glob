{-|
Module:       Glob.Import.TH
Description:  The reexport of the TemplatHaskell
Copyright:    (C) Qinka 2017
License:      GPL-3
Maintainer:   me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of the template haskell
-}

module Glob.Import.TH
       ( -- * the reexported module
         module Language.Haskell.TH
       , module Language.Haskell.TH.Quote
       , module Language.Haskell.TH.Syntax
       ) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

