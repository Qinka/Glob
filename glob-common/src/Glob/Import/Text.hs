{-|
Module:      Glob.Import.Text
Description: The reexport of text
Copyright:   (C) Qinka 2017
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknows

The reexport of text
-}

module Glob.Import.Text
       ( -- * the reexported module
         module Data.Text
       , module Data.Text.Encoding
       , -- * the methods
         read
       , show
       ) where

import qualified Data.Text
import qualified Data.Text.Encoding
import           Prelude            ((.))
import qualified Prelude            as P

-- | the read method for Text
read :: P.Read a
        => Data.Text.Text -- ^ the string
        -> a              -- ^ item
read = P.read . Data.Text.unpack

-- | the show method for Text
show :: P.Show a
        => a              -- ^ item
        -> Data.Text.Text -- ^ the string
show = Data.Text.pack . P.show
