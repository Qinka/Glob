{-|
Module:      Glob.Import.ByteString
Description: reexport of the bytestring
Copyright:   (C) Qinka 2017
License:     GPL3
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of the bytestring
-}

module Glob.Import.ByteString
       ( -- * the reexported module
         module Data.ByteString
       , -- * the methods
         pack
       , unpack
       , read
       , show
       ) where

import           Data.ByteString       hiding (pack, unpack)
import           Data.ByteString.Char8 (pack, unpack)
import           Prelude               ((.))
import qualified Prelude               as P

-- | The read method for bytestring
read :: P.Read a
        => Data.ByteString.ByteString  -- ^ string
        -> a                           -- ^ result
read = P.read . unpack

-- | The show method for bytestring
show :: P.Show a
        => a                           -- ^ item
        -> Data.ByteString.ByteString  -- ^ result
show = pack . P.show
