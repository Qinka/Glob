{-|
Module:       Glob.Import.ByteString.Lazy
Description:  The reexport of the Data.ByteString.Lazy
Copyright:    (C) 2017 Qinka
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The reexport of the Data.ByteString.Lazy
-}

module Glob.Import.ByteString.Lazy
       ( -- * the reexport modules
         module Data.ByteString.Lazy
       , module Data.ByteString.Lazy.Char8
       , -- * the methods
         read
       , show
       ) where

import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8
import           Prelude                    ((.))
import qualified Prelude

-- | the read method for Data.ByteString.Lazy.ByteString
read :: Prelude.Read a
        => Data.ByteString.Lazy.ByteString -- ^ string
        -> a                               -- ^ item
read = Prelude.read . Data.ByteString.Lazy.Char8.unpack


-- | the show method for Data.ByteString.Lazy.ByteString
show :: Prelude.Show a
        => a                               -- ^ item
        -> Data.ByteString.Lazy.ByteString -- ^ string
show = Data.ByteString.Lazy.Char8.pack . Prelude.show


