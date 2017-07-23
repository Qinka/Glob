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
         module BL
       , BLC8.pack
       , BLC8.unpack
       , -- * the methods
         read
       , show
       ) where

import           Data.ByteString.Lazy       as BL hiding (pack, unpack)
import           Data.ByteString.Lazy.Char8 as BLC8
import           Prelude                    ((.))
import qualified Prelude

-- | the read method for Data.ByteString.Lazy.ByteString
read :: Prelude.Read a
        => BL.ByteString -- ^ string
        -> a                               -- ^ item
read = Prelude.read . BLC8.unpack


-- | the show method for Data.ByteString.Lazy.ByteString
show :: Prelude.Show a
        => a                               -- ^ item
        -> BL.ByteString -- ^ string
show = BLC8.pack . Prelude.show


