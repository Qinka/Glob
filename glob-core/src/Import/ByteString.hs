




-- src/Import/ByteString.hs

module Import.ByteString
    ( module X
    , fromStrictBS
    , toStrictBS
    ) where

      import Data.ByteString as X
      import Data.ByteString.Lazy

      fromStrictBS = fromStrict
      toStrictBS = toStrict
