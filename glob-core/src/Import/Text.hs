




-- src/Import/Text.hs

module Import.Text
    ( module X
    , lazyDecodeUtf8
    , toStrictText
    , writeFileT
    , readFileT
    ) where
      import Data.Text as X
      import Data.Text.Encoding as X
      import Data.Text.Lazy as TL
      import Data.Text.Lazy.Encoding as TLE
      import Data.Text.IO as TI

      writeFileT = TI.writeFile
      readFileT = TI.readFile

      toStrictText = TL.toStrict
      lazyDecodeUtf8 = TLE.decodeUtf8
