{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
module Main
       ( main
       ) where

import           Control.Applicative         ((<|>))
import           Data.Maybe
import qualified Data.Yaml                   as Y
import qualified GHC.IO.Encoding             as E
import qualified Glob.Import.Aeson           as A
import qualified Glob.Import.ByteString      as B
import qualified Glob.Import.ByteString.Lazy as BL
import qualified Glob.Import.Text            as T
import           Glob.Launch
import           System.Console.CmdArgs
import           System.IO


main :: IO ()
main = do
#ifdef WithUTF8
  E.setLocaleEncoding E.utf8
  hSetEncoding stdout utf8
#endif
  rain <- createRain =<< parseCfgFile <$> getContents
  case rain of
    Just r@Rain{..} -> warp rainPort r
  return ()


parseCfgFileJSON :: String -> Maybe [RainConfig]
parseCfgFileJSON str = A.decode $ BL.pack str
parseCfgFileYAML :: String -> Maybe [RainConfig]
parseCfgFileYAML str = Y.decode $  B.pack str
parseCfgFile :: String ->  [RainConfig]
parseCfgFile str =  fromMaybe [] $ parseCfgFileYAML str <|> parseCfgFileJSON str
