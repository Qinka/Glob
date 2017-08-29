{-# LANGUAGE RecordWildCards #-}

module Main
       ( main
       ) where

import           Control.Applicative         ((<|>))
import           Data.Maybe
import qualified Data.Yaml                   as Y
import qualified Glob.Import.Aeson           as A
import qualified Glob.Import.ByteString      as B
import qualified Glob.Import.ByteString.Lazy as BL
import qualified Glob.Import.Text            as T
import           Glob.Launch
import           System.Console.CmdArgs
import           System.IO


main :: IO ()
main = do
  rain <- createRain =<< parseCfgFile <$> getContents
  case rain of
    Just r@Rain{..} -> warp rainPort r
    _               -> hPutStrLn stderr "can not parse"
  return ()


parseCfgFileJSON :: String -> Maybe [RainConfig]
parseCfgFileJSON str = A.decode $ BL.pack str
parseCfgFileYAML :: String -> Maybe [RainConfig]
parseCfgFileYAML str = Y.decode $  B.pack str
parseCfgFile :: String ->  [RainConfig]
parseCfgFile str =  fromMaybe [] $ parseCfgFileYAML str <|> parseCfgFileJSON str
