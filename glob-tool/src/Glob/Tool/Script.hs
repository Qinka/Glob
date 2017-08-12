{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.Script
  ( scriptHandler
  ) where

import           Glob.Tool.Opt
import           System.IO

scriptHandler :: Glob -> IO ()
scriptHandler Script{..} = do
  case sptKind of
    Nothing            -> hPutStrLn stderr "help"
    Just "cfg-upgrade" -> putStrLn cfgUpgrade


cfgUpgrade :: String
cfgUpgrade =  "sed 's/\"html\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"binary\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"text\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"url\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"var\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"create-time\"/\"cretime\"/g'"

