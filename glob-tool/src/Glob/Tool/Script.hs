{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.Script
  ( scriptHandler
  ) where

import           Glob.Tool.Opt
import           System.FilePath
import           System.IO

scriptHandler :: Glob -> IO ()
scriptHandler Script{..} = do
  case sptKind of
    Just "cfg-upgrade" -> do
      ls <- lines <$> getContents
      mapM_ (putStrLn . cfgUpgrade) ls
    Just "cfg-rename" -> do
      ls <- lines <$> getContents
      mapM_ (putStrLn . cfgRename) ls
    Just "make-all" -> putStrLn makeUpdate
    _                  -> hPutStrLn stderr "help"



cfgUpgrade :: String -> String
cfgUpgrade filepath  =  "cat " ++ filepath
  ++ " | " ++ "sed 's/\"html\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"binary\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"text\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"url\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"var\"/\"content\"/g'"
  ++ " | " ++ "sed 's/\"create-time\"/\"cretime\"/g'"
  ++ " > " ++ filepath


cfgRename :: String -> String
cfgRename file =
  let (name,ext) = splitExtension file
  in name++ext++" "++name++".item"++ext

makeUpdate :: String
makeUpdate = "ls -1 .glob/*.item.json | awk -F/ '{print $2}' | sed  's/.item.json//g' | awk '{print \"make \"$1\" SITE_URL=$SITE_URL SITE_TOKEN=`glob ih -h sha256 -t $SITE_TOKEN ` \"}'  | sh"
