{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.Del
  (
  ) where

import           Glob.Tool.Opt
import           Glob.Tool.Repo
import           System.Directory
import           System.IO


delHandler :: Glob -> IO ()
delHandler Del{..} = do
  case delId of
    Just i -> removePathForcibly i
    _      -> hPutStrLn stderr "error: path required"
