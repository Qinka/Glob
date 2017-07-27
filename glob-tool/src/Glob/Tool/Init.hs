{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.Init
  ( initHandler
  ) where

import           Glob.Import
import           Glob.Tool.Opt
import           Glob.Tool.Repo
import           System.IO
import           System.IO.Echo

initHandler :: Glob -> IO ()
initHandler Init{..} = do
  createDirectory globRepoName
  url   <- case initSiteUrl of
    Just u -> return u
    Nothing -> do
      hPutStrLn stderr "warrning: site url unset"
      return ""
  token <- case initTokenFile of
    Just fp -> return fp
    Nothing -> do
      hPutStrLn stderr "warrning: token file unset"
      return ""
  writeFile (globRepoName ++ "/url.glob-ignore")   url
  writeFile (globRepoName ++ "/token.glob-ignore") token


