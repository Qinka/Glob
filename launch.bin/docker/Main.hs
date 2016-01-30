




-- launch.bin/docker/Main.hs

{-# LANGUAGE OverloadedStrings #-}


module Main
    ( main
    ) where

      import qualified Glob.Config  as G
      import Data.Aeson
      import System.IO
      import System.Exit
      import System.Environment
      import System.Process
      import System.Directory
      import Data.ByteString.Internal
      import Data.Maybe
      import Main.CmdArgs

      main :: IO ()
      main = runArgs toConfig >>= main'
        where
          main' :: G.Config -> IO()
          main' c = do
            (hIn,_,_,hProc) <- createProcess (shell $ "glob" ++ exeExtension ++ " --file=stdin") {std_in=CreatePipe}
            hPutStrLn (fromMaybe stdout hIn) $ read $ show $ encode c
            hClose (fromMaybe stdout hIn)
            print =<< waitForProcess hProc

      toConfig :: DockerLaunch -> IO G.Config
      toConfig x = do
        port <- getEnv $ port x
        conThd <- getEnv $ conThd x
        favPath <- getEnv $ favPath x
        siteTitle <- getEnv $ siteTitle x
        certPath <- getEnv $ certPath x
        keyPath <- getEnv $ keyPath x
        dbAddr <- getEnv $ dbAddr x
        dbPort <- getEnv $ dbPort x
        dbName <- getEnv $ dbName x
        dbPsk <- getEnv $ dbPsk x
        dbUsr <- getEnv $ dbUsr x
        return $ G.Config
          (read port)
          (G.DbConfig dbAddr dbPort dbUsr dbPsk dbName $ read conThd)
          favPath
          siteTitle
          certPath
          keyPath
