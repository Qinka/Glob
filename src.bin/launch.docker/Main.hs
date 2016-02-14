




-- src.bin/launch.docker/Main.hs

{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

      import qualified Main.CmdArgs as C
      import qualified Glob.Config  as G
      import Data.Aeson
      import System.IO
      import System.Environment
      import System.Process
      import System.Directory
      import Data.Maybe

      main :: IO ()
      main = C.runArgs toConfig >>= main'
        where
          main' :: G.Config -> IO()
          main' c = do
            (hIn,_,_,hProc) <- createProcess (shell $ "glob" ++ exeExtension ++ " --file=stdin") {std_in=CreatePipe}
            hPutStrLn (fromMaybe stdout hIn) $ read $ show $ encode c
            hClose (fromMaybe stdout hIn)
            print =<< waitForProcess hProc

      toConfig :: C.DockerLaunch -> IO G.Config
      toConfig x = do
        port <- getEnv $ C.port x
        conThd <- getEnv $ C.conThd x
        favPath <- getEnv $ C.favPath x
        siteTitle <- getEnv $ C.siteTitle x
        certPath <- getEnv $ C.certPath x
        keyPath <- getEnv $ C.keyPath x
        let token = C.tokenEnv x
        dbAddr <- getEnv $ C.dbAddr x
        dbPort <- getEnv $ C.dbPort x
        dbName <- getEnv $ C.dbName x
        dbPsk <- getEnv $ C.dbPsk x
        dbUsr <- getEnv $ C.dbUsr x
        return $ G.Config
          (read port)
          (G.DbConfig dbAddr dbPort dbUsr dbPsk dbName $ read conThd)
          favPath
          siteTitle
          certPath
          keyPath
          token
