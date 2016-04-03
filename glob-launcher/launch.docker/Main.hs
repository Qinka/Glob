




-- src/Main.hs

{-# LANGUAGE CPP
           , TemplateHaskell
           , QuasiQuotes
           , DeriveDataTypeable
           #-}

module Main
    ( main
    ) where

      import Main.CmdArgs
      import System.Console.CmdArgs
      import Data.Version
      import Paths_glob_launcher
      import Glob.Foundation.Config
      import Glob.MDBS (dbConfigMk)
      import Glob.Default
      import Data.Yaml
      import System.IO
      import System.Environment
      import Data.Maybe
      import MDBS
      import System.Console.CmdArgs.Quote

      dlD
      dl
      tocfgi

      main :: IO ()
      main = runArgs toConfig >>= main'
        where
          main' :: GlobConfig -> IO()
#ifdef WithTls
          main' = defaultMainTlsWithConfig
#else
          main' = defaultMainWithConfig
#endif
{-
      toConfig :: DockerLaunch -> IO GlobConfig
      toConfig x = do
        port <- getEnv $ C.port x
        conThd <- getEnv $ C.conThd x
        staticPath <- getEnv $ C.staticPath x
        siteTitle <- getEnv $ C.siteTitle x
#ifdef WithTls
        certPath <- getEnv $ C.certPath x
        keyPath <- getEnv $ C.keyPath x
#else
        let certPath = ""
        let keyPath = ""
#endif
        let token = C.tokenEnv x
        dbAddr <- getEnv $ C.dbAddr x
        dbPort <- getEnv $ C.dbPort x
        dbName <- getEnv $ C.dbName x
        dbPsk <- getEnv $ C.dbPsk x
        dbUsr <- getEnv $ C.dbUsr x
        logpath <- getEnv $ C.logpath x
        return $ G.GlobConfig
          (read port)
          (G.DbConfig dbAddr dbPort dbUsr dbPsk dbName $ read conThd)
          siteTitle
          token
          staticPath
          keyPath
          certPath
          logpath

-}
      runArgs :: (DockerLaunch -> IO GlobConfig) -> IO GlobConfig
      runArgs = (cmdArgs dockerLaunch >>=)
