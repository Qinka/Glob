




-- src/Main.hs

{-# LANGUAGE CPP #-}

module Main
    ( main
    ) where

      import qualified Main.CmdArgs as C
      import qualified Glob.Foundation.Config as G
      import Glob.MDBS as G
      import Glob.Default
      import Data.Yaml
      import System.IO
      import System.Environment
      import Data.Maybe

      main :: IO ()
      main = C.runArgs toConfig >>= main'
        where
          main' :: G.GlobConfig -> IO()
#ifdef WithTls
          main' = defaultMainTlsWithConfig
#else
          main' = defaultMainWithConfig
#endif

      toConfig :: C.DockerLaunch -> IO G.GlobConfig
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
        return $ G.GlobConfig
          (read port)
          (G.DbConfig dbAddr dbPort dbUsr dbPsk dbName $ read conThd)
          siteTitle
          token
          staticPath
          keyPath
          certPath
