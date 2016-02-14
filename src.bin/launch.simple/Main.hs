




-- src.bin/launch.simple/Main.hs

{-# LANGUAGE OverloadedStrings #-}


module Main
    ( main
    ) where

      import qualified Main.CmdArgs as C
      import qualified Glob.Config  as G
      import Data.Aeson
      import System.IO
      import Data.Maybe
      import System.Console.CmdArgs

      main :: IO ()
      main = do
        l <- cmdArgs C.launch
        let c = G.Config (read $ fromMaybe "0" $ C.port l) (G.DbConfig (fromMaybe "" $ C.dbAddr l) (fromMaybe "" $ C.dbPort l) (fromMaybe "" $ C.dbUsr l) (fromMaybe "" $ C.dbPsk l) (fromMaybe "" $ C.dbName l) (read $ fromMaybe "0" $ C.conThd l)) (fromMaybe "" $ C.favPath l) (fromMaybe "" $ C.siteTitle l) (fromMaybe "" $ C.certPath l) (fromMaybe "" $ C.keyPath l) (fromMaybe "" $ C.tokenEnv l)
        if C.output l == "stdout"
          then
            putStrLn $ read $ show $ encode c
          else do
            h <- openFile (C.output l) WriteMode
            hPutStrLn h $ read $ show $ encode c
