




-- bin.src/Main.hs
{-# LANGUAGE RecordWildCards #-}

module Main
    ( main
    ) where

      import Yesod
      import Glob.Tls

      import System.IO

      import Glob
      import Glob.Config
      import Main.CmdArgs

      import Data.String(fromString)
      import Data.Maybe
      import Data.Aeson

      import Database.Persist.Postgresql

      import Control.Monad.Logger

      main :: IO ()
      main = do
        con <- runArgs withGlobCmdArgs
        let config = fromMaybe (error "glob:Error Config") con
        let (constr,conThd) = toConStr $ dbConfig config
        runStderrLoggingT $ withPostgresqlPool constr conThd $
          \pool -> liftIO $
            warpTls
              (certPath config)
              (keyPath config)
              (port config)
              (Glob pool config (Management pool config))

      withGlobCmdArgs ::  GlobCmdArgs -> IO (Maybe Config)
      withGlobCmdArgs GlobCmdArgs{..} = do
        text <- hGetContents =<< if fromFile == "stdin"
          then return stdin
          else openFile fromFile ReadMode
        return $ decode $ fromString text
