




-- bin.src/Main/CmdArgs.hs
{-# LANGUAGE DeriveDataTypeable #-}

module Main.CmdArgs
    ( GlobCmdArgs(..)
    , globCMdArgs
    , runArgs
    ) where

      import System.Console.CmdArgs
      import Glob.Config
      import Paths_Glob
      import Data.Version(showVersion)

      data GlobCmdArgs = GlobCmdArgs
        { fromFile  :: String
        } deriving (Show,Typeable,Data)
      globCMdArgs = GlobCmdArgs
        { fromFile = "stdin"
          &= opt "stdin"
          &= typFile
          &= help "Input the config-file from file."
          &= name "file"
        }
        &= program "glob"
        &= summary ("glob "++showVersion version++",(C) Qinka 2015")

      runArgs :: (GlobCmdArgs -> IO (Maybe Config)) -> IO (Maybe Config)
      runArgs = (cmdArgs globCMdArgs >>=)
