




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
      globCMdArgs :: GlobCmdArgs
      globCMdArgs = GlobCmdArgs
        { fromFile = "stdin"
          &= name "file"
          &= explicit
          &= typFile
          &= help "Input the config-file from file."
        }
        &= program "glob"
        &= summary ("glob "++showVersion version++",(C) Qinka 2016")

      runArgs :: (GlobCmdArgs -> IO (Maybe Config)) -> IO (Maybe Config)
      runArgs = (cmdArgs globCMdArgs >>=)
