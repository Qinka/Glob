
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where


import           Control.Exception
import           Control.Monad
import           Data.Either
import qualified Glob.Import.ByteString as B
import           Glob.Tool.Del
import           Glob.Tool.Ih
import           Glob.Tool.Init
import           Glob.Tool.Make
import           Glob.Tool.Nav
import           Glob.Tool.New
import           Glob.Tool.Opt
import           Glob.Tool.Script
import           System.Console.CmdArgs
import           System.Environment
import           System.Exit
import           System.IO
import           System.Process


main :: IO ()
main = do
  args' <- getArgs
  rt <- try $ cmdArgs glob :: IO (Either SomeException Glob)
  case rt of
    Left e   -> otherProg e args'
    Right it -> matchArgs it
    -- callProcess fc fo
  where matchArgs it =
          case it of
            Ih{..}     -> ihHandler     it
            Init{..}   -> initHandler   it
            New{..}    -> newHandler    it
            Del{..}    -> delHandler    it
            Make{..}   -> makeHandler   it
            Nav{..}    -> navHandler    it
            Script{..} -> scriptHandler it
        exitSucc
        otherProg ExitSuccess _ = return ()
        otherProg e1 (fc:fo) = do
          rt <- try $ callProcess ("glob-" ++ fc) fo :: IO (Either ExitCode ())
          when (isLeft rt) $ do
            let Left e2 = rt
            print e1
            print e2



