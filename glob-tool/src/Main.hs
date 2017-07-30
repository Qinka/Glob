
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where


import qualified Glob.Import.ByteString as B
import           Glob.Tool.Ih
import           Glob.Tool.Init
import           Glob.Tool.Opt
import           System.IO

main :: IO ()
main = do
  it <- cmdArgs glob
  case it of
    Ih{..}   -> ihHandler   it
    Init{..} -> initHandler it
    New{..}  -> newHandler  it
    Del{..}  -> delHandler  it
