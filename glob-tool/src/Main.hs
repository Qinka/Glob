
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where


import System.IO
import qualified Glob.Import.ByteString as B
import Glob.Tool.Opt
import Glob.Tool.Ih

main :: IO ()
main = do
  it <- cmdArgs glob
  case it of
    ih_@Ih{..} -> do
    new_@New{..} -> do
