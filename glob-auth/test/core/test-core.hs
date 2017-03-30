
module Main where

import Data.String
import Data.Char
import System.Random
import Control.Monad.Random
import System.Exit

import Glob.Auth.Core

import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text as T

main = do
  putStrLn "Begin"
  putStrLn "Generating"
  str <- mkRandomStrMax 100 
  (pub,pri) <- generate 1000 10001
  print (pri,pub,str)
  rt <-  generateToken True str pri
  checkToken rt pub str
  where
    checkToken (Left e) _ _ = do
      print e
      exitFailure
    checkToken (Right token) pub str = do
      let token' = T.pack token
      let str' = T.pack str
      case verifyToken True token' str' pub of
        Left e -> do
          print e
          exitFailure
        Right False -> do
          print False
          exitFailure
        Right True -> do
          print True
          exitSuccess
  

mkRandomStr :: IsString a => Int -> IO a
mkRandomStr len = do
  str <- take len <$> getRandoms
  return . fromString $ (chr . (`mod` 128)) <$> str

mkRandomStrMax :: IsString a => Int -> IO a
mkRandomStrMax limit = do
  len <- (`mod` limit) <$> getRandom
  str <- take len <$> getRandoms
  return . fromString $ (chr . (`mod` 128)) <$> str
