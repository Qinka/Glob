
module Main where

import           Control.Monad.Random
import           Data.Char
import           Data.String
import           Glob.Auth.Core
import           System.Exit
import           System.Random

import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T

main = do
  putStrLn "Begin"
  putStrLn "Generating"
  str <- mkRandomStrMax 100
  (pub,pri) <- generate 1000 10001
  print (pri,str)
  rt <-  generateToken True str pri defaultPSSParamsSHA1
  checkToken rt str pub
  where
    checkToken (Left e) _ _ = do
      print e
      exitFailure
    checkToken (Right token) str pub = do
      case verifyToken True token str pub defaultPSSParamsSHA1 of
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
