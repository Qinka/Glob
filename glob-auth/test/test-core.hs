module Main where

import           Control.Monad.Random
import           Data.Char
import           Data.String
import           Glob.Auth.Core
import           System.Exit
import           System.Random


import qualified Glob.Import.ByteString as B

main :: IO ()
main = do
  putStrLn "Glob.Auth.Core test"
  str <- mkRandomStrMax 100
  let token = generateHash SHA1 str
      rt1   = verifyHash SHA1         str  token
      rt2   = verifyHash SHA1 (B.init str) token
  print (rt1,rt2)
  if rt1 && (not rt2) then exitSuccess
    else exitFailure

mkRandomStr :: IsString str => Int -> IO str
mkRandomStr len = fromString . ((chr . (`mod` 128)) <$>) . take len <$> getRandoms

mkRandomStrMax :: IsString str => Int -> IO str
mkRandomStrMax len = (`mod` len) <$> getRandom >>= mkRandomStr
