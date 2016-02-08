




-- tools.bin/helper.identify

{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

      import Prelude as P
      import Data.Time
      import Data.Digest.Pure.SHA
      import System.Environment
      import Data.String(fromString)


      main :: IO ()
      main = do
        (args:r:_) <- getArgs
        ut' <- getCurrentTime
        let ut = addUTCTime (fromIntegral $ read r) ut'
        let s = showDigest $ sha256 $ fromString $ args ++ show ut
        cmd' <- P.getContents
        let cmd = (concat.lines) cmd'
              ++ " -H \"Tokens:"
              ++ s
              ++ "\" -H \"UTCTime:"
              ++ show ut
              ++ "\""
        putStr cmd
