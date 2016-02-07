




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
      import System.Process
      import System.Exit


      main :: IO ()
      main = do
        (args:r:_) <- getArgs
        ut' <- getCurrentTime
        tzone <- getTimeZone ut'
        let ut = addUTCTime (fromRational $ read r) ut'
        let lTime = utcToLocalTime tzone ut
        let s = showDigest $ sha256 $ fromString $ P.concat [args,show $ localDay lTime,show $ localTimeOfDay lTime]
        cmd' <- P.getContents
        let cmd = (concat.lines) cmd'
              ++ " -H \"Tokens:"
              ++ s
              ++ "\" -H \"Day:"
              ++ show (localDay lTime)
              ++ "\" -H \"TimeOfDay:"
              ++ show (localTimeOfDay lTime)
              ++ "\""
        putStr cmd
