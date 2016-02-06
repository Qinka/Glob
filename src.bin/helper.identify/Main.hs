




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
        args:_ <- getArgs
        ut <- getCurrentTime
        tzone <- getTimeZone ut
        let lTime = utcToLocalTime tzone ut
        let s = showDigest $ sha256 $ fromString $ P.concat [args,show $ localDay lTime,show $ localTimeOfDay lTime]
        cmd <- P.getContents
        P.putStr $ (concat.lines) cmd
              ++ " -H \"Tokens:"
              ++ s
              ++ "\" -H \"Day:"
              ++ show (localDay lTime)
              ++ "\" -H \"TimeOfDay:"
              ++ show (localTimeOfDay lTime)
              ++ "\""
