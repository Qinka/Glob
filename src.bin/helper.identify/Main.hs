




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
        utc <- getCurrentTime
        tzone <- getTimeZone utc
        let lTime = utcToLocalTime tzone utc
        let s = showDigest $ sha256 $ fromString $ P.concat [args,show $ localDay lTime,show $ localTimeOfDay lTime]
        P.putStr $ "-H \"Tokens:"
              ++ s
              ++ "\" -H \"Day:"
              ++ (show $ localDay lTime)
              ++ "\" -H \"TimeOfDay:"
              ++ (show $ localTimeOfDay lTime)
              ++ "\""
