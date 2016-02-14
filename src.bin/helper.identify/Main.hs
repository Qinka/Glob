




-- src.bin/helper.identify/Main.hs

{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

      import Prelude as P
      import Data.Time
      import Data.Digest.Pure.SHA
      import System.Environment
      import Data.String(fromString)
      import Glob.Common
      import qualified Data.ByteString.Lazy as BL
      import Data.List(elemIndex)
      import Data.Maybe(fromMaybe)
      import Glob.Auth(runToken)

      main :: IO ()
      main = do
        (r:stoken) <- getArgs
        ut' <- getCurrentTime
        let ut = show $ addUTCTime (fromIntegral $ read r) ut'
        let s = runToken stoken ut
        cmd' <- P.getContents
        let cmd = (concat.lines) cmd'
              ++ " -H \"Tokens:"
              ++ s
              ++ "\" -H \"UTCTime:"
              ++ ut
              ++ "\""
        putStr cmd
