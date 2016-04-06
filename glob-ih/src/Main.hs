




-- src/Main.hs

module Main
    ( main
    ) where

      import Prelude as P
      import Data.Time
      import System.Environment

      import Glob.Auth.Token

      main :: IO()
      main = do
        (r:stoken) <- getArgs
        ut' <- getCurrentTime
        let ut = show $ addUTCTime (fromIntegral $ read r) ut'
        let s = runToken stoken ut
        cmd' <- P.getContents
        putStr.concat.lines $ cmd'
                           ++ " -H \"Tokens:"
                           ++ s
                           ++ "\" -H \"UTCTime:"
                           ++ ut
                           ++ "\""
