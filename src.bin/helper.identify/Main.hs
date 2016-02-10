




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
      import Glob.Common
      import qualified Data.ByteString.Lazy as BL
      import Data.List(elemIndex)
      import Data.Maybe(fromMaybe)

      runToken :: [String] -> String -> String
      runToken xs time = shaR $ BL.fromStrict $ s2b $ xs !! ca ++timeS
        where
          stdToken = showDigest $ sha256 $ BL.fromStrict $ s2b $ head xs ++ timeS
          ca' = foldr ((+).oct) 0 $ take (len `quot` 16 +1) stdToken
          ca = ca' `mod` len
          len = length xs
          timeS = showDigest $ sha256 $ BL.fromStrict $ s2b time
          shaR = take 56.case ca `mod` 4 of
            0 -> showDigest.sha224
            1 -> showDigest.sha256
            2 -> showDigest.sha384
            3 -> showDigest.sha512
      oct :: Char -> Int
      oct = fromMaybe 0.flip elemIndex "0123456789abcdef"


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
