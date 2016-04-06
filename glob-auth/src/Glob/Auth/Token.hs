




-- src/Glob/Auth.hs

{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           , TypeFamilies
           #-}

module Glob.Auth.Token
    ( runToken
    ) where

      import Prelude as P
      import Data.Digest.Pure.SHA
      import qualified Data.ByteString as B
      import qualified Data.ByteString.Lazy as BL
      import qualified Data.Text as T
      import qualified Data.Text.Encoding as TE
      import Data.Maybe
      import Data.List

      runToken :: [String] -> String -> String
      runToken xs time = shaR $ fStBSL $ concat (loop xs ca) ++ timeS
        where
          fStBSL = BL.fromStrict .s2b
          stdToken = showDigest $ sha256 $ fStBSL $ head xs ++ timeS
          ca' = foldr ((+).oct) 0 $ take (len `quot` 16 +1) stdToken
          ca = ca' `mod` len
          len = length xs
          timeS = showDigest $ sha256 $ fStBSL time
          shaR = take 56.case ca `mod` 4 of
            0 -> showDigest.sha224
            1 -> showDigest.sha256
            2 -> showDigest.sha384
            3 -> showDigest.sha512
      oct :: Char -> Int
      oct = fromMaybe 0.flip elemIndex "0123456789abcdef"
      loop :: [a] -> Int -> [a]
      loop xs i = let len = length xs in
        take len $ drop i $ concat $ replicate 2 xs

      s2b :: String -> B.ByteString
      s2b = TE.encodeUtf8 . T.pack
