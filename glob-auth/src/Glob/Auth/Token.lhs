% src/Glob/Auth/Token.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Auth/Token.lhs}
  \CodeProject{glob-auth}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-15}
  \CodeChangeLog{2016-08-09}{0.0.9.1}{change sha224 -> sha1, for C\# uwp do not support sha224}
\end{codeinfo}

\begin{code}
module Glob.Auth.Token
    ( transPsk2Token
    ) where

      import Data.Char
      import Data.Digest.Pure.SHA
      import Data.List
      import Data.Maybe

      import qualified Data.ByteString as B
      import qualified Data.ByteString.Lazy as BL
      import qualified Data.Text as T
      import qualified Data.Text.Encoding as TE
\end{code}

      transform password to token
\begin{code}
      transPsk2Token :: String -> [String] -> String
      transPsk2Token timestamp psks =
        oneSHA $ fromStr2ByteStrL $ concat (chgeOrder varca psks) ++ timestampSHA
        where
          fromStr2ByteStrL = BL.fromStrict . s2bUtf8
          timestampSHA =showDigest $ sha1 $ fromStr2ByteStrL timestamp
          refToken = showDigest $ sha256 $ fromStr2ByteStrL $ head psks ++ timestampSHA
          varca' = foldr ((+).chr2hex) 0 $ take (pskCount`quot`16 +1) refToken
          varca = varca' `mod` pskCount
          pskCount = length psks
          oneSHA = take 56 . case varca`mod`4 of
            0 -> showDigest.sha256
            1 -> showDigest.sha512
            2 -> showDigest.sha1
            3 -> showDigest.sha384
            _ -> showDigest.sha224
\end{code}

      char to hex e.g. b -> 11
\begin{code}
      chr2hex :: Char -> Int
      chr2hex = fromMaybe 0.flip elemIndex "0123456789abcdef".toLower
\end{code}

      Change the order of list by circle queue
\begin{code}
      chgeOrder :: Int -> [a] -> [a]
      chgeOrder i xs = let len = length xs in
        take len $ drop i $ concat $ replicate 2 xs
\end{code}

      With UTF8 code
\begin{code}
      s2bUtf8 :: String -> B.ByteString
      s2bUtf8 = TE.encodeUtf8 . T.pack
\end{code}
