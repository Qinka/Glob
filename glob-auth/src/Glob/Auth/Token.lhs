% src/Glob/Auth/Token.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Auth/Token.lhs}
  \CodeProject{glob-auth}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-15}
  \CodeChangeLog{2016-08-09}{0.0.9.1}{change sha224 -> sha1, for C\# uwp do not support sha224}
  \CodeChangeLog{2016-08-09}{0.0.9.1}{add the version of auth}
  \CodeChangeLog{2016-08-10}{0.0.9.5}{changhow it work}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.1}{changed version}
\end{codeinfo}

\begin{code}
module Glob.Auth.Token
       ( transPsk2Token
       , globAuthVersion
       , globAuthVersionStr
       , globAuthVersionQuote
       ) where

import Data.Char
import Data.Digest.Pure.SHA
import Data.List
import Data.Maybe
       
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
       
import Data.Version
import Language.Haskell.TH
import Paths_glob_auth
\end{code}

      transform password to token
\begin{code}
transPsk2Token :: String -> [String] -> String
transPsk2Token timestamp psks =
  oneSHA . fromStr2ByteStrL $ concat (chgeOrder varca psks) ++ timestampSHA
  where
    fromStr2ByteStrL = BL.fromStrict . s2bUtf8
    timestampSHA =showDigest . sha1 $ fromStr2ByteStrL timestamp
    refToken = showDigest . sha256 . fromStr2ByteStrL $ head psks ++ timestampSHA
    varca' = foldr ((+).chr2hex) 0 $ take (pskCount`quot`16 +1) refToken
    varca = varca' `mod` pskCount
    pskCount = length psks
    s1   = showDigest.sha1
    s256 = showDigest.sha256
    s384 = showDigest.sha384
    s512 = showDigest.sha512
    com f g l r x = take l (f x) ++ take r (g x)
    oneSHA = case varca`mod`4 of
      0 -> com s256 s512 36 44
      1 -> com s384 s1   59 21
      2 -> com s512 s512 33 58
      3 -> com s256 s384 41 39
      _ -> s1
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
  take len . drop i . concat $ replicate 2 xs
\end{code}

      With UTF8 code
\begin{code}
s2bUtf8 :: String -> B.ByteString
s2bUtf8 = TE.encodeUtf8 . T.pack
\end{code}

version
\begin{code}
globAuthVersion = version
globAuthVersionStr = showVersion version
globAuthVersionQuote = stringE $ showVersion version
\end{code}
