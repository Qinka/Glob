% src/Glob/Common.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Common.lhs}
  \CodeInfo{some common functions for glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-15}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Common
    ( t2s, s2t
    , b2tUtf8, t2bUtf8
    , s2bUtf8, b2sUtf8
    , globCoreVersion
    , globCoreVersionStr
    , globCoreVersionQuote
    , tryH, catchH, handlerH
    , fromHttpDate2UTC
    , fromUTC2HttpDate
    ) where

      import Control.Monad
      import Control.Exception
      import Data.Aeson.Types
      import Data.Either
      import Data.Time.Format
      import Yesod.Core

      import Import
      import Import.TH

      import Data.Version
      import Paths_glob_core

      import qualified Import.ByteStringUtf8 as B
      import qualified Import.Text as T
\end{code}

transform between Text and String
\begin{code}
      t2s :: T.Text -> String
      t2s = T.unpack
      s2t :: String -> T.Text
      s2t = T.pack
\end{code}

transform between Text and ByteString, using utf8
\begin{code}
      b2tUtf8 :: B.ByteString -> T.Text
      b2tUtf8 = T.decodeUtf8
      t2bUtf8 :: T.Text -> B.ByteString
      t2bUtf8 = T.encodeUtf8
\end{code}

transform between String and ByteString, using Utf8, and it not fast
\begin{code}
      s2bUtf8 :: String -> B.ByteString
      s2bUtf8 = t2bUtf8.s2t
      b2sUtf8 :: B.ByteString -> String
      b2sUtf8 = t2s.b2tUtf8
\end{code}

the version of glob-core
\begin{code}
      globCoreVersion = version
      globCoreVersionStr = showVersion version
      globCoreVersionQuote = stringE $ showVersion version
\end{code}

Throw, catch, handler the exceptions.
\begin{code}
      catchH :: Exception e
             => HandlerT site IO a
             -> (e -> HandlerT site IO a)
             -> HandlerT site IO a
      catchH f h = handlerToIO >>=
        (\hio -> liftIO $ catch (hio f) (hio.h))

      handlerH :: Exception e
               => (e -> HandlerT site IO a)
               -> HandlerT site IO a
               -> HandlerT site IO a
      handlerH = flip catchH

      tryH :: Exception e
           => HandlerT site IO a
           -> HandlerT site IO (Either e a)
      tryH f = handlerToIO >>=
        (\hio -> liftIO . try $ hio f)
\end{code}

transform between UTCTime and HTTP date
\begin{code}
      fromUTC2HttpDate :: UTCTime -> String
      fromUTC2HttpDate = formatTime defaultTimeLocale "%a, %d %b %G %T GMT"
      fromHttpDate2UTC :: String -> UTCTime
      fromHttpDate2UTC = readTime defaultTimeLocale "%a, %d %b %G %T GMT"
\end{code}
