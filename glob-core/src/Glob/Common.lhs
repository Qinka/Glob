% src/Glob/Common.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Common.lhs}
  \CodeInfo{some common functions for glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-15}
  \CodeChangeLog{0.0.9.25}{2016.08.08}{
    Add a function named showJS, trans between Text and JS.
    It was a subfunction in Glob.Handler.Get.
    }
  \CodeChangeLog{0.0.9.31}{2016.08.13}{add <#> eq2 (f <\$>) <\$>}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{0.0.10.10}{2016.08.23}{add some functions to be helper of tryH}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
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
       , lookupPostUTCTime
       , showJS,rawJS
       , (<#>),(<%>), (=@)
       , returnE,returnET,returnER
       , globCoreBuildInfo
       ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Data.Aeson.Types
import Data.Char(toUpper)
import Data.Either
import Data.Time.Format
import Database.MongoDB ((=:))
import Language.Haskell.TH
import System.Info
import Text.Julius (rawJS,RawJavascript)
import Yesod.Core
       
import Import
import Import.TH
       
import Data.Version
import Paths_glob_core

import qualified Database.MongoDB as MDB
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

a kind of <\$>
\begin{code}
infixl 4 <#>,<%>
(<#>) :: (Functor f1,Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<#>) f = ((f <$>) <$>)
(<%>) :: (Monad m,Functor f) => (a -> m b) -> f (m a) -> f (m b)
(<%>) f = ((f =<<) <$>)
\end{code}


lookup time from param
\begin{code}
lookupPostUTCTime :: Yesod site
                  => T.Text -> HandlerT site IO (Maybe UTCTime)
lookupPostUTCTime = ((T.readT <$>) <$>).lookupPostParam
\end{code}

\begin{code}
showJS :: Show a => a -> RawJavascript
showJS = rawJS. T.showT
\end{code}


update from Nothing
\begin{code}
infix 0 =@
(=@) ::MDB.Val v => MDB.Label -> Maybe v -> Maybe MDB.Field
(=@) l = ((Just.(l =:)) =<<)
\end{code}

\begin{code}
returnE :: (Monad m,Exception e) => e -> m String
returnE = pure.(\x -> "{\"error\":\"exception\",\"content\":\""++x++"\"}").show
returnET :: (Monad m,Exception e) => e -> m T.Text
returnET = (T.pack <$>).returnE
returnER :: SomeException -> HandlerT site IO TypedContent
returnER e = returnE e >>= (\strE -> respondSource "application/json"  $ do
                               sendChunk strE
                               sendFlush
                           )
\end{code}


the build time of Glob
\begin{code}
globCoreBuildInfo :: Q Exp
globCoreBuildInfo = do
  timeStr <- formatTime defaultTimeLocale "-%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  stringE $ os ++ "-" ++ arch ++ "-"  ++ map toUpper compilerName ++ "-" ++ showVersion compilerVersion++timeStr
\end{code}
