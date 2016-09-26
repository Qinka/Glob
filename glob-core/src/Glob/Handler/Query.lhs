% src/Glob/Handler/Query.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Query.lhs}
  \CodeInfo{Foundation of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-08-19}
  \CodeChangeLog{0.0.10.10}{2016.08.23}{change to use stream}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}


\begin{code}
module Glob.Handler.Query
       ( getQueryR
       , restQuery
       , getNavR
       , putNavR
       , delNavR
       , returnSucc
       ) where

import Glob.Auth.Token(globAuthVersionQuote)
import Glob.Common
import Glob.Foundation
import Glob.Handler.Internal
import Glob.Handler.Query.Parser
import Glob.Model
import Glob.Types
       
import Control.Exception(Exception,SomeException)
import Data.Conduit
import Yesod.Core
       
import Import
import Import.ByteStringUtf8 (toStrictBS)
import qualified Import.Text as T
import qualified Import.ByteStringUtf8 as B
\end{code}


\begin{code}
returnSucc :: Handler TypedContent
returnSucc = respondSource "text/plain" $ sendChunkText "success"
\end{code}

query
\begin{code}
getQueryR :: [T.Text] -> Maybe Rest -> Handler TypedContent
getQueryR idx r  =
  case tail idx of
    "~version":"glob-auth":_ -> respondSource "text/plain" $
      sendChunkText $globAuthVersionQuote
    "~version":_ -> respondSource "text/plain" $
      sendChunkText $globCoreVersionQuote
    "~name":_ -> respondSource "text/plain" $
      sendChunkText "Glob"
    "~buildinfo":_ -> respondSource "text/plain" $
      sendChunkText $globCoreBuildInfo
    "~servertime":_ -> do
      time <- liftIO $ s2t.show <$> getCurrentTime
      respondSource "text/plain" $ sendChunkText time
    [] -> respondSource "text/plain" $ do
      sendChunkText "Glob-"
      sendChunkText $globCoreVersionQuote
      sendFlush
    "@nav":_ -> getNavR
    ".index":xs -> getIndexR xs
    _ -> getQ r
  where
    getQ (Just rest@Rest{..}) = do
      var <- runDB' $ fetchQuery rest
      case var of
        Just v ->respondSource "text/plain" $ do
          sendChunkText v
          sendFlush
        _ -> notFound
    getQ _ = notFound
\end{code}

\begin{code}
restQuery :: [T.Text] -> Handler TypedContent
restQuery idx = do
  unR <- lookupPostUnRest idx
  var <- lookupPostParam "var"
  restItem unR var updateQuery
\end{code}

\begin{code}
getIndexR :: [T.Text] -> Handler TypedContent
getIndexR qs  = case rp of
  Left e -> invalidArgs [T.showT e]
  Right rs -> rests rs
  where
    qu = T.concat qs
    rp = runQP $ T.unpack qu
    rests rs = do
      rt <- tryH.runDB' $ do
        cur <- find $ select [] "index"
        rt <- rest cur
        closeCursor cur
        return.catMaybes $ doc2Rest <$> rt
      case rt of
        Left e -> returnER e
        Right item -> respondSource "application/json" $ do
          sendChunkLBS.encode.rs $ item
          sendFlush
\end{code}



nav
\begin{code}
getNavR :: Handler TypedContent
getNavR = do
  rt <- tryH.runDB' $ (doc2Nav <$>) <$> dbAction
  case rt of
    Left e -> returnER e
    Right d -> respondSource "application/json".
      sendChunkLBS.encode $ catMaybes d
  where
    dbAction = do
      cr <- find (select [] "nav")
      rtval <- rest cr
      closeCursor cr
      return rtval
\end{code}

update nav
\begin{code}
putNavR :: Handler TypedContent
putNavR = do
  Just idx <- lookupPostParam "label"
  url      <- lookupPostParam "url"
  order    <- lookupPostParam "order"
  rt <- tryH.runDB'.upsert (select ["label" =: idx] "nav") $ catMaybes
    [ "index" =@        Just idx
    , "url"   =@             url
    , "order" =@ (T.readT<$> order :: Maybe Int)
    ]
  case rt of
    Left e -> returnER e
    Right _ -> returnSucc
\end{code}

delete nav
\begin{code}
delNavR :: Handler TypedContent
delNavR = do
  idx <- lookupPostParam "label"
  let sel = case idx of
        Just idx -> select ["index" =: idx] "nav"
        _ -> select [] "nav"
  rt <- tryH.runDB' $ delete sel
  case rt of
    Left e -> returnER e
    Right _ -> returnSucc
\end{code}
