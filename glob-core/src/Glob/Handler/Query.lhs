% src/Glob/Handler/Query.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Query.lhs}
  \CodeInfo{Foundation of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-08-19}
  %\CodeChangeLog{date}{text}
\end{codeinfo}


\begin{code}
module Glob.Handler.Query
    ( getQueryR
    , getNavR
    , putQueryR
    , putNavR
    , delQueryR
    , delNavR
    ) where

      import Glob.Auth.Token(globAuthVersionQuote)
      import Glob.Common
      import Glob.Foundation
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



query
\begin{code}
      getQueryR :: [T.Text] -> Handler T.Text
      getQueryR idx =
        case idx of
          "version":"glob-auth":_ -> return $globAuthVersionQuote
          "version":_ -> return $globCoreVersionQuote
          "name":_ -> return "Glob"
          "servertime":_ -> liftIO $ s2t.show <$> getCurrentTime
          [] -> return $ T.concat ["Glob",$globCoreVersionQuote]
          "index":xs -> getIndexR xs
          _ -> getQ
        where
          getQ = do
            x <- runDB'.findOne $ select ["index"=:("query":idx)] "query"
            case sigTypeFunc x of
              Just vs ->
                return $ T.unwords vs
              _ -> notFound
          sigTypeFunc :: Maybe Document -> Maybe [T.Text]
          sigTypeFunc x = x >>= (!? "var")
      getQueryR _ = notFound
\end{code}

\begin{code}
      putQueryR :: [T.Text] -> Handler T.Text
      putQueryR idx = do
        var <- lookupPostParam "var"
        runDB'.upsert (select ["index" =: idx] "query") $ catMaybes
          [ "index" =@ Just idx
          , "var"   =@      var
          ]
        return "success"
\end{code}


\begin{code}
      delQueryR :: [T.Text] -> Handler T.Text
      delQueryR idx = do
        runDB'.delete$ select ["index" =: idx] "query"
        return "success"
\end{code}

\begin{code}
      getIndexR :: [T.Text] -> Handler T.Text
      getIndexR qs  = case rp of
        Left e -> invalidArgs [T.showT e]
        Right rs -> (b2tUtf8.toStrictBS.encode.rs) <$> rests
        where
          qu = T.concat qs
          rp = runQP $ T.unpack qu
          rests = runDB' $ do
            cur <- find $ select [] "index"
            rt <- rest cur
            closeCursor cur
            return $ catMaybes $ doc2Rest <$> rt
\end{code}



nav
\begin{code}
      getNavR :: Handler T.Text
      getNavR = do
        rt <- runDB' $ (doc2Nav <$>) <$> dbAction
        return.b2tUtf8.toStrictBS.encode $ catMaybes rt
        where
          dbAction = do
            cr <- find (select [] "nav")
            rtval <- rest cr
            closeCursor cr
            return rtval
\end{code}

update nav
\begin{code}
      putNavR :: Handler T.Text
      putNavR = do
        Just idx <- lookupPostParam "label"
        url      <- lookupPostParam "url"
        order    <- lookupPostParam "order"
        runDB'.upsert (select ["label" =: idx] "nav") $ catMaybes
            [ "index" =@        Just idx
            , "url"   =@             url
            , "order" =@ (T.readT<$> order :: Maybe Int)
            ]
        return "success"
\end{code}

delete nav
\begin{code}
      delNavR :: Handler T.Text
      delNavR = do
        idx <- lookupPostParam "label"
        let sel = case idx of
              Just idx -> select ["index" =: idx] "nav"
              _ -> select [] "nav"
        runDB' $ delete sel
        return "success"
\end{code}
