% src/Glob/Handler.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler.lhs}
  \CodeInfo{Export of Glob's handler}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{0.0.10.10}{2016.08.23}{using tryH to catch exception}
  \CodeChangeLog{0.0.10.10}{2016.08.23}{change to use stream}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Handler
    ( getUrlR
    , postUrlR
    , putUrlR
    , deleteUrlR
    ) where

      import Control.Exception (throw)
      import Control.Monad(mapM)
      import Glob.Auth.Token(globAuthVersionQuote)
      import Glob.Common
      import Glob.Handler.Fetch
      import Glob.Handler.Query
      import Glob.Foundation
      import Glob.Model
      import Glob.Types
      import Text.Blaze.Html (preEscapedToHtml)
      import Yesod.Core
      import Yesod.Core.Types

      import Import
      import Import.ByteStringUtf8 (toStrictBS)
      import qualified Import.Text as T
\end{code}

\begin{code}
      getUrlR,putUrlR,deleteUrlR :: [T.Text] -> Handler TypedContent
\end{code}

\begin{code}
      getUrlR idx = do
        rest <- runDB' $ fetchRest idx
        liftIO $ print rest
        case rType <$> rest of
          Just "post" -> getPostR rest
          Just "text" -> getResourceR True rest
          Just "binary" -> getResourceR False rest
          Just "static" -> getStaticR rest
          _ -> notFound
\end{code}
\begin{code}
      putUrlR idx = do
        typ <- lookupPostParam "type"
        case typ of
          Just "post" -> restPostR idx
          Just "text" -> restResourceR True idx
          Just "binary" -> restResourceR False idx
          Just "static" -> restStaticR idx
          Just "frame" -> restFrame idx
          _ -> notFound
\end{code}
\begin{code}
      deleteUrlR idx = do
        typ <- lookupPostParam "type"
        db <- case typ of
            Just "post" -> return "post"
            Just "text" -> return "resource"
            Just "binary" -> return "resource"
            Just "static" -> return "static"
            _ -> notFound
        rt <- tryH.runDB' $ deleteItem idx db
        case rt of
          Left e -> returnER e
          Right _ -> returnSucc
\end{code}

\begin{code}
      postUrlR :: [T.Text] -> Handler TypedContent
\end{code}

\begin{code}
      postUrlR idx = do
        meth <- lookupHeader "HOW"
        case (meth,idx) of
          (Just "put", "n":_) -> putNavR
          (Just "get", "n":_) -> getNavR
          (Just "del", "n":_) -> delNavR
          (Just "put", "q":is) -> putQueryR is
          (Just "get", "q":is) -> getQueryR is
          (Just "del", "q":is) -> delQueryR is
          _ -> notFound
\end{code}
