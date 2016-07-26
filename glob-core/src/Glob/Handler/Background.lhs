% src/Glob/Handler/Background.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Background.lhs}
  \CodeInfo{The handler of update(insert),and delete datas}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-22}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Handler.Background
    ( putPageR     , deletePageR
    , putBlogR     , deleteBlogR
    , putTagR      , deleteTagR
    , putNavR      , deleteNavR
    , putFrameR    , deleteFrameR
    , putQueryR    , deleteQueryR
    , putResourceR , deleteResourceR
    , postRawR
    ) where

      import Glob.Common
      import Glob.Foundation
      import Glob.Model

      import Control.Exception(Exception,SomeException)
      import Data.Conduit
      import Yesod.Core

      import Import
      import Import.ByteStringUtf8 (toStrictBS)
      import qualified Import.Text as T
      import qualified Import.ByteStringUtf8 as B
\end{code}

Get file's text
\begin{code}
      getFilesBS :: (MonadResource a,MonadHandler a)
                 => [FileInfo] -> a (Maybe B.ByteString)
      getFilesBS [] = return Nothing
      getFilesBS xs = Just. B.concat.concat <$>
        mapM (sourceToList.fileSource) xs
\end{code}

update from Nothing
\begin{code}
      infix 0 =@
      (=@) :: Val v => Label -> Maybe v -> Maybe Field
      (=@) l = ((Just.(l =:)) =<<)
\end{code}

for common return
\begin{code}
      returnRT = (returnRT' =<<)
      returnRT' :: Either SomeException a -> Handler TypedContent
      returnRT' i = selectRep.provideRep $ case i of
          Left e  -> return.T.pack $ show e
          Right _ -> return "success"
\end{code}

update pages
\begin{code}
      putPageR :: [T.Text] -> Handler TypedContent
      putPageR idx = do
        html    <- getFilesBS =<< lookupFiles "html"
        title   <- lookupPostParam            "title"
        summary <- lookupPostParam            "summary"
        uTime   <- lookupPostUTCTime          "update-time"
        cTime   <- lookupPostUTCTime          "create-time"
        returnRT.tryH.runDB'.upsert (select ["index" =: idx,"type"=:("page"::T.Text)] "html") $ catMaybes
          [ "index"       =@       Just idx
          , "title"       =@            title
          , "html"        =@ b2tUtf8<$> html
          , "summary"     =@            summary
          , "update-time" =@            uTime
          , "create-time" =@            cTime
          , "type"        =@       Just ("page"::T.Text)
          ]
\end{code}

update blog
\begin{code}
      putBlogR :: [T.Text] -> Handler TypedContent
      putBlogR idx = do
        html    <- getFilesBS =<< lookupFiles "html"
        title   <- lookupPostParam            "title"
        summary <- lookupPostParam            "summary"
        uTime   <- lookupPostUTCTime          "update-time"
        cTime   <- lookupPostUTCTime          "create-time"
        returnRT.tryH.runDB'.upsert (select ["index" =: idx,"type"=:("blog"::T.Text)] "html") $ catMaybes
          [ "index"       =@       Just idx
          , "title"       =@            title
          , "html"        =@ b2tUtf8<$> html
          , "summary"     =@            summary
          , "update-time" =@            uTime
          , "create-time" =@            cTime
          , "type"        =@       Just ("blog" ::T.Text)
          ]
\end{code}

update frame
\begin{code}
      putFrameR :: [T.Text] -> Handler TypedContent
      putFrameR idx = do
        html    <- getFilesBS =<< lookupFiles "html"
        title   <- lookupPostParam            "title"
        summary <- lookupPostParam            "summary"
        uTime   <- lookupPostUTCTime          "update-time"
        cTime   <- lookupPostUTCTime          "create-time"
        typ     <- lookupPostParam            "type"
        returnRT.tryH.runDB'.upsert (select ["index" =: idx] "html") $ catMaybes
          [ "index"       =@        Just idx
          , "title"       =@             title
          , "html"        =@  b2tUtf8<$> html
          , "summary"     =@             summary
          , "update-time" =@             uTime
          , "create-time" =@             cTime
          , "type"        =@             typ
          ]
\end{code}

Update query
\begin{code}
      putQueryR :: [T.Text] -> Handler TypedContent
      putQueryR idx = do
        var <- lookupPostParam "var"
        returnRT.tryH.runDB'.upsert (select ["index" =: idx] "query") $ catMaybes
          [ "index" =@ Just idx
          , "var"   =@      var
          ]
\end{code}

tag update
\begin{code}
      putTagR :: [T.Text] -> Handler TypedContent
      putTagR _ = selectRep.provideRep $ return ("TODO" ::T.Text)
\end{code}

resource update
\begin{code}
      putResourceR :: [T.Text] -> Handler TypedContent
      putResourceR idx = do
        Just typ <- lookupPostParam "type"
        mime <- lookupPostParam "mime"
        returnRT $ case typ of
          "txt" -> do
            text <- getFilesBS =<< lookupFiles "text"
            tryH.runDB'.upsert (select ["index" =: idx,"type" =: ("txt"::T.Text)] "resource") $ catMaybes
              [ "index"  =@       Just idx
              , "mime"   =@            mime
              , "txt"    =@ b2tUtf8<$> text
              , "type"   =@       Just typ
              ]
          "binary" -> do
            bin <- getFilesBS =<< lookupFiles "binary"
            tryH.runDB'.upsert (select ["index" =: idx,"type" =: ("binary"::T.Text)] "resource") $ catMaybes
              [ "index"  =@      Just idx
              , "mime"   =@           mime
              , "binary" =@ Binary<$> bin
              , "type"   =@      Just typ
              ]
          _ ->notFound
\end{code}

update nav
\begin{code}
      putNavR :: Handler TypedContent
      putNavR = do
        Just idx <- lookupPostParam "label"
        url      <- lookupPostParam "url"
        order    <- lookupPostParam "order"
        returnRT.tryH.runDB'.upsert (select ["label" =: idx] "nav") $ catMaybes
          [ "index" =@ Just idx
          , "url"   =@      url
          , "order" =@      order
          ]
\end{code}

delete page
\begin{code}
      deletePageR :: [T.Text] -> Handler TypedContent
      deletePageR idx =
        returnRT.tryH.runDB'.delete$ select ["index" =: idx, "type" =: ("page"::T.Text)] "html"
\end{code}

delete blog
\begin{code}
      deleteBlogR :: [T.Text] -> Handler TypedContent
      deleteBlogR idx =
        returnRT.tryH.runDB'.delete$ select ["index" =: idx, "type" =: ("blog"::T.Text)] "html"
\end{code}

delete frame
\begin{code}
      deleteFrameR :: [T.Text] -> Handler TypedContent
      deleteFrameR idx =
        returnRT.tryH.runDB'.delete$ select ["index" =: idx] "html"
\end{code}

delete query
\begin{code}
      deleteQueryR :: [T.Text] -> Handler TypedContent
      deleteQueryR idx =
        returnRT.tryH.runDB'.delete$ select ["index" =: idx] "query"
\end{code}


delete tags
\begin{code}
      deleteTagR :: [T.Text] -> Handler TypedContent
      deleteTagR _ = selectRep.provideRep $ return ("TODO" ::T.Text)
\end{code}

delete resource
\begin{code}
      deleteResourceR :: [T.Text] -> Handler TypedContent
      deleteResourceR idx =
        returnRT.tryH.runDB'.delete$ select ["index" =: idx] "resource"
\end{code}

delete nav
\begin{code}
      deleteNavR :: Handler TypedContent
      deleteNavR = do
        Just idx <- lookupPostParam "label"
        returnRT.tryH.runDB'.delete$ select ["index" =: idx] "nav"
\end{code}


raw js for mongo
\begin{code}
      postRawR :: Handler TypedContent
      postRawR = do
        Just js <- getFilesBS =<< lookupFiles "javascript"
        selectRep.provideRep.(T.showT<$>).runDB'.runCommand1 $ b2tUtf8 js
\end{code}
