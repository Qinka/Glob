% src/Glob/Handler/Fetch.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Fetch.lhs}
  \CodeInfo{The handler of GET method}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{0.0.9.25}{2016.08.08}{Add the author to blogs and pages.}
  \CodeChangeLog{0.0.9.26}{2016.08.09}{add  the version of auth}
  \CodeChangeLog{0.0.9.30}{2016.08.12}{made bloglist more powerful}
  \CodeChangeLog{0.0.9.31}{2016.08.13}{\ref{func:bloglist} change to use  HTTP date}
\end{codeinfo}

\begin{code}
module Glob.Handler.Fetch
    ( getPostR
    , getStaticR
    , getResourceR
    , restPostR
    , restStaticR
    , restResourceR
    , restFrame
    ) where

      import Control.Exception (throw)
      import Control.Monad(mapM)
      import Data.Conduit
      import Glob.Auth.Token(globAuthVersionQuote)
      import Glob.Common
      import Glob.Foundation
      import Glob.Model
      import Glob.Types
      import Text.Blaze.Html (preEscapedToHtml)
      import Yesod.Core
      import Yesod.Core.Types

      import Import
      import Import.ByteStringUtf8 (toStrictBS)
      import Import.Wai(status301)
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

      getFile :: (MonadResource a,MonadHandler a)
              => T.Text -> a (Maybe B.ByteString)
      getFile file = getFilesBS =<< lookupFiles file

      getField :: (MonadResource a,MonadHandler a)
               => T.Text -> a (Maybe T.Text)
      getField fieled = do
        su <- b2tUtf8 <#> getFile fieled
        case su of
          Just s -> return su
          _ -> lookupPostParam fieled
\end{code}


\begin{code}
      lookupPostUnRest :: [T.Text] -> Handler (Maybe Rest)
      lookupPostUnRest idx = do
        ty <- lookupPostParam "type"
        ct <- lookupPostParam "create-time"
        ut <- lookupPostParam "update-time"
        ti <- lookupPostParam "title"
        su <- getField        "summary"
        wh <- lookupPostParam "whose"
        mi <- lookupPostParam "mime"
        tg <- lookupPostParams "tag"
        ts <- T.words <#> lookupPostParams "tags"
        return $ case (ty,ct,ut,ti) of
          (Just t,Just c,Just u,Just i) -> Just .Rest
            idx undefined t (T.readT c) (T.readT u) i su wh mi .concat $ tg:ts
          _ -> Nothing
        where
\end{code}

\begin{code}
      restItem :: Val a => Maybe Rest -> Maybe a
                          -> ( a -> Rest -> Action Handler ())
                          -> Handler T.Text
      restItem unR item f = case (unR,item) of
        (Just r,Just i) -> do
          runDB' $ f i r
          return "success"
        _ ->  invalidArgs [" args failed"]
\end{code}

pages
\begin{code}
      getPostR :: Maybe Rest -> Handler TypedContent
      getPostR (Just rest@Rest{..}) = do
        html <- runDB' $ fetchPost rest
        case html of
          Just pH -> do
            let sH = preEscapedToHtml <$> rSummary
            selectRep.provideRep.defaultLayout $ do
              setTitle $ toHtml $ rTitle
              case sH of
                Just sH' -> [whamlet|<summary id=sum>#{sH'}|]
                _ -> return ()
              [whamlet|#{pH}|]
              let au = case rWhose of
                    Just a -> showJS a
                    _ -> rawJS ("null" ::T.Text)
              toWidget [julius|author=#{au};|]
          _ -> notFound
      getPostR _ = notFound
\end{code}

\begin{code}
      restPostR :: [T.Text] -> Handler T.Text
      restPostR idx = do
        unR <- lookupPostUnRest idx
        html <- b2tUtf8 <#> getFile "html"
        restItem unR html updatePost
\end{code}

resource
\begin{code}
      getResourceR :: Bool -> Maybe Rest -> Handler TypedContent
      getResourceR t (Just rest@Rest{..}) = do
        ct <- runDB' $ fetchItem rest
        case ct of
          Just (Left  text) -> selectRep $  provideRepType (gmime rMIME) $ return text
          Just (Right binary) ->respondSource (gmime rMIME) $ sendChunkBS binary
          _ -> notFound
          where
            gmime = fromMaybe "".(t2bUtf8 <$>)
            fetchItem :: Rest-> Action Handler (Maybe (Either T.Text B.ByteString))
            fetchItem = if t
              then (Left  <#>) <$> fetchResourceT
              else (Right <#>) <$> fetchResourceB
      getResourceR _ _ = notFound
\end{code}

\begin{code}
      restResourceR :: Bool -> [T.Text] -> Handler T.Text
      restResourceR t idx = do
        unR <- lookupPostUnRest idx
        text <- b2tUtf8 <#> getFile "text"
        bin <- getFile "binary"
        if t
          then restItem unR text updateResourceT
          else restItem unR (Binary <$> bin) updateResourceB
\end{code}

\begin{code}
      getStaticR :: Maybe Rest -> Handler TypedContent
      getStaticR (Just rest@Rest{..}) = do
        url <- runDB' $ fetchStatic rest
        case url of
          Just u -> redirectWith status301 u
          _ -> notFound
      getStaticR _ = notFound
\end{code}

\begin{code}
      restStaticR :: [T.Text] -> Handler T.Text
      restStaticR idx = do
        unR <- lookupPostUnRest idx
        url <- lookupPostParam "url"
        restItem unR url updateStatic
\end{code}

\begin{code}
      restFrame :: [T.Text] -> Handler T.Text
      restFrame idx = do
        unR <- lookupPostUnRest idx
        html <- b2tUtf8 <#> getFile "html"
        restItem unR html updateFrame
\end{code}
