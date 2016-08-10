% src/Glob/Handler/Get.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Get.lhs}
  \CodeInfo{The handler of GET method}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{0.0.9.25}{2016.08.08}{Add the author to blogs and pages.}
  \CodeChangeLog{0.0.9.26}{2016.08.09}{add  the version of auth}
\end{codeinfo}

\begin{code}
module Glob.Handler.Get
    ( getHomeR
    , getBlogListR
    , getPageR
    , getBlogR
    , getTagR
    , getNavR
    , getQueryR
    , getResourceR
    ) where

      import Control.Exception (throw)
      import Control.Monad(mapM)
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
      import qualified Import.Text as T
\end{code}

Home
\begin{code}
      getHomeR :: Handler Html
      getHomeR = do
        home' <- runDB' $  (doc2HR =<<) <$> findOne (select ["index"=:["frame",("home"::String)]] "html")
        case home' of
          Just home -> do
            let homeH = preEscapedToHtml $ hrpHtml home
            let sumH  = preEscapedToHtml <$> hrpSummary home
            defaultLayout $ do
              setTitle $ toHtml $ hrpTitle home
              case sumH of
                Just sH -> [whamlet|#{sH}|]
                _ -> return ()
              [whamlet|#{homeH}|]
              let au = case hrpAuthor home of
                    Just a -> showJS a
                    _ -> rawJS ("null" ::T.Text)
              toWidget [julius|
                  author=#{au}
                  |]
          _ -> notFound
\end{code}

the list of blog
\begin{code}
      getBlogListR :: Handler T.Text
      getBlogListR = do
        blog' <- runDB' $ do
          cr <-find (select ["type"=:("blog"::T.Text)] "html")
          rtval <- rest cr
          closeCursor cr
          return rtval
        let blogs = map (\b -> (hrbIndex b,hrbTitle b,hrbCreateTime b,hrbUpdateTime b,hrbSummary b,hrbTags b,hrbAuthor b)).catMaybes $ doc2HR <$> blog'
        return.b2tUtf8.toStrictBS.encode$ toValue <$> blogs
        where
          toValue (i,t,c,u,s,ts,a) = object
            [ "index".=i , "title".=t
            , "create-time".=show c , "update-time".=show u
            , "summary" .= s, "tags" .= ts
            , "author"  .= a
            ]
\end{code}

pages
\begin{code}
      getPageR :: [T.Text] -> Handler Html
      getPageR idx = do
        page <- runDB' $ (doc2HR=<<) <$> findOne (select ["index"=:idx,"type"=:("page"::T.Text)] "html")
        case page of
          Just HtmlPage{..} -> do
            let pH = preEscapedToHtml     hrpHtml
            let sH = preEscapedToHtml <$> hrpSummary
            defaultLayout $ do
              setTitle $ toHtml hrpTitle
              case sH of
                Just sH' -> [whamlet|<summary id=sum>#{sH'}|]
                _ -> return ()
              [whamlet|#{pH}|]
              let au = case hrpAuthor of
                    Just a -> showJS a
                    _ -> rawJS ("null" ::T.Text)
              toWidget [julius|
                  author=#{au}
                  |]
          _ -> notFound
\end{code}
blogs
\begin{code}
      getBlogR :: [T.Text] -> Handler Html
      getBlogR idx = do
        blog <- runDB' $ (doc2HR =<<) <$> findOne
          (select ["index"=:idx,"type"=: ("blog" ::T.Text)] "html")
        case blog of
          Just HtmlBlog{..} -> do
            let pH = preEscapedToHtml     hrbHtml
            let sH = preEscapedToHtml <$> hrbSummary
            defaultLayout $ do
              setTitle $ toHtml hrbTitle
              case sH of
                Just sH' -> [whamlet|<summary id=sum>#{sH'}|]
                _ -> return ()
              [whamlet|#{pH}|]
              let au = case hrbAuthor of
                    Just a -> showJS a
                    _ -> rawJS ("null" ::T.Text)
              toWidget [julius|
                tags=#{showJS hrbTags}
                author=#{au}
                |]
          _ -> notFound
\end{code}

query
\begin{code}
      getQueryR :: [T.Text] -> Handler T.Text
      getQueryR idx = do
        case idx of
          "version":"glob-auth":_ -> return $globAuthVersionQuote
          "version":_ -> return $globCoreVersionQuote
          "name":_ -> return "Glob"
          "servertime":_ -> liftIO $ s2t.show <$> getCurrentTime
          _ -> getQ
        where
          getQ = do
            x <- runDB' $ findOne (select ["index"=:idx] "query")
            case sigTypeFunc x of
              Just vs ->
                return $ T.unwords vs
              _ -> notFound
          sigTypeFunc :: Maybe Document -> Maybe [T.Text]
          sigTypeFunc x = x >>= (!? "var")
\end{code}

tags
\begin{code}
      getTagR :: [T.Text] -> Handler Html
      getTagR _ = defaultLayout [whamlet|TODO|]
\end{code}

resource
\begin{code}
      getResourceR :: [T.Text] -> Handler TypedContent
      getResourceR idx = do
        rc <- runDB' $ (doc2Rc =<<) <$> findOne (select ["index" =: idx] "resource")
        selectRep $ case rc of
          Just RcTxt{..} -> provideRepType (t2bUtf8 rctMIME) $ return rctTxt
          Just RcBin{..} -> provideRepType (t2bUtf8 rcbMIME) $ return rcbBinary
          _ -> throw $ HCError NotFound
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
