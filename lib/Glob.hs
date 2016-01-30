




-- lib/GLob.hs

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Glob where

      import Glob.Config

      import Yesod

      import Database.Persist.Postgresql

      import Data.Aeson

      import Data.Text.Lazy.Encoding(decodeUtf8)
      import Data.Text.Encoding(encodeUtf8)

      import Data.Text.Lazy hiding(null,map)

      import Data.Time

      import Text.Blaze.Html

      import qualified Data.ByteString as B



      data Glob = Glob
        { conPool :: ConnectionPool
        , config :: Config
        }

      instance YesodPersist Glob where
        type YesodPersistBackend Glob = SqlBackend
        runDB a = do
          (Glob p _) <- getYesod
          runSqlPool a p






      share [mkPersist sqlSettings,mkMigrate "migrateAll"] [persistLowerCase|
        Nav json sql=table_nav
          Id sql=
          label Text sql=key_label
          order Int Maybe sql=key_order
          ref Text sql=key_ref
          Primary label
          deriving Eq Show
        Htm sql=table_html
          Id sql=
          index Text sql=key_index
          html Text sql=key_html
          title Text sql=key_title
          typ Text sql=key_content
          time Day sql=key_time
          Primary index time
        Txt sql=table_txt
          Id sql=
          index Text sql=key_index
          txt Text sql=key_text
          content Text sql=key_content
          Primary index
        Bin sql=table_bin
          Id sql=
          index Text sql=key_index
          bin B.ByteString sql=key_binary
          content Text sql=key_content
          Primary index
      |]




      mkYesod "Glob" [parseRoutes|
      / HomeR GET
      /nav NavR POST
      /page/#Text PageR GET
      /blog BlogListR POST GET
      /blog/#Text/#Text BlogItemR GET
      /favicon.ico FaviconR GET
      /txt/#Text TxtR GET
      /bin/#Text BinR GET
      |]

      instance Yesod Glob where
        errorHandler NotFound = selectRep $ provideRep $ defaultLayout [whamlet|NotFound-404|]
        errorHandler NotAuthenticated = selectRep $ provideRep $ defaultLayout [whamlet|NotAuthenticated|]
        errorHandler (PermissionDenied x) = selectRep $ provideRep $ defaultLayout [whamlet|
          PermissionDenied
          <br>
          #{x}
          |]
        errorHandler (InvalidArgs x) = selectRep $ provideRep $ defaultLayout [whamlet|
          InvalidArgs
          <br>
          #{show x}
          |]
        errorHandler (BadMethod x) = selectRep $ provideRep $ defaultLayout [whamlet|
          BadMethod
          <br>
          #{show x}
          |]
        errorHandler (InternalError x) = selectRep $ provideRep $ defaultLayout [whamlet|
          InternalError
          <br>
          #{x}
          |]
        isAuthorized _ _ = return Authorized
        defaultLayout = globLayout

      getHomeR :: Handler Html
      getHomeR = do
        [Entity _ (Htm _ mainText mainTitle _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. "@#page.main",HtmTyp ==. "home"] []
        let mainHtml = preEscapedToHtml mainText
        defaultLayout $ do
          setTitle $ toHtml mainTitle
          [whamlet|#{mainHtml}|]
      getBlogItemR :: Text -> Text -> Handler Html
      getBlogItemR t i= do
        [Entity _ (Htm _ blogText blogTitle _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. i,HtmTime ==. read (read $ show t),HtmTyp ==. "blog"] []
        let blogHtml = preEscapedToHtml blogText
        defaultLayout $ do
          setTitle $ toHtml blogTitle
          [whamlet|#{blogHtml}|]
      getFaviconR :: Handler Html
      getFaviconR = do
        (Glob _ (Config _ _ path _ _ _)) <- getYesod
        sendFile "applcation/x-ico" path

      postBlogListR :: Handler TypedContent
      postBlogListR = do
        blogs' <- liftHandlerT $ runDB $ selectList [HtmTyp ==. "blog"] []
        let blogs = map (\(Entity _ x) -> x) blogs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ encode $ map (\(Htm i _ tit _ tim) -> object ["index" .= i,"title" .= tit,"time" .= tim]) blogs

      getBlogListR :: Handler Html
      getBlogListR = do
        [Entity _ (Htm _ blogText blogTitle _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. "@#page.blog",HtmTyp ==. "home"] []
        let blogHtml = preEscapedToHtml blogText
        defaultLayout $ do
          setTitle $ toHtml blogTitle
          [whamlet|#{blogHtml}|]

      getPageR :: Text -> Handler Html
      getPageR i = do
        [Entity _ (Htm _ pageText pageTitle_ _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. i,HtmTyp ==. "page"] []
        let pageHtml = preEscapedToHtml pageText
        defaultLayout $ do
          setTitle $ toHtml pageTitle_
          [whamlet|#{pageHtml}|]


      getTxtR :: Text -> Handler TypedContent
      getTxtR i = do
        [Entity _ (Txt _ txtText content)] <- liftHandlerT $ runDB $ selectList [TxtIndex ==. i] []
        selectRep $ provideRepType (encodeUtf8 $ toStrict content) $ return txtText

      getBinR :: Text -> Handler TypedContent
      getBinR i = do
        [Entity _ (Bin _ binText content)] <- liftHandlerT $ runDB $ selectList [BinIndex ==. i] []
        selectRep $ provideRepType (encodeUtf8 $ toStrict content) $ return binText


      postNavR :: Handler TypedContent
      postNavR = do
        navs' <- liftHandlerT $ runDB $ selectList [] [Desc NavOrder]
        let navs = map (\(Entity _ x) -> x) navs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ encode navs


      globLayout :: Widget -> Handler Html
      globLayout w = do
        Glob _ (Config _ _ _ ti _ _) <- liftHandlerT getYesod
        pc <- widgetToPageContent w
        [Entity _ (Htm _ topText _ _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. "@#page.frame.top" , HtmTyp ==. "home"] []
        [Entity _ (Htm _ bottomText _ _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. "@#page.frame.bottom", HtmTyp ==. "home"] []
        [Entity _ (Htm _ navText _ _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. "@#page.frame.nav", HtmTyp ==. "home"] []
        let topHtml = preEscapedToHtml topText
        let bottomHtml = preEscapedToHtml bottomText
        let navHtml = preEscapedToHtml navText
        withUrlRenderer --width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no
          [hamlet|
            $newline never
            $doctype 5
            <html>
              <head>
                <title>
                  #{ti} - #{pageTitle pc}
                <meta charset=utf-8>
                <meta name=viewport content=width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no>
                ^{pageHead pc}
              <body>
                <link rel=stylesheet href=@{TxtR "css.frame.css"}>
                #{topHtml}
                #{navHtml}
                ^{pageBody pc}
                #{bottomHtml}
          |]
