




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

module Glob
    ( module Glob
    , module Glob.Management
    ) where

      import Glob.Config
      import Glob.Database
      import Glob.Management

      import Yesod

      import Database.Persist.Postgresql

      import System.Environment

      import Data.Text.Encoding(encodeUtf8,decodeUtf8)

      import Data.Text hiding(null,map,head)
      import qualified Data.Text.Lazy as L

      import Data.Time

      import Text.Blaze.Html

      import qualified Data.ByteString as B
      import qualified Data.ByteString.Lazy as BL
      import qualified Data.ByteString.Lazy.Char8 as C
      import Data.Digest.Pure.SHA

      import Data.String(fromString)

      import Paths_Glob
      import Data.Aeson
      import Data.Version(showVersion)

      data Glob = Glob
        { conPool :: ConnectionPool
        , config :: Config
        , getManagement :: Management
        }

      instance YesodPersist Glob where
        type YesodPersistBackend Glob = SqlBackend
        runDB a = do
          (Glob p _ _) <- getYesod
          runSqlPool a p




      mkYesod "Glob" [parseRoutes|
      / HomeR GET
      /nav NavR POST
      /page/#Text PageR GET
      /blog BlogListR POST GET
      /blog/#Text/#Text BlogItemR GET
      /favicon.ico FaviconR GET
      /txt/#Text TxtR GET
      /bin/#Text BinR GET
      /query/#Text QueryR GET
      /management SubmanagementR Management getManagement
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
        isAuthorized (SubmanagementR _) _ = managementAuthorCheck
        isAuthorized _ _ = return Authorized
        defaultLayout = globLayout

      managementAuthorCheck :: Handler AuthResult
      managementAuthorCheck = do
        token <- lookupHeaders "Tokens"
        day <- lookupHeaders "Day"
        time <- lookupHeaders "TimeOfDay"
        let td = read $ read $ show $ head day
        let tt = read $ read $ show $ head time
        utcT <- liftIO getCurrentTime
        timezone <- liftIO $ getTimeZone utcT
        let localT  = utcToLocalTime timezone $ addUTCTime (-6) utcT
        let localT' = utcToLocalTime timezone $ addUTCTime   6 utcT
        (Glob _ (Config _ _ _ _ _ _ env) _) <- getYesod
        stoken <- liftIO $ getEnv env
        let s1 = showDigest $ sha256 $ BL.concat [fromString stoken,read $ show $ head day,read $ show $ head time]
        liftIO $ putStrLn s1
        if   (s1 `elem` map (read.show) token)
          && (localDay localT <= td)
          && (localTimeOfDay localT <= tt)
          && (localDay localT' >= td)
          && (localTimeOfDay localT' >= tt)
          then return Authorized
          else return $ Unauthorized ":( Who are you!"

      getQueryR :: Text -> Handler Text
      getQueryR i =
        case i of
          "version" -> return $ pack $ showVersion version
          "name" -> return "Glob"
          _ -> getQry
        where
          getQry = do
            x <- liftHandlerT $ runDB $ selectList [QryIndex ==. i] []
            if null x
              then return ""
              else return $ (\[Entity _ (Qry _ t)]->t) x

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
        (Glob _ (Config _ _ path _ _ _ _) _) <- getYesod
        sendFile "applcation/x-ico" path

      postBlogListR :: Handler TypedContent
      postBlogListR = do
        blogs' <- liftHandlerT $ runDB $ selectList [HtmTyp ==. "blog"] []
        let blogs = map (\(Entity _ x) -> x) blogs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode $
            map (\(Htm i _ tit _ tim) -> object ["index" .= i,"title" .= tit,"time" .= tim]) blogs

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
        selectRep $ provideRepType (encodeUtf8 content) $ return txtText

      getBinR :: Text -> Handler TypedContent
      getBinR i = do
        [Entity _ (Bin _ binText content)] <- liftHandlerT $ runDB $ selectList [BinIndex ==. i] []
        selectRep $ provideRepType (encodeUtf8 content) $ return binText


      postNavR :: Handler TypedContent
      postNavR = do
        navs' <- liftHandlerT $ runDB $ selectList [] [Asc NavOrder]
        let navs = map (\(Entity _ x) -> x) navs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode navs


      globLayout :: Widget -> Handler Html
      globLayout w = do
        Glob _ (Config _ _ _ ti _ _ _) _<- liftHandlerT getYesod
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
