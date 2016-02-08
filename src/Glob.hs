




-- src/GLob.hs

{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , QuasiQuotes
           , TypeFamilies
           , GADTs
           , FlexibleInstances
           , ViewPatterns
           #-}

module Glob
    ( module Glob
    , module Glob.Management
    ) where

      import Glob.Config
      import Glob.Database
      import Glob.Management
      import Yesod
      import Data.Text.Encoding(encodeUtf8,decodeUtf8)
      import Data.Text hiding(null,map,head)
      import Text.Blaze.Html
      import qualified Data.ByteString.Lazy as BL
      import Paths_Glob
      import Data.Aeson
      import Data.Version(showVersion)
      import Glob.Data

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
              else return $ (\[Entity _ (Qry _ t _)]->t) x

      getHomeR :: Handler Html
      getHomeR = do
        [Entity _ (Htm _ mainText mainTitle _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. "$page.main",HtmTyp ==. "home"] []
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
          selectList [HtmIndex ==. "$page.blog",HtmTyp ==. "home"] []
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
        [Entity _ (Txt _ txtText content _ )] <- liftHandlerT $ runDB $ selectList [TxtIndex ==. i] []
        selectRep $ provideRepType (encodeUtf8 content) $ return txtText

      getBinR :: Text -> Handler TypedContent
      getBinR i = do
        [Entity _ (Bin _ binText content _)] <- liftHandlerT $ runDB $ selectList [BinIndex ==. i] []
        selectRep $ provideRepType (encodeUtf8 content) $ return binText


      postNavR :: Handler TypedContent
      postNavR = do
        navs' <- liftHandlerT $ runDB $ selectList [] [Asc NavOrder]
        let navs = map (\(Entity _ x) -> x) navs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode navs

      mkYesodDispatch "Glob" resourcesGlob
