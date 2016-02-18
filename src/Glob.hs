




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
      import Glob.Management.TH(a2o)
      import Glob.Common
      import Yesod
      import Data.Text.Encoding(encodeUtf8,decodeUtf8)
      import Data.Text hiding(null,map,head)
      import qualified Data.Text as T
      import Text.Blaze.Html
      import qualified Data.ByteString.Lazy as BL
      import Paths_Glob
      import Data.Aeson
      import Data.Version(showVersion)
      import Glob.Data
      import Data.Time
      import Control.Monad

      getQueryR :: Texts -> Handler Text
      getQueryR i =
        case i of
          "version":_ -> return $ pack $ showVersion version
          "name":_ -> return "Glob"
          "servertime":_ -> liftIO $ liftM (s2t.show)getCurrentTime
          _ -> getQry
        where
          getQry = do
            x <- liftHandlerT $ runDB $ selectList [QryIndex ==. ws2s i] []
            if null x
              then return ""
              else return $ (\[Entity _ (Qry _ t _)]->t) x

      getHomeR :: Handler Html
      getHomeR = do
        [Entity _ (Htm _ mainText mainTitle _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","main"],HtmTyp ==. "home"] []
        let mainHtml = preEscapedToHtml mainText
        defaultLayout $ do
          setTitle $ toHtml mainTitle
          [whamlet|#{mainHtml}|]

      getBlogItemR :: Text -> Texts -> Handler Html
      getBlogItemR t i= do
        [Entity _ (Htm _ blogText blogTitle _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ("blog":i),HtmTime ==. read (read $ show t),HtmTyp ==. "blog"] []
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
        inde <- lookupPostParams "index"
        len <- liftHandlerT $ lookupPostParam "lenlimit"
        blogs' <- liftHandlerT $ runDB $ selectList ((HtmTyp ==. "blog"):ind inde) []
        let blogs = map (\(Entity _ x) -> x) blogs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode $
            map (\(Htm i h tit _ tim) -> object $
              ["index" .= i,"title" .= tit,"time" .= (show tim)] ++ from len h
            ) blogs
          where
            from (Just len) x= ["content" .= T.take (read $ t2s len) x]
            from _ _ = []
            ind [] = []
            ind xs = a2o $ map (\x -> [HtmIndex ==. x]) xs

      getBlogListR :: Handler Html
      getBlogListR = do
        [Entity _ (Htm _ blogText blogTitle _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","blog"],HtmTyp ==. "home"] []
        let blogHtml = preEscapedToHtml blogText
        defaultLayout $ do
          setTitle $ toHtml blogTitle
          [whamlet|#{blogHtml}|]

      getPageR :: Texts -> Handler Html
      getPageR i = do
        [Entity _ (Htm _ pageText pageTitle_ _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ("page":i),HtmTyp ==. "page"] []
        let pageHtml = preEscapedToHtml pageText
        defaultLayout $ do
          setTitle $ toHtml pageTitle_
          [whamlet|#{pageHtml}|]

      getTxtR :: Texts -> Handler TypedContent
      getTxtR i = do
        [Entity _ (Txt _ txtText content _ )] <- liftHandlerT $ runDB $ selectList [TxtIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 content) $ return txtText

      getBinR :: Texts -> Handler TypedContent
      getBinR i = do
        [Entity _ (Bin _ binText content _)] <- liftHandlerT $ runDB $ selectList [BinIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 content) $ return binText

      postNavR :: Handler TypedContent
      postNavR = do
        navs' <- liftHandlerT $ runDB $ selectList [] [Asc NavOrder]
        let navs = map (\(Entity _ x) -> x) navs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode navs

      mkYesodDispatch "Glob" resourcesGlob
