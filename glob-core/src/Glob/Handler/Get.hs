




-- src/Glob/Handler/Get.hs

{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           #-}

module Glob.Handler.Get
    ( getQueryR
    , getHomeR
    , getBlogItemR
    , getBlogListR
    , getPageR
    , getTxtR
    , getBinR
    ) where

      import Import
      import Import.Handler
      import qualified Import.ByteString as B
      import qualified Import.Text as T

      getQueryR :: Texts -> Handler T.Text
      getQueryR i =
        case i of
          "version":_ -> return $(globVersion)
          "name":_ -> return "Glob"
          "servertime":_ -> liftIO $ liftM (s2t.show) getCurrentTime
          "database-kind":_ -> return $(dbKind)
          _ -> getQry
        where
          getQry = do
            x <- liftHandlerT $ runDB $ selectList [QryIndex ==. ws2s i] []
            if null x
              then return ""
              else return $ (\[Entity _ q] -> qryTxt q) x

      getHomeR :: Handler Html
      getHomeR = do
        Entity _ h:_ <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","main"],HtmTyp ==. "home"] []
        let mainHtml = preEscapedToHtml $ htmHtml h
        let sumHtml = preEscapedToHtml $ fromMaybe "" $ htmSum h
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle h
          $(whamletFileT "home.hamlet")

      getBlogItemR :: T.Text -> Texts -> Handler Html
      getBlogItemR t i= do
        let bi = ws2s ("blog":i)
        Entity _ b:_ <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. bi,HtmCtime ==. read (t2s t),HtmTyp ==. "blog"] []
        let blogHtml = preEscapedToHtml $ htmHtml b -- blogText
        let blogSum = preEscapedToHtml $ fromMaybe "" $ htmSum b
        let biH = rawJS bi
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle b -- blogTitle
          toWidget $(juliusFileT "blogitem.julius")
          $(whamletFileT "blogitem.hamlet")

      getBlogListR :: Handler Html
      getBlogListR = do
        Entity _ h:_ <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","blog"],HtmTyp ==. "home"] []
        let blogHtml = preEscapedToHtml $ htmHtml h
        let blogSum = preEscapedToHtml $ fromMaybe "" $ htmSum h
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle h -- blogTitle
          $(whamletFileT "bloglist.hamlet")

      getPageR :: Texts -> Handler Html
      getPageR i = do
        Entity _ p:_ <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ("page":i),HtmTyp ==. "page"] []
        let pageHtml = preEscapedToHtml $ htmHtml p -- pageText
        let pageSum = preEscapedToHtml $ fromMaybe "" $ htmSum p
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle p
          $(whamletFileT "page.hamlet")

      getTxtR :: Texts -> Handler TypedContent
      getTxtR i = do
        Entity _ t:_ <- liftHandlerT $ runDB $ selectList [TxtIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 $ txtContent t) $ return $ txtTxt t

      getBinR :: Texts -> Handler TypedContent
      getBinR i = do
        Entity _ b:_ <- liftHandlerT $ runDB $ selectList [BinIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 $ binContent b) $ return $ binBin b
