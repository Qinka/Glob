




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
    , module Glob.Static
    ) where

      import Glob.Config
      import Glob.Database
      import Glob.Management
      import Glob.Management.TH(a2o)
      import Glob.Common
      import Glob.Static
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
      import Text.Julius(rawJS)
      import Data.Maybe(fromMaybe)

      getQueryR :: Texts -> Handler Text
      getQueryR i =
        case i of
          "version":_ -> return $ pack $ showVersion version
          "name":_ -> return "Glob"
          "servertime":_ -> liftIO $ liftM (s2t.show) getCurrentTime
          _ -> getQry
        where
          getQry = do
            x <- liftHandlerT $ runDB $ selectList [QryIndex ==. ws2s i] []
            if null x
              then return ""
              else return $ (\[Entity _ q]- qryTxt q>) x

      getHomeR :: Handler Html
      getHomeR = do
        [Entity _ h] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","main"],HtmTyp ==. "home"] []
        let mainHtml = preEscapedToHtml $ htmHtml h
        let sumHtml = preEscapedtoHtml $ fromMaybe "" $ htmSum h
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle h
          [whamlet|
            <div class=summaryS id=summaryI>
              #{sumHtml}
            #{mainHtml}
          |]

      getBlogItemR :: Text -> Texts -> Handler Html
      getBlogItemR t i= do
        let bi = ws2s ("blog":i)
        [Entity _ (Htm _ blogText blogTitle _ _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. bi,HtmCtime ==. read (t2s t),HtmTyp ==. "blog"] []
        let blogHtml = preEscapedToHtml $ htmHtml b -- blogText
        let blogSum = preEscapedToHtml $ htmSum b
        let biH = rawJS bi
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle b -- blogTitle
          toWidget [julius| globblogid="#{biH}"; |]
          [whamlet|
            <div class=summaryS id=summaryI>
              #{blogSum}
            #{blogHtml}
          |]

      postBlogListR :: Handler TypedContent
      postBlogListR = do
        inde <- lookupPostParams "index"
        len <- liftHandlerT $ lookupPostParam "lenlimit"
        blogs' <- liftHandlerT $ runDB $ selectList ((HtmTyp ==. "blog"):ind inde) [Desc HtmCtime]
        let blogs = map (\(Entity _ x) -> x) blogs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode $
            map (\h -> object $
              ["index" .= htmIndex h,"title" .= htmTitle h,"createtime" .= show $ htmCtime h,"updatetime" .= show $ htmUtime h,"summary" .= htmSum h]
            ) blogs
          where
            ind [] = []
            ind xs = a2o $ map (\x -> [HtmIndex ==. x]) xs

      getBlogListR :: Handler Html
      getBlogListR = do
        [Entity _ h] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","blog"],HtmTyp ==. "home"] []
        let blogHtml = preEscapedToHtml $ htmHtml h -- blogText
        let blogSum = preEscapedtoHtml $ htmSum h
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle h -- blogTitle
          [whamlet|
            <div class=summaryS id=summaryI>
              #{blogSum}
            #{blogHtml}
          |]

      getPageR :: Texts -> Handler Html
      getPageR i = do
        [Entity _ (Htm _ pageText pageTitle_ _ _ _)] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ("page":i),HtmTyp ==. "page"] []
        let pageHtml = preEscapedToHtml $ htmHtml p -- pageText
        let pageSum = preEscapedToHtml $ htmSum p
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle p
          [whamlet|
            <div class=summaryS id=summaryI>
              #{pageSum}
            #{pageHtml}
          |]

      getTxtR :: Texts -> Handler TypedContent
      getTxtR i = do
        [Entity _ (Txt _ txtText content _ _)] <- liftHandlerT $ runDB $ selectList [TxtIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 content) $ return txtText

      getBinR :: Texts -> Handler TypedContent
      getBinR i = do
        [Entity _ (Bin _ binText content _ _)] <- liftHandlerT $ runDB $ selectList [BinIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 content) $ return binText

      postNavR :: Handler TypedContent
      postNavR = do
        navs' <- liftHandlerT $ runDB $ selectList [] [Asc NavOrder]
        let navs = map (\(Entity _ x) -> x) navs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode navs

      mkYesodDispatch "Glob" resourcesGlob
