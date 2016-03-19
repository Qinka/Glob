




-- src/Glob/Handler.hs

{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , QuasiQuotes
           , TypeFamilies
           , GADTs
           , FlexibleInstances
           , ViewPatterns
           #-}

module Glob.Handler
    ( getQueryR
    , getHomeR
    , getTxtR
    , getBinR
    , getBlogItemR
    , getBlogListR
    , getPageR
    , postBlogListR
    , postNavR
    ) where

      import Prelude as P
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
      import Text.Julius(rawJS,juliusFile)
      import Data.Maybe(fromMaybe)
      import MDB(dbKind)

      getQueryR :: Texts -> Handler Text
      getQueryR i =
        case i of
          "version":_ -> return $ pack $ showVersion version
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
        [Entity _ h] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","main"],HtmTyp ==. "home"] []
        let mainHtml = preEscapedToHtml $ htmHtml h
        let sumHtml = preEscapedToHtml $ fromMaybe "" $ htmSum h
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle h
          $(whamletFile "src/QuasiContext/Home.hamlet")

      getBlogItemR :: Text -> Texts -> Handler Html
      getBlogItemR t i= do
        let bi = ws2s ("blog":i)
        [Entity _ b] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. bi,HtmCtime ==. read (t2s t),HtmTyp ==. "blog"] []
        let blogHtml = preEscapedToHtml $ htmHtml b -- blogText
        let blogSum = preEscapedToHtml $ fromMaybe "" $ htmSum b
        let biH = rawJS bi
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle b -- blogTitle
          toWidget $(juliusFile "src/QuasiContext/BlogItem.julius")
          $(whamletFile "src/QuasiContext/BlogItem.hamlet")

      postBlogListR :: Handler TypedContent
      postBlogListR = do
        inde <- lookupPostParams "index"
        page' <- liftHandlerT $ lookupPostParam "page"
        let page = maybe 1 (read.t2s) page'
        split' <- liftHandlerT $ lookupHeader "page-split"
        let split = fmap (read.b2s) split'
        blogs' <- liftHandlerT $ runDB $ selectList ((HtmTyp ==. "blog"):ind inde) [Desc HtmCtime]
        let blogs = map (\(Entity _ x) -> x) blogs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode $
            sized split page $ map (\h -> object
              ["index" .= htmIndex h,"title" .= htmTitle h,"createtime" .= show (htmCtime h),"updatetime" .= show (htmUtime h),"summary" .= htmSum h]
            ) blogs
          where
            ind [] = []
            ind xs = a2o $ map (\x -> [HtmIndex ==. x]) xs
            sized (Just s) p xs = P.take s $ P.drop (p*s-s)  xs
            sized _ _ xs = xs

      getBlogListR :: Handler Html
      getBlogListR = do
        [Entity _ h] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ["home","page","blog"],HtmTyp ==. "home"] []
        let blogHtml = preEscapedToHtml $ htmHtml h -- blogText
        let blogSum = preEscapedToHtml $ fromMaybe "" $ htmSum h
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle h -- blogTitle
          $(whamletFile "src/QuasiContext/BlogList.hamlet")

      getPageR :: Texts -> Handler Html
      getPageR i = do
        [Entity _ p] <- liftHandlerT $ runDB $
          selectList [HtmIndex ==. ws2s ("page":i),HtmTyp ==. "page"] []
        let pageHtml = preEscapedToHtml $ htmHtml p -- pageText
        let pageSum = preEscapedToHtml $ fromMaybe "" $ htmSum p
        defaultLayout $ do
          setTitle $ toHtml $ htmTitle p
          $(whamletFile "src/QuasiContext/Page.hamlet")

      getTxtR :: Texts -> Handler TypedContent
      getTxtR i = do
        [Entity _ t] <- liftHandlerT $ runDB $ selectList [TxtIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 $ txtContent t) $ return $ txtTxt t

      getBinR :: Texts -> Handler TypedContent
      getBinR i = do
        [Entity _ b] <- liftHandlerT $ runDB $ selectList [BinIndex ==. ws2s i] []
        selectRep $ provideRepType (encodeUtf8 $ binContent b) $ return $ binBin b

      postNavR :: Handler TypedContent
      postNavR = do
        navs' <- liftHandlerT $ runDB $ selectList [] [Asc NavOrder]
        let navs = map (\(Entity _ x) -> x) navs'
        selectRep $ provideRepType "application/json" $
          return $ decodeUtf8 $ BL.toStrict $ encode navs
