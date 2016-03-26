




-- src/Glob/Handler/Post.hs

{-# LANGUAGE OverloadedStrings #-}

module Glob.Handler.Post
    ( postBlogListR
    , postNavR
    , postTagR
    ) where

      import Prelude as P
      import Import.Handler
      import qualified Import.ByteString as B
      import qualified Import.Text as T
      import Glob.Sub.Background.TH(a2o)

      postBlogListR :: Handler TypedContent
      postBlogListR = do
        inde <- lookupPostParams "index"
        page' <- liftHandlerT $ lookupPostParam "page"
        let page = maybe 1 (read.t2s) page'
        split' <- liftHandlerT $ lookupHeader "page-split"
        let split = fmap (read.b2s) split'
        blogs' <- liftHandlerT $ runDB $ selectList ((HtmTyp ==. "blog"):ind inde) [Desc HtmCtime]
        let blogs = map (\(Entity _ x) -> x) blogs'
        selectRep $ provideRepValue $
            sized split page $ map (\h -> object
              [ "index" .= htmIndex h
              , "title" .= htmTitle h
              , "createtime" .= show (htmCtime h)
              , "updatetime" .= show (htmUtime h)
              , "summary" .= htmSum h
              ]
            ) blogs
          where
            ind [] = []
            ind xs = a2o $ map (\x -> [HtmIndex ==. x]) xs
            sized (Just s) p xs = P.take s $ P.drop (p*s-s) xs
            sized _ _ xs = xs


      postNavR :: Handler TypedContent
      postNavR = do
        navs' <- liftHandlerT $ runDB $ selectList [] [Asc NavOrder]
        let navs = map (\(Entity _ x) -> x) navs'
        selectRep $ provideRepValue navs

      postTagR :: Handler TypedContent
      postTagR = do
        tags <- liftHandlerT $ lookupPostParams "tag"
        typs <- liftHandlerT $ lookupPostParams "type"
        infos' <- liftHandlerT $ runDB $ selectList [TagTag <-. tags,TagTyp <-. typs] []
        let infos = map (\(Entity _ x)->x) infos'
        selectRep $ provideRepValue infos
