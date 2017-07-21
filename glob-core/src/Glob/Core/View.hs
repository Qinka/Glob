{-|
Module       : Glob.Core.View
Description  : The view of glob
Copyright    : (C) Qinka 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknow

The view part of the glob.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

module Glob.Core.View
       ( -- * responds
         respond_post
       , respond_resource_t
       , respond_resource_b
       , respond_static
       , -- * reexport module
         module Glob.Core.View.Internal
       , module Glob.Core.View.Query
       ) where

import           Control.Monad.Writer.Lazy
import           Data.Maybe
import           Data.Monoid
import           Glob.Core.Model           (ResT (..))
import           Glob.Core.View.Internal
import           Glob.Import.ByteString    (ByteString)
import qualified Glob.Import.ByteString    as B
import           Glob.Import.Text          (Text)
import qualified Glob.Import.Text          as T
import           Glob.Utils.Handler
import           Glob.View.Query
import           Network.HTTP.Types        (status301)
import           Text.Blaze.Html           (Html, preEscapedToHtml)
import           Yesod.Core
import           Yesod.Core.Handler
import           Yesod.Core.Widget

-- | response the post with ResT and html
respond_post :: (Yesod a, Hamletic a (HandlerT a IO))
                => ResT -- ^ resource index
                -> Html -- ^ html body
                -> HandlerT a IO TypedContent
respond_post res@ResT{..} raw_body = do
  isRaw <- null <$> lookupHeader "GLOBRAW"
  body <- if isRaw then return raw_body
          else defaultLayout $ with_html raw_body res
  respondSource "text/html" $ do
    sendChunkHtml body
    sendFlush

-- | with tags, import tags to js
with_tags :: [Text] -- ^ tags
          -> WidgetT site IO ()
with_tags tags = let tags_j = toJSON tags in toWidget [julius|tags=#{tags_j};|]

-- | with summary, import summary to page when it not Nothing
with_summary :: Maybe Html -- ^ summary html
             -> WidgetT site IO ()
with_summary (Just summary_html) = [whamlet|<summary id=sum>#{summary_html}|]
with_summary _                   = return ()

-- | with whose, import the author to the js
with_whose :: Maybe Text -- ^ author
           -> WidgetT site IO ()
with_whose (Just whose) = let w = show_js whose in toWidget [julius|author=#{w}|]
with_whose _                    = return ()

-- | with html combine the parts to one
with_html :: Html -- ^ the html for main part
          -> ResT -- ^ resource
          -> WidgetT site IO ()
with_html body ResT{..} = do
  setTitle $ toHtml rTitle
  with_summary $ preEscapedToHtml <$> rSummary
  [whamlet|#{body}|]
  with_whose rWhose
  with_tags rTags


-- | respond resource(text)
respond_resource_t :: (Yesod a, Hamletic a (HandlerT a IO))
                      => ResT    -- ^ resource index
                      -> Text    -- ^ text
                      -> HandlerT a IO TypedContent
respond_resource_t ResT{..} text = do
  respondSource (fromMaybe "" $ fmap T.encodeUtf8 rMIME) $ do
    sendChunkText text
    sendFlush

-- | respond resource(binary)
respond_resource_b :: (Yesod a, Hamletic a (HandlerT a IO))
                      => ResT    -- ^ resource index
                      -> ByteString    -- ^ text
                      -> HandlerT a IO TypedContent
respond_resource_b ResT{..} bin = do
  respondSource (fromMaybe "" $ fmap T.encodeUtf8 rMIME) $ do
    sendChunkBS bin
    sendFlush


-- | response the static url
respond_static :: (Yesod a, Hamletic a (HandlerT a IO))
                  => ResT -- ^ index for resource
                  -> Text -- ^ Url
                  -> HandlerT a IO TypedContent
respond_static _ url = redirectWith status301 url
