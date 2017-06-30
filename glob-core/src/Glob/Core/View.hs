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
       (
       ) where

import           Control.Monad.Writer.Lazy
import           Data.Monoid
import           Glob.Core.Model
import           Glob.Core.View.Internal
import qualified Glob.Import.Text          as T
import           Glob.Utils.Handler
import           Text.Blaze.Html           (Html, preEscapedToHtml)
import           Yesod.Core
import           Yesod.Core.Handler
import           Yesod.Core.Widget

response_post :: (Yesod a, Hamletic a (HandlerT a IO))
                 => ResT
                 -> HandlerT a IO TypedContent
response_post res@ResT{..} = do
  isRaw <- null <$> lookupHeader "GLOBRAW"
  raw_body_maybe <- run_db_default $ fetch_post res
  case raw_body_maybe of
    Just raw_body -> do
      body <- if isRaw then return raw_body
              else defaultLayout $ with_html raw_body res
      respondSource "text/html" $ do
        sendChunkHtml body
        sendFlush
    _             -> error "Can not found datas"



-- | with tags, import tags to js
with_tags :: [T.Text] -- ^ tags
          -> WidgetT site IO ()
with_tags tags = let tags_j = toJSON tags in toWidget [julius|tags=#{tags_j};|]

-- | with summary, import summary to page when it not Nothing
with_summary :: Maybe Html -- ^ summary html
             -> WidgetT site IO ()
with_summary (Just summary_html) = [whamlet|<summary id=sum>#{summary_html}|]
with_summary _                   = return ()

-- | with whose, import the author to the js
with_whose :: Maybe T.Text -- ^ author
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
