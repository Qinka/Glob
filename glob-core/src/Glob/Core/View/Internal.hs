{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}

{-|
Module       : Glob.Core.View.Internal
Description  : The internal module for view
Copyright    : (C) Qinka 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknown

The internal party for View, including Hamletic.
-}


module Glob.Core.View.Internal
       ( Hamletic(..)
       , glob_layout
       , glob_error_handler
         -- * layouts
       , layout_bootstrap
       ) where

import           Glob.Core.Model
import           Glob.Import.Text   (Text)
import qualified Glob.Import.Text   as T
import           Glob.Utils.Handler
import           Yesod.Core
import           Yesod.Core.Handler
import           Yesod.Core.Json

-- | Hamtletic
--
--   Limit, and test
class (MonadHandler m, Mongodic a m) => Hamletic a m | m -> a where
  get_title        :: m Text  -- ^ get title
  get_frame_prefix :: m Text  -- ^ get the prefix path of frame
  get_version      :: m Text  -- ^ get the version of blog itself or application
  get_raw          :: m Bool  -- ^ return raw html


-- | layout for layout
type GlobLayout site = (  PageContent (Route site)
                       -> Html -- hd
                       -> Text -- title
                       -> Html -- nav
                       -> Html -- top
                       -> Html -- bottom
                       -> ((Route site -> [(Text, Text)] -> Text) -> Html)
                       )

-- | layout with bootstrap
layout_bootstrap :: Yesod site => GlobLayout site
layout_bootstrap page_content hd title nav top bottom = [hamlet|
  $newline never
  $doctype 5
  <html>
    <head>
      <title> #{pageTitle page_content} - #{title}
      <meta charset=utf-8>
      <meta name=viewport content="width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no">
      #{hd}
      ^{pageHead page_content}
    <body>
      #{nav}
      <div id="container">
        #{top}
        <div id="main-part">
          ^{pageBody page_content}
      #{bottom}
  |]

{-|
The example, or say template for layout

@
layout_xx page_content hd title nav top bottom = [hamlet|
  $newline never
  $doctype 5
  \<html>\
    \<head\>
      \<title\> #{pageTitle page_content} - #{title}
      \<meta charset=utf-8\>
      \<meta name=viewport content="width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no"\>
      #{hd}
      ^{pageHead page_content}
    \<body\>
      #{nav}
      #{top}
      ^{pageBody page_content}
      #{bottom}
  |]
@
-}

-- | the default of glob with Yesod
glob_layout :: (Hamletic a (HandlerT a IO),Yesod a)
            => GlobLayout a        -- ^ the layout for glob
            -> WidgetT a IO ()     -- ^ widget
            -> HandlerT a IO Html  -- ^ return
glob_layout layout w = do
  frame_prefix <- get_frame_prefix
  title        <- get_title
  page_content <- widgetToPageContent w
  htmls <- run_db_default $ do
    top_html    <- fetch_maybe_i fetch_frame [frame_prefix,"top"]
    bottom_html <- fetch_maybe_i fetch_frame [frame_prefix,"bottom"]
    nav_html    <- fetch_maybe_i fetch_frame [frame_prefix,"nav"]
    header      <- fetch_maybe_i fetch_frame [frame_prefix,"header"]
    case (top_html,bottom_html,nav_html,header) of
      (Just top, Just bottom, Just nav, Just hd) -> return $ Right (top,bottom,nav,hd)
      _                                          -> return $ Left "cannot launch frames"
  case htmls of
    Left err -> error err
    Right (top,bottom,nav,hd) -> withUrlRenderer $ layout page_content hd title nav top bottom



-- | handler the error
glob_error_handler :: Yesod site
                      => ErrorResponse -- ^ error
                      -> HandlerT site IO TypedContent
glob_error_handler er = selectRep $ do
  provideJson er
  provideRep $
    defaultLayout [whamlet|
                          <h1> error
                          <p> #{T.show er}
                          |]


