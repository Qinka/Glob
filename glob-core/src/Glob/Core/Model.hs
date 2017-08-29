{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

{-|
Module       : Glob.Core.Model
Description  : The module for model
Copyright    : (C) Qinka 2017
Maintainer   : me@qinka.pro
License      : GPL3
Stability    : experimenta
Portability  : unknown

The codes for model
-}

module Glob.Core.Model
       ( -- * run database
         run_db
       , run_db_default
       , -- * model
         fetch_frame
       , update_frame
       , fetch_post
       , update_post
       , fetch_resource_b
       , update_resource_b
       , fetch_resource_t
       , update_resource_t
       , fetch_static
       , update_static
       , fetch_query
       , update_query
       , fetch_maybe_i
       , fetch_maybe_r
       , -- ** for navgation
         fetch_nav
       , update_nav
       , delete_nav
       , -- re-export
         module Glob.Core.Model.Internal
       ) where


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Pool
import           Glob.Core.Model.Internal
import           Glob.Core.Model.TH
import           Glob.Import
import           Glob.Import.ByteString      (ByteString (..))
import           Glob.Import.Text            (Text (..))
import           Glob.Utils.Handler
import           Glob.Utils.Handler
import           Text.Blaze.Html             (Html (..))
import qualified Text.Blaze.Html             as TBH

-- | escapted to html
preEscapedToHtml :: Text -> Html
preEscapedToHtml = TBH.preEscapedToHtml

-- | about frame
make_fetch 'preEscapedToHtml "frame" ''Html "html" "frame"
make_update                  "frame" ''Text "html" "frame"

-- | about post
make_fetch 'preEscapedToHtml "post" ''Html "html" "post"
make_update                  "post" ''Text "html" "post"

-- | about text resource
make_fetch 'id "resource_t" ''Text "text" "resource"
make_update    "resource_t" ''Text "text" "resource"

-- | about binary resource
make_fetch 'fromBinary "resource_b" ''ByteString "binary" "resource"
make_update            "resource_b" ''Binary "binary" "resource"

-- | about static
make_fetch 'id "static" ''Text "url" "static"
make_update    "static" ''Text "url" "static"

-- | about query
make_fetch 'id "query" ''Text "var" "query"
make_update    "query" ''Text "var" "query"


-- | fetch maybe index
fetch_maybe_i :: MonadIO m
              => (ResT -> Action m (Maybe a))  -- ^ funcion for action
              -> [Text] -- ^ index
              -> Action m (Maybe a)
fetch_maybe_i mf idx =
  fetch_res idx >>= fetch_maybe_r mf

-- | fetch maybe resource
fetch_maybe_r :: MonadIO m
              => (ResT -> Action m (Maybe a)) -- ^ function for action
              -> Maybe ResT -- ^ index
              -> Action m (Maybe a)
fetch_maybe_r mf (Just r) = mf r
fetch_maybe_r _  _        = return Nothing

-- | fetch the nav
fetch_nav :: (MonadBaseControl IO m, MonadIO m)
             => Action m [Nav]
fetch_nav = do
  cr <- find $ select [] "nav"
  navs <- map doc_to_nav <$> rest cr
  closeCursor cr
  return $ catMaybes navs

-- | update nav
update_nav :: MonadIO m
           => Maybe Text  -- ^ label
           -> Maybe Text  -- ^ url
           -> Maybe Int     -- ^ order
           -> Action m ()
update_nav label url order =
  void $ upsert (select ["label" =: label] "nav") $ catMaybes
  [ Just ("index" =: label)
  , "url"   =@ url
  , "order" =@ order
  ]

-- | delete the nav
delete_nav :: MonadIO m
           => Maybe Text -- ^ label ( if it is Nothing, the all nav item will be delete)
           -> Action m ()
delete_nav label =
    delete $ select (catMaybes ["index" =@ label]) "nav"

-- | run mongo
run_db :: Mongodic site m
       => AccessMode  -- ^ access mode
       -> Database    -- ^ database
       -> Action m a  -- ^ action
       -> m a
run_db am db mf = get_pool >>= \pool ->
  withResource pool $ \p -> do
  (user,pass) <- get_db_u_p
  access p am db $ do
    auth user pass
    mf

-- | run mongo with default
run_db_default  :: Mongodic site m
                => Action m a  -- ^ action
                -> m a
run_db_default mf = do
  am <- get_default_access_mode
  db <- get_default_db
  run_db am db mf

