{-|
Module       : Glob.Core.Control
Description  : The view of glob
Copyright    : (C) Qinka 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknow

The control part of the glob.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Glob.Core.Control
       ( Controly(..)
       , getUrlR
       , putUrlR
       , deleteUrlR
       ) where

import           Glob.Core.Control.Internal
import           Glob.Core.Model
import           Glob.Core.View
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString     as B
import           Glob.Import.Text           (Text)
import qualified Glob.Import.Text           as T
import           Glob.Utils.Handler
import           Yesod.Core



-- | get method router
getUrlR :: Controly site
           => [T.Text] -- ^ index
           -> HandlerT site IO TypedContent
getUrlR idx@(".query":_) = getQueryR idx =<< run_db_default (fetch_res idx)
getUrlR idx = do
  res <- run_db_default $ fetch_res idx
  case rType <$> res of
    Just "post"   -> getPostR           res
    Just "text"   -> getResourceR True  res
    Just "binary" -> getResourceR False res
    Just "static" -> getStaticR         res
    _             -> liftIO (print res) >> notFound

-- | put method router
putUrlR :: Controly site
           => [Text] -- ^ index
           -> HandlerT site IO TypedContent
putUrlR (".query":".nav":_) = putNavR
putUrlR idx = do
  typ <- lookupPostParam "type"
  case typ of
    Just "post"   -> putPostR           idx
    Just "text"   -> putResourceR True  idx
    Just "binary" -> putResourceR False idx
    Just "static" -> putStaticR         idx
    Just "frame"  -> putFrameR          idx
    Just "query"  -> putQueryR          idx
    _             -> notFound

-- | delete
deleteUrlR :: Controly site
              => [Text] -- ^ index
              -> HandlerT site IO TypedContent
deleteUrlR (".query":".nav":_) = delNavR
deleteUrlR idx = do
  typ <- lookupPostParam "type"
  db <- case typ of
    Just "post"   -> return "post"
    Just "text"   -> return "resource"
    Just "binary" -> return "resource"
    Just "static" -> return "static"
    Just "query"  -> return "query"
    Just "frame"  -> return "frame"
    _             -> notFound
  rt <- tryH.run_db_default $ delete_item idx db
  case rt of
    Left e  -> return_e_h e
    Right _ -> return_succ


-- | get post
getPostR :: Controly site
            => Maybe ResT -- ^ index
            -> HandlerT site IO TypedContent
getPostR (Just res@ResT{..}) = do
  html <- run_db_default $ fetch_post res
  case html of
    Just pH -> respond_post res pH
    _       -> liftIO (putStrLn "Faile to get") >> notFound
getPostR _ = notFound

putPostR :: Controly site
            => [Text] -- ^ index
            -> HandlerT site IO TypedContent
putPostR idx = do
  unR  <- lookupPostUnResT idx
  html <- T.decodeUtf8 <#> getFile "html"
  putItem unR html update_post

getResourceR :: Controly site
                => Bool
                -> Maybe ResT
                -> HandlerT site IO TypedContent
getResourceR t (Just res@ResT{..}) = do
  ct <- run_db_default $ fetch_item res
  case ct of
    Just (Left    text) -> respond_resource_t res text
    Just (Right binary) -> respond_resource_b res binary
    _                   -> notFound
  where
    fetch_item :: Controly site
                  => ResT
                  -> Action (HandlerT site IO) (Maybe (Either T.Text B.ByteString))
    fetch_item = if t
                 then (Left  <#>) <$> fetch_resource_t
                 else (Right <#>) <$> fetch_resource_b
getResourceR _ _ = notFound


putResourceR :: Controly site
                => Bool
                -> [T.Text]
                -> HandlerT site IO TypedContent
putResourceR t idx = do
  unR  <- lookupPostUnResT idx
  text <- T.decodeUtf8 <#> getFile "text"
  bin  <- getFile "binary"
  if t
    then putItem unR            text  update_resource_t
    else putItem unR (Binary <$> bin) update_resource_b


getStaticR :: Controly site
              => Maybe ResT
              -> HandlerT site IO TypedContent
getStaticR (Just res@ResT{..}) = do
  url <- run_db_default $ fetch_static res
  case url of
    Just u -> respond_static res u
    _      -> notFound
getStaticR _ = notFound


putStaticR :: Controly site
               => [Text]
               -> HandlerT site IO  TypedContent
putStaticR idx = do
  unR <- lookupPostUnResT idx
  url <- lookupPostParam "url"
  putItem unR url update_static



putFrameR :: Controly site
             => [T.Text]
             -> HandlerT site IO TypedContent
putFrameR idx = do
  unR <- lookupPostUnResT idx
  html <- T.decodeUtf8 <#> getFile "html"
  putItem unR html update_frame

getQueryR :: Controly site
             => [Text]
             -> Maybe ResT
             -> HandlerT site IO TypedContent
getQueryR idx r =
  case tail idx of
    ".version":"author":_ -> query_version_author
    ".version":"utils":_  -> query_version_utils
    ".version":"core":_   -> query_version_core
    ".version":_          -> query_version
    ".name":_             -> query_name
    ".buildinfo":_        -> query_build_info
    ".servertime":_       -> query_server_time
    ".nav":_              -> run_db_default fetch_nav >>= query_nav
    ".index":xs           -> run_db_default fetch_res_all >>= query_index (T.unpack $ T.concat xs)
    _ -> run_db_default (fetch_maybe_r fetch_query r)
      >>= (\t -> case t of
              Just text -> query_query text
              _         -> notFound
          )
putQueryR :: Controly site
            => [T.Text]
            -> HandlerT site IO TypedContent
putQueryR idx = do
  unR <- lookupPostUnResT idx
  var <- lookupPostParam "var"
  putItem unR var update_query


putNavR :: Controly site
           => HandlerT site IO TypedContent
putNavR = do
  idx   <- lookupPostParam "label"
  url   <- lookupPostParam "url"
  order <- lookupPostParam "order"
  run_db_default $ update_nav idx url (T.read <$> order)
  return_succ

delNavR :: Controly site
           => HandlerT site IO TypedContent
delNavR = do
  idx <- lookupPostParam "label"
  run_db_default $ delete_nav idx
  return_succ

