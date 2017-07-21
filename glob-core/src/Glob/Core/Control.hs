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
{-# LANGUAGE RecordWildCards       #-}

module Glob.Core.Control
       (
       ) where

import           Glob.Core.Model
import           Glob.Core.View
import           Glob.Import.Text   (Text)
import qualified Glob.Import.Text   as T
import           Glob.Utils.Handler
import           Yesod.Core




-- | get method router
getUrlR :: Controly site
           => [T.Text] -- ^ index
           -> HandlerT site IO TypedContent
getUrlR idx@("@":_) = getQuery idx =<< run_db_default (fetch_res idx)
getUrlR idx = do
  res <- run_db_default $ fetch_res idx
  case rType <$> res of
    Just "port"   -> getPostR           res
    Just "text"   -> getResourceR True  res
    Just "binary" -> getResourceR False res
    Just "static" -> getStaticR         res
    _             -> notFound

-- | put method router
putUrlR :: Controly site
           => [Text] -- ^ index
           -> HandlerT site IO TypedContent
putUrlR ("@":"@nav":_) = putNavR
putUrlR idx = do
  typ <- lookupPostParam "type"
  case typ of
    Just "post"   -> putPostR           idx
    Just "text"   -> putResourceR True  idx
    Just "binary" -> putResourceR False idx
    Just "static" -> putStaticR         idx
    Just "frame"  -> putFrame           idx
    Just "query"  -> putQuery           idx
    _             -> notFound

-- | delete
deleteUrlR :: Controly site
              => [Text] -- ^ index
              -> HandlerT site IO TypedContent
deleteUrlR ("@":"@nav":_) = delNavR
deleteUrlR idx = do
  typ <- lookupPostParam "type"
  db <- case typ of
    Just "post"   -> return "post"
    Just "text"   -> return "resource"
    Just "binary" -> return "resource"
    Just "static" -> return "static"
    Just "query"  -> return "query"
    _             -> notFound
  rt <- tryH.run_db_default $ delete_item idx db
  case rt of
    Left e  -> returnER e
    Right _ -> returnSucc


-- | get post
getPostR :: Controly site
            => Maybe ResT -- ^ index
            -> HandlerT site IO TypedContent
getPostR (Just res@ResT{..}) = do
  html <- run_db_default $ fetch_post res
  case html of
    Just pH -> respond_post res pH
    _       -> notFound
getPostR _ = notFound

putPostR :: Controly site
            => [Text] -- ^ index
            -> HandlerT site IO TypedContent
putPostR idx = do
  unR  <- lookupPostUnResT idx
  html <- T.decodeUtf8 <#> getFile "html"
  putItem unR html updatePost

getResourceR :: Controly site
                => Bool
                -> Maybe ResT
                -> HandlerT site IO TypedContent
getResourceR t (Just res@ResT{..}) = do
  ct <- run_db_default $ fetch_item rest
  case ct of
    Just (Left    text) -> respond_resource_t res text
    Just (Right binary) -> respond_resource_b res bin
    _                   -> notFound
  where
    gmime = fromMaybe "" . (T.encodeUtf8 <$>)
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
    Just u -> respond_static u
    _      -> notFound
getStaticR _ = notFound


putStaticR :: Controly site
               => [Text]
               -> HandlerT site IO  TypedContent
putStaticR idx = do
  unR <- lookupPostUnRest idx
  url <- lookupPostParam "url"
  putItem unR url update_static



putFrame :: Controly site
            => [T.Text]
            -> HandlerT site IO TypedContent
putFrame idx = do
  unR <- lookupPostUnResT idx
  html <- T.decodeUtf8 <#> getFile "html"
  putItem unR html update_frame

getQueryR :: Contronly site
             => [Text]
             -> Maybe ResT
             -> HandlerT site TypedContent
getQueryR idx r =
  case tail idx of
    "~version":"author":_ -> query_version_author
    "~version":"utils":_  -> query_version_utils
    "~version":"core":_   -> query_version_core
    "~version":_          -> query_version
    "~name":_             -> query_name
