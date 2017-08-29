{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

{-|
Module       : Glob.Core.Control.Internal
Description  : The view of glob
Copyright    : (C) Qinka 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknow

The control part of the glob.
-}


module Glob.Core.Control.Internal
       ( lookupPostUnResT
       , getFile
       , getField
       , putItem
       , return_succ
       , Controly(..)
       ) where

import           Data.Conduit
import           Glob.Core.Model
import           Glob.Core.View
import qualified Glob.Import.ByteString as B
import           Glob.Import.Text       (Text)
import qualified Glob.Import.Text       as T
import           Glob.Utils.Handler
import           Yesod.Core



-- | for control
class (Mongodic site (HandlerT site IO), MonadHandler (HandlerT site IO), Hamletic site (HandlerT site IO), Yesod site) => Controly site

-- | lookup the undefined index
lookupPostUnResT :: Controly site
                 => [Text] -- ^ index
                 -> HandlerT site IO (Maybe ResT)
lookupPostUnResT idx = do
  ty <- lookupPostParam  "type"
  ct <- lookupPostParam  "create-time"
  ut <- lookupPostParam  "update-time"
  ti <- lookupPostParam  "title"
  su <- getField         "summary"
  wh <- lookupPostParam  "whose"
  mi <- lookupPostParam  "mime"
  tg <- lookupPostParams "tag"
  ts <- T.words <#> lookupPostParams "tags"
  return $ case (ty,ct,ut,ti) of
    (Just t,Just c,Just u,Just i) -> Just . ResT
      idx undefined t (T.read c) (T.read u) i su wh mi . concat $ tg:ts
    _ -> Nothing

-- | get the uploaded file in ByteString
getFilesBS :: (MonadResource m, MonadHandler m)
           => [FileInfo] -- ^ file infos
           -> m (Maybe B.ByteString)
getFilesBS [] = return Nothing
getFilesBS xs = Just. B.concat.concat <$>
  mapM (sourceToList.fileSource) xs

-- | get the file via file name
getFile :: (MonadResource m, MonadHandler m)
        => T.Text -- ^ file name (field name)
        -> m (Maybe B.ByteString)
getFile file = getFilesBS =<< lookupFiles file

-- | get the field text
getField :: (MonadResource m, MonadHandler m)
         => T.Text -- ^ field name
         -> m (Maybe T.Text)
getField fieled = do
  su <- T.decodeUtf8 <#> getFile fieled
  case su of
    Just s -> return su
    _      -> lookupPostParam fieled

-- | for upload the items
putItem :: (Controly site, Val a)
        => Maybe ResT -- ^ resource index (maybe)
        -> Maybe a    -- ^ item (maybe)
        -> (a -> ResT -> Action (HandlerT site IO) ()) -- ^ upload action for database
        -> HandlerT site IO TypedContent
putItem unR item f = case (unR,item) of
  (Just r,Just i) -> do
    rt <- tryH.run_db_default $ f i r
    return_i rt
  _ ->  invalidArgs [" args failed"]
  where
    return_i (Left e)  = return_e_h e
    return_i (Right _) = respondSource "" $ sendChunkText "success"

-- | return sucecess
return_succ :: HandlerT site IO TypedContent
return_succ = respondSource "text/plain" $ sendChunkText "success"
