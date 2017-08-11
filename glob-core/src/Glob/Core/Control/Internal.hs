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

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

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


getFilesBS :: (MonadResource a,MonadHandler a)
              => [FileInfo]
              -> a (Maybe B.ByteString)
getFilesBS [] = return Nothing
getFilesBS xs = Just. B.concat.concat <$>
  mapM (sourceToList.fileSource) xs
getFile :: (MonadResource a,MonadHandler a)
           => T.Text
           -> a (Maybe B.ByteString)
getFile file = getFilesBS =<< lookupFiles file
getField :: (MonadResource a,MonadHandler a)
            => T.Text
            -> a (Maybe T.Text)
getField fieled = do
  su <- T.decodeUtf8 <#> getFile fieled
  case su of
    Just s -> return su
    _      -> lookupPostParam fieled

putItem :: (Controly site, Val a)
           => Maybe ResT
           -> Maybe a
           -> (a -> ResT -> Action (HandlerT site IO) ())
           -> HandlerT site IO TypedContent
putItem unR item f = case (unR,item) of
  (Just r,Just i) -> do
    rt <- tryH.run_db_default $ f i r
    return_i rt
  _ ->  invalidArgs [" args failed"]
  where
    return_i (Left e)  = return_e_h e
    return_i (Right _) = respondSource "" $ sendChunkText "success"


return_succ :: HandlerT site IO TypedContent
return_succ = respondSource "text/plain" $ sendChunkText "success"
