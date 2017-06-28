{-|
Model         : Glob.Core.Model.Internal
Description   : The basic methods for model and types
Copyright     : (C) Qinka 2017
License       : GPL3
Maintainer    : me@qinka.pro
Stability     : experimental
Portability   : unknown

The basic method and type for model in MVC
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Glob.Core.Model.Internal
       ( -- * navigation bar
         Nav(..)
       , nav_to_doc
       , doc_to_nav
       , -- * the resource
         ResT
       , res_to_doc
       , doc_to_res
       , -- * about mongoDB
         Mongodic(..)
       , defaultAM
       , defaultDB
       , fetch_context
       , fetch_res
       , update_context
       , update_item
       , update_res
       , delete_context
       , delete_item
       , delete_res
       , delete_context_maybe
       ) where

import           Control.Monad.IO.Class
import           Data.Pool
import           Database.MongoDB       as Mongo
import           Glob.Import
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import           Glob.Utils.Handler


-- | ConnectionPool
type ConnectionPool = Pool Pipe

-- | model for navigation bar
data Nav = Nav
           { navUrl   :: T.Text -- ^ The url of the link
           , navLabel :: T.Text -- ^ The label of the link
           , navOrder :: Int    -- ^ The order of the link
           }

-- instance eq, order for nav
instance Eq Nav where
  n1 == n2 = navOrder n1 == navOrder n2
instance Ord Nav where
 compare n1 n2 = compare (navOrder n1) (navOrder n2)

-- |transfor between nav and document
nav_to_doc :: Nav -> Document
nav_to_doc Nav{..} =
  [ "index"   =: navLabel
  , "url"   =: navUrl
  , "order" =: navOrder
  ]
doc_to_nav :: Document -> Maybe Nav
doc_to_nav doc = Nav
  <$> doc !? "url"
  <*> doc !? "index"
  <*> doc !? "order"

-- | instance Json(to)
instance ToJSON Nav where
  toJSON Nav{..} = object
    [ "label" .= navLabel
    , "url" .= navUrl
    , "order" .= navOrder
    ]

-- | the resource type for item
data ResT = ResT
            { rIndex   :: [T.Text]      -- ^ the path of the url
            , rRes     :: ObjectId      -- ^ the id of the object in the db
            , rType    :: T.Text        -- ^ the type of resource
            , rCTime   :: UTCTime       -- ^ the time when it created
            , rUTime   :: UTCTime       -- ^ the time when it updated
            , rTitle   :: T.Text        -- ^ the title
            , rSummary :: Maybe T.Text  -- ^ the summary of result
            , rWhose   :: Maybe T.Text  -- ^ the own of the result
            , rMIME    :: Maybe T.Text  -- ^ the MIME type of the result
            , rTags    :: [T.Text]      -- ^ the tags for result
            }
          deriving (Eq,Show)

-- | transform between res document
res_to_doc :: ResT -> Document
res_to_doc ResT{..} =
  [ "index"       =: rIndex
  , "res"         =: rRes
  , "type"        =: rType
  , "create-time" =: rCTime
  , "update-time" =: rUTime
  , "title"       =: rTitle
  , "summary"     =: rSummary
  , "whose"       =: rWhose
  , "mime"        =: rMIME
  , "tags"        =: rTags
  ]



-- | transform between res document
doc_to_res :: Document -> Maybe ResT
doc_to_res doc = ResT
  <$>       doc !? "index"
  <*>       doc !? "res"
  <*>       doc !? "type"
  <*>       doc !? "create-time"
  <*>       doc !? "update-time"
  <*>       doc !? "title"
  <*> Just (doc !? "summary")
  <*> Just (doc !? "whose")
  <*> Just (doc !? "mime")
  <*>  m2l (doc !? "tags")
  where
    m2l (Just xs) = Just xs
    m2l _         = Just []

instance ToJSON ResT where
  toJSON ResT{..} = object
    [ "index"       .= rIndex
    , "type"        .= rType
    , "create-time" .= rCTime
    , "update-time" .= rUTime
    , "title"       .= rTitle
    , "summary"     .= rSummary
    , "whose"       .= rWhose
    , "mime"        .= rMIME
    , "tags"        .= rTags
    ]


-- | the type-class which means mongoDB available.
class Mongodic a where
  get_db_connect_pool     :: a -> ConnectionPool   -- ^ get the connection pool
  get_default_access_mode :: a -> AccessMode       -- ^ get the accedd mode
  get_default_access_mode =  defaultAM
  get_default_db          :: a -> Database         -- ^ get the default database
  get_default_db          =  defaultDB
  get_db_u_p              :: a -> (T.Text,T.Text)  -- ^ get the user and pass

-- | default access mode
defaultAM _ = master
-- | default database
defaultDB _ = "master"

-- | fetch context
fetch_context :: (MonadIO m,Val a)
                 => T.Text    -- ^ field name
                 -> ResT      -- ^ resource index
                 -> T.Text    -- ^ collection
                 -> Action m (Maybe a) -- ^ result
fetch_context field ResT{..} =  ((!? field) <%>).findOne.select ["_id" =: rRes]

-- | fetch resource index
fetch_res :: MonadIO m
             => [T.Text]
             -> Action m (Maybe ResT)
fetch_res index = (doc_to_res <%>) . findOne $ select ["index" =: index] "index"

-- | update context
update_context :: (MonadIO m, Val a)
                  => T.Text            -- ^ Collection
                  -> Maybe ObjectId    -- ^ obj id of item
                  -> T.Text            -- ^ field name
                  -> a
                  -> Action m ObjectId -- ^ return id
update_context c oid field v = case oid of
  Just i -> upsert (select ["_id" =: i] c) [field =: v] >> return i
  _      -> (\(ObjId i) -> i) <$> insert c [field =: v]

-- | the update for item
update_item :: (MonadIO m, Val a)
               => T.Text   -- ^ type, or say collection
               -> T.Text   -- ^ field name
               -> a        -- ^ item
               -> ResT     -- ^ ``undefined'' ResT
               -> Action m ()
update_item t f v uR = do
  let index = rIndex uR
  res <- fetch_res index
  rr <- if (rType <$> res) /= Just t
        then delete_context_maybe res t >> return Nothing
        else return $ rRes <$> res
  rO <- update_context t rr f v
  update_res (uR {rRes = rO})

-- | the update for resource
update_res :: MonadIO m
              => ResT     -- ^ the index
              -> Action m ()
update_res res@ResT{..} =
  upsert (select ["index" =: rIndex] "index") $ res_to_doc res

-- | delete the context
delete_context :: MonadIO m
                  => ResT           -- ^ index
                  -> T.Text         -- ^ collection
                  -> Action m ()
delete_context ResT{..} c =
  delete $ select ["_id" =: rRes] c


-- | delete resource
delete_res :: MonadIO m
              => ResT        -- ^ index
              -> Action m ()
delete_res ResT{..} =
  delete $ select ["index" =: rIndex] "index"

-- | delete the resouce in maybe
delete_context_maybe :: MonadIO m
                    => Maybe ResT -- ^ index
                    -> T.Text     -- ^ field
                    -> Action m ()
delete_context_maybe (Just r) = delete_context r
delete_context_maybe _        = \_ -> return ()

-- | delete item
delete_item :: MonadIO m
               => [T.Text] -- ^ url
               -> T.Text   -- ^ collection
               -> Action m ()
delete_item index c = fetch_res index >>=
  (\res -> case res of
      Just r -> delete_context r c >> delete_res r
      _      -> return ())
