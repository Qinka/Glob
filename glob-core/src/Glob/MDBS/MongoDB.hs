




-- src/Glob/MDBS/MongoDB.hs

{-# LANGUAGE CPP
           , TemplateHaskell
           , FlexibleContexts
           , OverloadedStrings
           , TypeFamilies
           , RecordWildCards
           #-}

module Glob.MDBS.MongoDB
#ifdef WithMongoDB
    ( DBBackend
    , runWithPool
    , withConfigAndPool
    , dbKind
    , mdbsRaw
    , shareEntitDef
    , ConnectionPool
    , DbConfig
    ) where
#else
      where
#endif

#ifdef WithMongoDB

      import Yesod
      import Glob.Foundation.Common
      import Import.TH
      import Control.Monad.Logger
      import Control.Monad.IO.Class
      import Control.Monad.Trans.Control
      import Import.Text as T
      import Database.Persist.MongoDB
      import Network
      import Data.Conduit
      import qualified Data.ByteString.Char8 as BC8
      import Database.MongoDB (runCommand1)

      type DBBackend = MongoContext
      runWithPool :: (MonadIO m,MonadBaseControl IO m)
                  => Action m a -> ConnectionPool -> m a
      runWithPool = runMongoDBPoolDef
      withConfigAndPool :: (MonadIO m,Applicative m,MonadBaseControl IO m,MonadLogger m)
                        => DbConfig -> (ConnectionPool -> m b) -> m b
      withConfigAndPool dbc = mdb
        where
          mdb = withMongoDBPool dbN hn sport auth (dbConThd dbc) (dbConThd dbc) defaultConnectionIdleTime
          auth = Just $ MongoAuth (s2t $ dbUser dbc) $ s2t $ dbPsk dbc
          sport = Service $ dbPort dbc
          hn = dbAddr dbc
          dbN = (s2t $ dbName dbc)

      dbKind :: Q Exp
      dbKind = [e|"MongoDB"|]

      shareEntitDef = [mkPersist mongoSetting]
       where
         mongoSetting = mkPersistSettings (ConT ''MongoContext)

      mdbsRaw :: ( YesodPersist site
                 , YesodPersistBackend site ~ DBBackend
                 )
              => HandlerT site IO TypedContent
      mdbsRaw = do
         fileinfo' <- lookupFile "js"
         case fileinfo' of
           Just fileInfo -> do
             text <- sourceToList $ fileSource fileInfo
             let cmd = b2t $ BC8.concat text
             liftHandlerT $ runDB $ runCommand1 cmd
             selectRep $ provideRepType "application/json" $ returnTJson $ rtMsg' "success" ""
           _ -> invalidArgs ["failed/less and less."]

      data DbConfig = DbConfig
        { dbAddr :: String
        , dbPort     :: String
        , dbUser      :: String
        , dbPsk :: String
        , dbName   :: String
        , dbConThd   :: Int
        }

      instance FromJSON DbConfig where
        parseJSON (Object v) = DbConfig
          <$> v .: "host"
          <*> v .: "port"
          <*> v .: "usr"
          <*> v .: "psk"
          <*> v .: "dbName"
          <*> v .: "conThd"
      instance ToJSON DbConfig where
        toJSON DbConfig{..} = object
          [ "host"   .= dbAddr
          , "port"   .= dbPort
          , "usr"    .= dbUser
          , "psk"    .= dbPsk
          , "dbName" .= dbName
          , "conThd" .= dbConThd
          ]


#else
      import Prelude
#endif
