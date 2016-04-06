




-- src/Glob/MDBS/MySQL.hs

{-# LANGUAGE TemplateHaskell
           , RecordWildCards
           , FlexibleContexts
           , OverloadedStrings
           , TypeFamilies
           , AllowAmbiguousTypes
           #-}

module Glob.MDBS.MySQL
    ( DBBackend
    , runWithPool
    , withConfigAndPool
    , dbKind
    , mdbsRaw
    , shareEntitDef
    , ConnectionPool
    , DbConfig(..)
    , SSLI(..)
    , mkSSLIJust
    ) where

      import Yesod
      import Glob.Foundation.Common
      import Import.TH
      import Data.Aeson
      import Import.Text as T
      import Database.Persist.MySQL
      import Data.Conduit
      import Import.ByteString as B
      import Database.Persist.Sql
      import Data.String(fromString)
      import Import.Monad
      import Database.MySQL.Simple

      type DBBackend = SqlBackend

      runWithPool :: (MonadBaseControl IO m)
                  => SqlPersistT m a -> ConnectionPool -> m a
      runWithPool = runSqlPool

      withConfigAndPool :: (MonadIO m,Applicative m,MonadBaseControl IO m,MonadLogger m)
                        => DbConfig -> (ConnectionPool -> m b) -> m b
      withConfigAndPool c a = let (ci,conThd) = toConInfo c
        in withMySQLPool ci conThd a

      toConInfo :: DbConfig -> (ConnectInfo,Int)
      toConInfo DbConfig{..} = (ci,dbConThd)
        where
          ci = ConnectInfo
            { connectHost = dbAddr
            , connectPort = read dbPort
            , connectUser = dbUser
            , connectPassword = dbPsk
            , connectDatabase = dbName
            , connectOptions = []
            , connectPath = fromMaybe (connectPath defaultConnectInfo) dbPath
            , connectSSL = case dbSSL of
                Nothing -> Nothing
                Just SSLI{..} -> Just SSLInfo
                  { sslKey = dsKey
                  , sslCert = dsCert
                  , sslCA = dsCA
                  , sslCAPath = dsCAPath
                  , sslCiphers = dsCiphers
                  }
            }


      dbKind :: Q Exp
      dbKind = [|"MySQL"|]

      shareEntitDef = [mkPersist sqlSettings,mkMigrate "migrateAll"]

      mdbsRaw :: ( YesodPersist site
                 , YesodPersistBackend site ~ DBBackend
                 )
              => HandlerT site IO TypedContent
      mdbsRaw = do
        fileinfo' <- lookupFile "sql"
        rawWays <- lookupPostParams "raw-way"
        case fileinfo' of
          Just fileinfo -> do
            textt <- sourceToList $ fileSource fileinfo
            let cmd =  b2t $ B.concat textt
            case rawWays of
              "raw-sql":_ -> do
                trt <- liftHandlerT $ runDB $ rawSql' cmd
                rt $ show trt
              "raw-exectue-cols":_ -> do
                c <- liftHandlerT $ runDB $ rawExecuteCount cmd []
                rt $ show c
              _ -> do
                liftHandlerT $ runDB $ rawExecute cmd []
                rt ""
          _ -> invalidArgs ["failed/less and less."]
        where
          rt x = selectRep $ provideRepValue $ rtMsg' "success" x
          rawSql' :: MonadIO m
                  => T.Text -> ReaderT DBBackend m [Single PersistValue]
          rawSql' t = rawSql t []

      data SSLI = SSLI
        { dsKey :: FilePath
        , dsCert :: FilePath
        , dsCA :: FilePath
        , dsCAPath :: FilePath
        , dsCiphers :: String
        }

      mkSSLIJust a b c d e = case (a,b,c,d,e) of
        (Just aa,Just bb,Just cc,Just dd,Just ee) -> Just $ SSLI aa bb cc dd ee
        _ -> Nothing

      data DbConfig = DbConfig
        { dbAddr :: String
        , dbPort :: String
        , dbUser :: String
        , dbPsk :: String
        , dbName :: String
        , dbConThd :: Int
        , dbPath :: Maybe FilePath
        , dbSSL :: Maybe SSLI
        }

      instance FromJSON DbConfig where
        parseJSON (Object v) = DbConfig
          <$> v .: "host"
          <*> v .: "port"
          <*> v .: "usr"
          <*> v .: "psk"
          <*> v .: "name"
          <*> v .: "conThd"
          <*> v .:? "path"
          <*> v .:? "ssl"
      instance FromJSON SSLI where
        parseJSON (Object v) = SSLI
          <$> v .: "key"
          <*> v .: "certificate"
          <*> v .: "ca"
          <*> v .: "capath"
          <*> v .: "ciphers"

      instance ToJSON DbConfig where
        toJSON DbConfig{..} = object $
          [ "host" .= dbAddr
          , "port" .= dbPort
          , "usr" .= dbUser
          , "psk" .= dbPsk
          , "name" .= dbName
          , "conThd" .= dbConThd
          ] ++ p ++ s
          where
            p = case dbPath of
              Nothing -> []
              Just x -> ["path" .= x]
            s = case dbSSL of
              Nothing -> []
              Just x -> ["ssl" .= x]

      instance ToJSON SSLI where
        toJSON SSLI{..} = object
          [ "key" .= dsKey
          , "certificate" .= dsCert
          , "ca" .= dsCA
          , "capath" .= dsCAPath
          , "ciphers" .= dsCiphers
          ]
