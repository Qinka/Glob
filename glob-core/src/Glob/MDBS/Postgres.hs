




-- src/Glob/MDBS/Postgre.hs

{-# LANGUAGE TemplateHaskell
           , RecordWildCards
           , FlexibleContexts
           , OverloadedStrings
           , TypeFamilies
           , AllowAmbiguousTypes
           #-}

module Glob.MDBS.Postgres
    ( DBBackend
    , runWithPool
    , withConfigAndPool
    , dbKind
    , mdbsRaw
    , shareEntitDef
    , ConnectionPool
    , DbConfig(..)
    ) where

      import Yesod
      import Glob.Foundation.Common
      import Import.TH
      import Import.Text as T
      import Import.ByteString as B
      import Database.Persist.Postgresql
      import Database.Persist.Sql
      import Data.String(fromString)
      import Import.Monad
      import Data.Conduit

      type DBBackend = SqlBackend
      runWithPool :: (MonadBaseControl IO m)
                  => SqlPersistT m a -> ConnectionPool -> m a
      runWithPool = runSqlPool

      withConfigAndPool :: (MonadIO m,Applicative m,MonadBaseControl IO m, MonadLogger m)
                        => DbConfig -> (ConnectionPool -> m b) -> m b
      withConfigAndPool c a = let (constr,conThd) = toConStr c
        in withPostgresqlPool constr conThd a

      toConStr :: DbConfig -> (ByteString,Int)
      toConStr DbConfig{..} = (str,dbConThd)
        where
          str = toStrictBS $
            fromString $ "host=\'"++ dbAddr
                      ++ "\' port=\'"++ dbPort
                      ++ "\' user=\'"++ dbUser
                      ++ "\' password=\'"++ dbPsk
                      ++ "\' dbname=\'"++ dbName
                      ++ "\'"
      dbKind :: Q Exp
      dbKind = [e|"PostgreSQL"|]

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
            text <- sourceToList $ fileSource fileinfo
            let cmd = b2t $ B.concat text
            case rawWays of
              "raw-sql":_ -> do
                trt <- liftHandlerT $ runDB $ rawSql' cmd
                rt $ show trt
              "raw-execute-cols":_ -> do
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
          <*> v .: "name"
          <*> v .: "conThd"
      instance ToJSON DbConfig where
        toJSON DbConfig{..} = object
          [ "host"   .= dbAddr
          , "port"   .= dbPort
          , "usr"    .= dbUser
          , "psk"    .= dbPsk
          , "name" .= dbName
          , "conThd" .= dbConThd
          ]
