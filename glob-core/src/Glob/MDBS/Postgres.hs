




-- src/Glob/MDBS/Postgre.hs

{-# LANGUAGE CPP
           , TemplateHaskell
           , RecordWildCards
           , FlexibleContexts
           , OverloadedStrings
           , TypeFamilies
           , AllowAmbiguousTypes
           #-}

module Glob.MDBS.Postgres
#ifdef WithPostgres
    ( DBBackend
    , runWithPool
    , withConfigAndPool
    , dbKind
    , mdbsRaw
    , shareEntitDef
    , ConnectionPool
    ) where
#else
      where
#endif

#ifdef WithPostgres

      import Yesod
      import Glob.Foundation.Config
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
                        => GlobConfig -> (ConnectionPool -> m b) -> m b
      withConfigAndPool c a = let (constr,conThd) = toConStr $ globDb c
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
#else
      import Prelude
#endif
