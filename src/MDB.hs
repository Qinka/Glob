
{-# LANGUAGE CPP
           , FlexibleContexts
           , TemplateHaskell
           , OverloadedStrings
           #-}

module MDB
    ( module MDB
    , ConnectionPool
    ) where


      import Yesod
      import Control.Monad.Logger
      import Glob.Config
      import Glob.Common
      import Language.Haskell.TH
      import Control.Monad.IO.Class
      import Control.Monad.Trans.Control
      import Data.Text
#ifdef WithPostgres
      import Database.Persist.Postgresql
      import Database.Persist.Sql
#endif

#ifdef WithMongoDB
      import Database.Persist.MongoDB
      import Network
#endif


#ifdef WithPostgres
      type DBBackend = SqlBackend
      runWithPool :: (MonadBaseControl IO m)
                  => SqlPersistT m a -> ConnectionPool -> m a
      runWithPool = runSqlPool
#endif

#ifdef WithMongoDB
      type DBBackend = MongoContext
      runWithPool :: (MonadIO m,MonadBaseControl IO m)
                  => Action m a -> ConnectionPool -> m a
      runWithPool = runMongoDBPoolDef
#endif

#ifdef WithPostgres
      withCNPTH = [d|
        withConfigAndPool :: (MonadIO m, Applicative m,MonadBaseControl IO m, MonadLogger m)
                          => Config -> (ConnectionPool -> m b) -> m b
        withConfigAndPool c a = do
          let (constr,conThd) = toConStr $ dbConfig c
          withPostgresqlPool constr conThd a
        |]
#endif

#ifdef WithMongoDB
      withCNPTH = [d|
        withConfigAndPool :: (MonadIO m, Applicative m,MonadBaseControl IO m, MonadLogger m)
                          => Config -> (ConnectionPool -> m b) -> m b
        withConfigAndPool c a = mdb a
          where
            mdb = withMongoDBPool (s2t $ dbName dbc) (hostname dbc) (Service $ dbPort dbc) (Just $ MongoAuth (s2t $ usr dbc) (s2t $ password dbc)) (conThd dbc) (conThd dbc) defaultConnectionIdleTime
            dbc = dbConfig c
        |]

#endif
