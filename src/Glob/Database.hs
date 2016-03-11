




-- src/Glob/Database.hs

{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell
           , FlexibleInstances
           , TypeFamilies
           , GADTs
           , GeneralizedNewtypeDeriving
           , CPP
           , MultiParamTypeClasses
           #-}

module Glob.Database where

      import Data.Text
      import Data.Text.Encoding(decodeUtf8,encodeUtf8)
      import Yesod
      import MDB
      import Database.Persist.Quasi
      import Language.Haskell.TH.Syntax
      import Data.Time
      import qualified Data.ByteString as B
      import qualified Data.ByteString.Lazy as BL
#ifdef WithMongoDB
      import Database.Persist.MongoDB
#endif
      instance ToJSON B.ByteString where
        toJSON x = String $ decodeUtf8 x
      instance FromJSON B.ByteString where
        parseJSON (String x) = pure $ encodeUtf8 x

#ifdef WithMongoDB
      let mongoSettings = (mkPersistSettings (ConT ''MongoContext)) -- {mpsGeneric = False}
        in share [mkPersist mongoSettings]
          $(persistFileWith lowerCaseSettings "src/QuasiContext/database")
#endif

#ifdef WithPostgres
      share [mkPersist sqlSettings,mkMigrate "migrateAll"]
        $(persistFileWith lowerCaseSettings "src/QuasiContext/database")
#endif
