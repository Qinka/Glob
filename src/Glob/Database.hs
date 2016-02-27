




-- src/Glob/Database.hs

{-# LANGUAGE OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell
           , FlexibleInstances
           , TypeFamilies
           , GADTs
           , GeneralizedNewtypeDeriving
           #-}

module Glob.Database where

      import Data.Text
      import Data.Text.Encoding(decodeUtf8,encodeUtf8)
      import qualified Data.ByteString as B
      import Data.Time
      import Yesod
      import qualified Data.Aeson as A
      import Database.Persist
      import Database.Persist.TH
      import Database.Persist.Quasi

      instance A.ToJSON B.ByteString where
        toJSON x = A.String $ decodeUtf8 x
      instance A.FromJSON B.ByteString where
        parseJSON (String x) = pure $ encodeUtf8 x

      share [mkPersist sqlSettings,mkMigrate "migrateAll"] $(persistFileWith lowerCaseSettings "src/Glob/QQ/database")
