




-- src/Glob/Foundation/Config.hs

{-# LANGUAGE RecordWildCards
           , OverloadedStrings
           #-}

module Glob.Foundation.Config
    ( GlobConfig(..)
    , DbConfig(..)
    ) where

      import Import.ByteString as B
      import Data.Aeson
      import Data.String

      data GlobConfig = GlobConfig
        { globPort :: Int
        , globDb :: DbConfig
        , globSiteTitle :: String
        , globTokenEnv :: String
        , globStaticPath :: FilePath
        , globKeyPath :: FilePath
        , globCertPath :: FilePath
        }
      instance ToJSON GlobConfig where
        toJSON GlobConfig{..} = object
          [ "port" .= globPort
          , "db" .= globDb
          , "title" .= globSiteTitle
          , "token-env" .= globTokenEnv
          , "static-path" .= globStaticPath
          , "key-file" .= globKeyPath
          , "certificate-file" .= globCertPath
          ]

      instance FromJSON GlobConfig where
        parseJSON (Object v) = GlobConfig
          <$> v .: "port"
          <*> v .: "db"
          <*> v .: "title"
          <*> v .: "token-env"
          <*> v .: "static-path"
          <*> v .: "key-file"
          <*> v .: "certificate-file"

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
