




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
      import Glob.MDBS

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
