




-- src/Glob/Config.hs

{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           , RecordWildCards
           #-}

module Glob.Config
    ( toConStr
    , Config(..)
    , DbConfig(..)
    ) where
      import Data.String(fromString)
      import Data.ByteString.Lazy(toStrict)
      import qualified Data.ByteString.Internal as B
      import Data.Aeson

      -- 全局设置
      data Config = Config
        { port :: Int
        , dbConfig :: DbConfig
        , staticPath :: FilePath
        , siteTitle :: String
        , certPath  :: FilePath
        , keyPath   :: FilePath
        , tokenEnv  :: String
        }

      instance FromJSON Config where
        parseJSON (Object v) = Config
          <$> v .: "port"
          <*> v .: "dbConfig"
          <*> v .: "static-path"
          <*> v .: "site-title"
          <*> v .: "certificate-file"
          <*> v .: "key-file"
          <*> v .: "token-value-name"

      instance ToJSON Config where
        toJSON Config{..} = object
          [ "port" .= port
          , "dbConfig" .= dbConfig
          , "static-path" .= staticPath
          , "site-title" .= siteTitle
          , "certificate-file" .= certPath
          , "key-file" .= keyPath
          , "token-value-name" .= tokenEnv
          ]

      -- 数据库设置
      data DbConfig = DbConfig
        { hostname :: String
        , dbPort     :: String
        , usr      :: String
        , password :: String
        , dbName   :: String
        , conThd   :: Int
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
          [ "host"   .= hostname
          , "port"   .= dbPort
          , "usr"    .= usr
          , "psk"    .= password
          , "dbName" .= dbName
          , "conThd" .= conThd
          ]

      -- 转换成连接字符串
      toConStr :: DbConfig -> (B.ByteString,Int)
      toConStr DbConfig{..} = (str,conThd)
        where
          str = toStrict $
            fromString $ "host=\'"++hostname
                      ++ "\' port=\'"++dbPort
                      ++ "\' user=\'"++usr
                      ++ "\' password=\'"++password
                      ++ "\' dbname=\'"++dbName
                      ++ "\'"
