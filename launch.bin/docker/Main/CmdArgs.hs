




-- launch.bin/docker/Main/CmdArgs.hs

{-# LANGUAGE DeriveDataTypeable #-}

module Main.CmdArgs
    ( runArgs
    , DockerLaunch(..)
    , dockerLaunch
    ) where

      import System.Console.CmdArgs
      import Paths_Glob
      import Data.Version(showVersion)
      import Glob.Config(Config)

      data DockerLaunch = DockerLaunch
        { port :: String
        , conThd :: String
        , favPath :: String
        , siteTitle :: String
        , certPath ::String
        , keyPath ::String
        , dbPort :: String
        , dbAddr :: String
        , dbName :: String
        , dbPsk :: String
        , dbUsr :: String
        } deriving (Show,Typeable,Data)

      dockerLaunch :: DockerLaunch
      dockerLaunch = DockerLaunch
        { port = "GLOB_PORT"
            &= help "The environment value(\'s name) of this web site's port."
            &= name "port"
            &= explicit
            &= groupname "Glob Settings"
        , conThd = "GLOB_CONTHD"
            &= help "The environment value(\'s name) of this web site's connection-limit between site and database."
            &= name "conthd"
            &= explicit
            &= groupname "Glob Settings"
        , favPath = "FAVICON_PATH"
            &= help "The environment value(\'s name) of this web site's favicon.ico."
            &= name "favicon"
            &= explicit
            &= groupname "Glob Settings"
        , siteTitle = "SITE_TITLE"
            &= help "The environment value(\'s name) of this web site's title."
            &= name "sitetitle"
            &= explicit
            &= groupname "Glob Settings"
        , certPath = "CERTIFICATE_PATH"
            &= help "The environment value(\'s name) of the website's certificate file's path."
            &= name "certpath"
            &= explicit
            &= groupname "Glob Settings"
        , keyPath = "KEY_PATH"
            &= help "The environment value(\'s name) of the website's key file's path."
            &= name "keypath"
            &= explicit
            &= groupname "Glob Settings"
        , dbPort = "DB_PORT"
            &= help "The environment value(\'s name) of database's port."
            &= name "dbport"
            &= explicit
            &= groupname "Database Settings"
        , dbAddr = "DB_ADDR"
            &= help "The environment value(\'s name) of database's addr."
            &= name "dbaddr"
            &= explicit
            &= groupname "Database Settings"
        , dbName = "DB_NAME"
            &= help "The environment value(\'s name) of database's name."
            &= name "dbname"
            &= explicit
            &= groupname "Database Settings"
        , dbPsk  = "DB_PSK"
            &= help "The environment value(\'s name) of database's password."
            &= name "dbpsk"
            &= explicit
            &= groupname "Database Settings"
        , dbUsr  = "DB_USR"
            &= help "The environment value(\'s name) of database's user."
            &= name "dbusr"
            &= explicit
            &= groupname "Database Settings"
        }
        &= program "launch.docker"
        &= details
          [ "This is a launcher of glob, when used docker."
          ]
        &= summary ("Glob's launch for docker "++showVersion version++",(C) Qinka 2016")
        &= verbosity

      runArgs :: (DockerLaunch -> IO Config) -> IO Config
      runArgs = (cmdArgs dockerLaunch >>=)
