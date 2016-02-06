




--launch.bin/simple/Main/CmdArgs.hs

{-# LANGUAGE DeriveDataTypeable #-}

module Main.CmdArgs
    ( Launch(..)
    , launch
    ) where

      import System.Console.CmdArgs
      import Paths_Glob
      import Data.Version(showVersion)

      data Launch = Launch
        { port :: Maybe String
        , conThd :: Maybe String
        , favPath :: Maybe String
        , siteTitle :: Maybe String
        , certPath ::Maybe String
        , keyPath ::Maybe String
        , tokenEnv :: Maybe String
        , dbPort :: Maybe String
        , dbAddr :: Maybe String
        , dbName :: Maybe String
        , dbPsk :: Maybe String
        , dbUsr :: Maybe String
        , output :: String
        } deriving (Show,Typeable,Data)

      launch :: Launch
      launch = Launch
        { output = "stdout"
            &= help "The way it output config-file."
            &= groupname "I/O"
        , port = def
            &= help "The value of this web site's port."
            &= name "port"
            &= explicit
            &= groupname "Glob Settings"
        , conThd = def
            &= help "The value of this web site's connection-limit between site and database."
            &= name "conthd"
            &= explicit
            &= groupname "Glob Settings"
        , favPath = def
            &= help "The value of this web site's favicon.ico."
            &= name "favicon"
            &= explicit
            &= groupname "Glob Settings"
        , siteTitle = def
            &= help "The value of this web site's title."
            &= name "sitetitle"
            &= explicit
            &= groupname "Glob Settings"
        , certPath = def
            &= help "The value of the website's certificate file's path."
            &= name "certpath"
            &= explicit
            &= groupname "Glob Settings"
        , keyPath = def
            &= help "The value of the website's key file's path."
            &= name "keypath"
            &= explicit
            &= groupname "Glob Settings"
        , tokenEnv = def
            &= help "The environment value of Token."
            &= name "token"
            &= explicit
            &= groupname "Glob Settings"
        , dbPort = def
            &= help "The value of database's port."
            &= name "dbport"
            &= explicit
            &= groupname "Database Settings"
        , dbAddr = def
            &= help "The value of database's addr."
            &= name "dbaddr"
            &= explicit
            &= groupname "Database Settings"
        , dbName = def
            &= help "The value of database's name."
            &= name "dbname"
            &= explicit
            &= groupname "Database Settings"
        , dbPsk  = def
            &= help "The value of database's password."
            &= name "dbpsk"
            &= explicit
            &= groupname "Database Settings"
        , dbUsr  = def
            &= help "The value of database's user."
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
