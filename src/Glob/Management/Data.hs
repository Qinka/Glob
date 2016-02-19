




-- src/Glob/Management/Data.hs

{-# LANGUAGE  QuasiQuotes
            , TemplateHaskell
            , OverloadedStrings
            , TypeFamilies
            , RecordWildCards
            #-}

module Glob.Management.Data where

      import Yesod
      import Database.Persist.Postgresql
      import Glob.Config

      data Management = Management
        { cpM :: ConnectionPool
        , configM :: Config
        } 

      mkYesodSubData "Management" $(parseRoutesFile "src/Glob/QQ/management.route")

      data RtMsg a = RtMsg
        { rtStatus :: String
        , rtMsg :: a
        }

      instance ToJSON a =>  ToJSON (RtMsg a) where
        toJSON RtMsg{..} = object
          [ "status" .= rtStatus
          , "msg" .= rtMsg
          ]
