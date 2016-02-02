




-- lib/Glob/Management/Data.hs
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



      data Management = Management ConnectionPool Config

      mkYesodSubData "Management" [parseRoutes|
        /html UphtmlR POST
        /txt UptxtR POST
        /nav UpnavR POST
        /bin UpbinR POST
        /qry UpqryR POST
        /del DelR   POST
        /list ListR POST
        |]

      data RtMsg a = RtMsg
        { rtStatus :: String
        , rtMsg :: a
        }

      instance ToJSON a =>  ToJSON (RtMsg a) where
        toJSON RtMsg{..} = object
          [ "status" .= rtStatus
          , "msg" .= rtMsg
          ]
