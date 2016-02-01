




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
        |]

      data RtMsg = RtMsg
        { rtStatus :: String
        , rtMsg :: String
        }

      instance ToJSON RtMsg where
        toJSON RtMsg{..} = object
          [ "status" .= rtStatus
          , "msg" .= rtMsg
          ]
