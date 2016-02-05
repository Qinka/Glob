




-- src/Glob/Management.hs
{-# LANGUAGE  QuasiQuotes
            , TemplateHaskell
            , OverloadedStrings
            , TypeFamilies
            , GADTs
            , FlexibleInstances
            , FlexibleContexts
            , GeneralizedNewtypeDeriving
            , DeriveGeneric
            , MultiParamTypeClasses
            #-}


module Glob.Management
    ( module Glob.Management
    , module Glob.Management.Data
    ) where


      import Yesod
      import Glob.Database
      import Glob.Common
      import Glob.Management.Data
      import Glob.Management.TH
      import Data.Conduit
      import Data.Aeson
      import Database.Persist.Postgresql
      import Data.ByteString.Char8 (unpack)
      import Data.ByteString.Lazy(toStrict)
      import Data.Text.Encoding(decodeUtf8)

      instance YesodPersist Management where
        type YesodPersistBackend Management = SqlBackend
        runDB a = do
          Management p _ <- getYesod
          runSqlPool a p

      plItem :: ( Yesod master
                , PersistQuery (PersistEntityBackend v)
                , PersistEntity v
                , ToJSON v
                , PersistEntityBackend v ~ SqlBackend
                )
             => [Filter v]
             -> HandlerT Management (HandlerT master IO) TypedContent
      plItem fil = do
        da <- liftHandlerT $ runDB $ selectList fil []
        selectRep $ provideRepType "application/json" $
          returnTJson $ RtMsg "success" $ ent da

      pdItem :: ( Yesod master
                , PersistQuery (PersistEntityBackend v)
                , PersistEntity v
                , ToJSON v
                , PersistEntityBackend v ~ SqlBackend
                )
             => [Filter v]
             -> HandlerT Management (HandlerT master IO) TypedContent
      pdItem fil = do
        liftHandlerT $ runDB $ deleteWhere fil
        selectRep $ provideRepType "application/json" $
          returnTJson $ rtMsg' "success" ""

      postListR :: Yesod master
               => HandlerT Management (HandlerT master IO) TypedContent
      postListR = do
        index <- lookupPostParams "index"
        typ <- lookupPostParams "type"
        case typ of
          ("html":_) -> plItem $ $(mkPLFunc "HtmIndex") index
          ("nav" :_) -> plItem $ $(mkPLFunc "NavLabel") index
          ("txt" :_) -> plItem $ $(mkPLFunc "TxtIndex") index
          ("bin" :_) -> plItem $ $(mkPLFunc "BinIndex") index
          ("qry" :_) -> plItem $ $(mkPLFunc "QryIndex") index
          _ -> selectRep $ provideRepType "application/json" $
            returnTJson $ rtMsg' "failed" "invalid args"


      rtMsg' :: String -> String -> RtMsg String
      rtMsg' = RtMsg

      postDelR :: Yesod master
               => HandlerT Management (HandlerT master IO) TypedContent
      postDelR = do
        index <- lookupPostParams "index"
        typ <- lookupPostParams "type"
        case typ of
          ("html":_) -> pdItem $ $(mkPLFunc "HtmIndex") index
          ("txt":_)  -> pdItem $ $(mkPLFunc "NavLabel") index
          ("nav":_)  -> pdItem $ $(mkPLFunc "TxtIndex") index
          ("bin":_)  -> pdItem $ $(mkPLFunc "BinIndex") index
          ("qry":_)  -> pdItem $ $(mkPLFunc "QryIndex") index
          _ -> selectRep $ provideRepType "application/json" $
            returnTJson $ rtMsg' "failed" "Invalid Args"





      postUphtmlR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUphtmlR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "html"
        let fileinfo = case fileinfo' of {(Just x)-> [x];_ -> []}
        title <- lookupPostParams "title"
        typ <- lookupPostParams "type"
        day <- lookupHeaders "Day"
        if any null [index,title,typ] || any null [day] || null fileinfo
          then selectRep $ provideRepType "application/json" $
            returnTJson $ rtMsg' "failed" "less and less."
          else do
            liftHandlerT $ runDB $ deleteWhere [HtmIndex ==. head index]
            text <- sourceToList $ fileSource $ head fileinfo
            liftHandlerT $ runDB $ insert $ Htm
              (head index)
              (decodeUtf8 $ head text)
              (head title)
              (head typ)
              (read $ unpack $ head day)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""

      postUptxtR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUptxtR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "txt"
        let fileinfo = case fileinfo' of {(Just x)-> [x];_ -> []}
        typ <- lookupPostParams "type"
        if any null [index,typ] ||  null fileinfo
          then selectRep $ provideRepType "application/json" $
            returnTJson $ rtMsg' "failed" "less and less."
          else do
            liftHandlerT $ runDB $ deleteWhere [TxtIndex ==. head index]
            text <- sourceToList $ fileSource $ head fileinfo
            liftHandlerT $ runDB $ insert $ Txt
              (head index)
              (decodeUtf8 $ head text)
              (head typ)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""

      postUpnavR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpnavR = do
        label <- lookupPostParams "label"
        order <- lookupPostParams "order"
        ref <- lookupPostParams "ref"
        if any null [label,order,ref]
          then selectRep $ provideRepType "application/json" $
            returnTJson $ rtMsg' "failed" "less and less."
          else do
            liftHandlerT $ runDB $ deleteWhere [NavLabel ==. head label]
            liftHandlerT $ runDB $ insert $ Nav
              (head label)
              (Just $ read $ read $ show $ head order)
              (head ref)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""

      postUpbinR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpbinR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "bin"
        let fileinfo = case fileinfo' of {(Just x)-> [x];_ -> []}
        typ <- lookupPostParams "type"
        if any null [index] || null fileinfo
          then selectRep $ provideRepType "application/json" $
            returnTJson $ rtMsg' "failed" "less and less."
          else do
            liftHandlerT $ runDB $ deleteWhere [BinIndex ==. head index]
            text <- sourceToList $ fileSource $ head fileinfo
            liftHandlerT $ runDB $ insert $ Bin
              (head index)
              (head text)
              (head typ)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""

      postUpqryR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpqryR = do
        index <- lookupPostParams "index"
        txt <- lookupPostParams "txt"
        if any null [index,txt]
          then selectRep $ provideRepType "application/json" $
            returnTJson $ rtMsg' "failed" "less and less."
          else do

            liftHandlerT $ runDB $ deleteWhere [QryIndex ==. head index]
            liftHandlerT $ runDB $ insert $ Qry
              (head index)
              (head txt)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""


      instance Yesod master => YesodSubDispatch  Management (HandlerT master IO) where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesManagement)
