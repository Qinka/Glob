




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
      import Database.Persist.Postgresql
      import Data.ByteString.Char8 (unpack)
      import Data.Text.Encoding(decodeUtf8)
      import qualified Data.ByteString.Char8 as BC8

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
          _ -> invalidArgs ["failed/less and less"]

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
          _ -> invalidArgs ["failed/less and less"]

      postUphtmlR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUphtmlR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "html"
        let fileinfo = toList fileinfo'
        title <- lookupPostParams "title"
        typ <- lookupPostParams "type"
        time <- lookupHeaders "UTCTime"
        if any null [index,title,typ] || any null [time] || null fileinfo
          then invalidArgs ["failed/less and less"]
          else do
            liftHandlerT $ runDB $ deleteWhere [HtmIndex ==. head index]
            text <- fileInfo fileinfo
            liftHandlerT $ runDB $ insert $ Htm
              (head index)
              (decodeUtf8 text)
              (head title)
              (head typ)
              (read $ unpack $ head time)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""


      fileInfo :: (MonadResource a,MonadHandler a) => [FileInfo] -> a BC8.ByteString
      fileInfo [] = invalidArgs ["failed/less and less"]
      fileInfo (x:_) = do
        rt <- sourceToList $ fileSource x
        return $ BC8.concat rt

      postUptxtR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUptxtR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "txt"
        typ <- lookupPostParams "type"
        let fileinfo = toList fileinfo'
        time <- lookupHeaders "UTCTime"
        if any null [index,typ] ||  null fileinfo
          then invalidArgs ["failed/less and less"]
          else do
            liftHandlerT $ runDB $ deleteWhere [TxtIndex ==. head index]
            text <- fileInfo fileinfo
            liftHandlerT $ runDB $ insert $ Txt
              (head index)
              (decodeUtf8 text)
              (head typ)
              (read $ unpack $ head time)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""
      toList :: Maybe a -> [a]
      toList (Just x) = [x]
      toList _ = []

      postUpnavR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpnavR = do
        label <- lookupPostParams "label"
        order <- lookupPostParams "order"
        ref <- lookupPostParams "ref"
        time <- lookupHeaders "UTCTime"
        if any null [label,order,ref]
          then invalidArgs ["failed/less and less"]
          else do
            liftHandlerT $ runDB $ deleteWhere [NavLabel ==. head label]
            liftHandlerT $ runDB $ insert $ Nav
              (head label)
              (Just $ read $ read $ show $ head order)
              (head ref)
              (read $ unpack $ head time)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""

      postUpbinR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpbinR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "bin"
        let fileinfo = toList fileinfo'
        typ <- lookupPostParams "type"
        time <- lookupHeaders "UTCTime"
        if any null [index] || null fileinfo
          then invalidArgs ["failed/less and less"]
          else do
            liftHandlerT $ runDB $ deleteWhere [BinIndex ==. head index]
            text <- fileInfo fileinfo
            liftHandlerT $ runDB $ insert $ Bin
              (head index)
              text
              (head typ)
              (read $ unpack $ head time)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""

      postUpqryR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpqryR = do
        index <- lookupPostParams "index"
        txt <- lookupPostParams "txt"
        time <- lookupHeaders "UTCTime"
        if any null [index,txt]
          then invalidArgs ["failed/less and less"]
          else do
            liftHandlerT $ runDB $ deleteWhere [QryIndex ==. head index]
            liftHandlerT $ runDB $ insert $ Qry
              (head index)
              (head txt)
              (read $ unpack $ head time)
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""

      postSqlR :: Yesod master
               => HandlerT Management (HandlerT master IO) TypedContent
      postSqlR = do
        fileinfo' <- lookupFile "sql"
        case fileinfo' of
          Just fileinfo -> do
            text <- sourceToList $ fileSource fileinfo
            _ <- liftHandlerT $ runDB $ rawExecute (decodeUtf8 $ BC8.concat text) []
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""
          _ -> invalidArgs ["failed/less and less."]

      instance Yesod master => YesodSubDispatch  Management (HandlerT master IO) where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesManagement)
