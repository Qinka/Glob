




-- lib/Glob/Management.hs
{-# LANGUAGE  QuasiQuotes
            , TemplateHaskell
            , OverloadedStrings
            , TypeFamilies
            , GADTs
            , FlexibleInstances
            , GeneralizedNewtypeDeriving
            , DeriveGeneric
            , MultiParamTypeClasses
            #-}


module Glob.Management
    ( module Glob.Management
    , module Glob.Management.Data
    ) where


      import Yesod
      import Glob.Config
      import Glob.Database
      import Glob.Management.Data
      import Data.Conduit
      import Data.Aeson

      import Database.Persist.Postgresql

      import Data.ByteString.Char8 (unpack)
      import Data.ByteString.Lazy(toStrict)
      import Data.Text as T hiding (map,null,head,unpack,any)
      import Data.Text.Encoding(decodeUtf8)
      import System.Environment

      instance YesodPersist Management where
        type YesodPersistBackend Management = SqlBackend
        runDB a = do
          Management p _ <- getYesod
          runSqlPool a p

      postListR :: Yesod master
               => HandlerT Management (HandlerT master IO) TypedContent
      postListR = do
        index <- lookupPostParams "index"
        typ <- lookupPostParams "type"
        case typ of
          ("html":_) -> do
            da <- liftHandlerT $ runDB $ selectList (htms index) []
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ RtMsg "success" $ ent da
          ("nav":_) -> do
            da <- liftHandlerT $ runDB $ selectList (navs index) []
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ RtMsg "success" $ ent da
          ("txt":_) -> do
            da <- liftHandlerT $ runDB $ selectList (txts index) []
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ RtMsg "success" $ ent da
          ("bin":_) -> do
            da <- liftHandlerT $ runDB $ selectList (bins index) []
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ RtMsg "success" $ ent da
          ("qry":_) -> do
            da <- liftHandlerT $ runDB $ selectList (qrys index) []
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ RtMsg "success" $ ent da
          _ -> selectRep $ provideRepType "application/json" $
            return.decodeUtf8.toStrict.encode $ rtMsg' "failed" "invalid args"
        where
          htms = a2o.map (\x -> HtmIndex ==. x)
          navs = a2o.map (\x -> NavLabel ==. x)
          txts = a2o.map (\x -> TxtIndex ==. x)
          bins = a2o.map (\x -> BinIndex ==. x)
          qrys = a2o.map (\x -> QryIndex ==. x)
          a2o [] = []
          a2o [x] = [x]
          a2o (x:xs) = [x] ||. a2o xs
          ent' (Entity _ x) = x
          ent = map ent'

      rtMsg' :: String -> String -> RtMsg String
      rtMsg' = RtMsg

      postDelR :: Yesod master
               => HandlerT Management (HandlerT master IO) TypedContent
      postDelR = do
        index <- lookupPostParams "index"
        typ <- lookupPostParams "type"
        case (index,typ) of
          (i:_,"html":_) -> do
            liftHandlerT $ runDB $ deleteWhere [HtmIndex ==. i]
            selectRep $ provideRepType "application/json" $
              return .decodeUtf8.toStrict.encode $ rtMsg' "success" ""
          (i:_,"txt":_) -> do
            liftHandlerT $ runDB $ deleteWhere [TxtIndex ==. i]
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""
          (i:_,"nav":_) -> do
            liftHandlerT $ runDB $ deleteWhere [NavLabel ==. i]
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""
          (i:_,"bin":_) -> do
            liftHandlerT $ runDB $ deleteWhere [BinIndex ==. i]
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""
          (i:_,"qry":_) -> do
            liftHandlerT $ runDB $ deleteWhere [QryIndex ==. i]
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""
          _ -> selectRep $ provideRepType "application/json" $
            return .decodeUtf8.toStrict.encode $ rtMsg' "failed" "Invalid Args"





      postUphtmlR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUphtmlR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "upload"
        let fileinfo = case fileinfo' of {(Just x)-> [x];_ -> []}
        title <- lookupPostParams "title"
        typ <- lookupPostParams "type"
        day <- lookupHeaders "Day"
        if any null [index,title,typ] || any null [day] || null fileinfo
          then selectRep $ provideRepType "application/json" $
            return.decodeUtf8.toStrict.encode $ rtMsg' "failed" "less and less."
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
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""

      postUptxtR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUptxtR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "txt"
        let fileinfo = case fileinfo' of {(Just x)-> [x];_ -> []}
        typ <- lookupPostParams "type"
        if any null [index,typ] ||  null fileinfo
          then selectRep $ provideRepType "application/json" $
            return.decodeUtf8.toStrict.encode $ rtMsg' "failed" "less and less."
          else do
            liftHandlerT $ runDB $ deleteWhere [TxtIndex ==. head index]
            text <- sourceToList $ fileSource $ head fileinfo
            liftHandlerT $ runDB $ insert $ Txt
              (head index)
              (decodeUtf8 $ head text)
              (head typ)
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""

      postUpnavR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpnavR = do
        label <- lookupPostParams "label"
        order <- lookupPostParams "order"
        ref <- lookupPostParams "ref"
        if any null [label,order,ref]
          then selectRep $ provideRepType "application/json" $
            return.decodeUtf8.toStrict.encode $ rtMsg' "failed" "less and less."
          else do
            liftHandlerT $ runDB $ deleteWhere [NavLabel ==. head label]
            liftHandlerT $ runDB $ insert $ Nav
              (head label)
              (Just $ read $ read $ show $ head order)
              (head ref)
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""

      postUpbinR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpbinR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "bin"
        let fileinfo = case fileinfo' of {(Just x)-> [x];_ -> []}
        typ <- lookupPostParams "type"
        if any null [index] || null fileinfo
          then selectRep $ provideRepType "application/json" $
            return.decodeUtf8.toStrict.encode $ rtMsg' "failed" "less and less."
          else do

            liftHandlerT $ runDB $ deleteWhere [BinIndex ==. head index]
            text <- sourceToList $ fileSource $ head fileinfo
            liftHandlerT $ runDB $ insert $ Bin
              (head index)
              (head text)
              (head typ)
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""

      postUpqryR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpqryR = do
        index <- lookupPostParams "index"
        txt <- lookupPostParams "txt"
        if any null [index,txt]
          then selectRep $ provideRepType "application/json" $
            return.decodeUtf8.toStrict.encode $ rtMsg' "failed" "less and less."
          else do

            liftHandlerT $ runDB $ deleteWhere [QryIndex ==. head index]
            liftHandlerT $ runDB $ insert $ Qry
              (head index)
              (head txt)
            selectRep $ provideRepType "application/json" $
              return.decodeUtf8.toStrict.encode $ rtMsg' "success" ""


      instance Yesod master => YesodSubDispatch  Management (HandlerT master IO) where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesManagement)
