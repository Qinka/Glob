




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
            , RankNTypes
            , CPP
            #-}

module Glob.Management
    ( module Glob.Management
    , module Glob.Management.Data
    ) where

      import Yesod
      import MDB
      import Glob.Database
      import Glob.Common
      import Glob.Management.Data
      import Glob.Management.TH
      import Glob.Config
      import Data.Conduit
      import Data.ByteString.Char8 (unpack)
      import Data.Text.Encoding(decodeUtf8)
      import System.Directory
      import qualified Data.Text.Internal as TI
      import qualified Data.ByteString.Char8 as BC8
      import qualified Data.Text.IO as TIO
      import Control.Monad(mapM)
      import Control.Monad.Trans.Reader
      import Database.Persist.Sql

#ifdef WithMongoDB
      import Database.MongoDB (runCommand1)
#endif

      instance YesodPersist Management where
        type YesodPersistBackend Management = DBBackend
        runDB a = getYesod >>= (runWithPool a.cpM)

      plItem :: ( Yesod master
                , PersistQuery (PersistEntityBackend v)
                , PersistEntity v
                , ToJSON v
                , PersistEntityBackend v ~ DBBackend
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
                , PersistEntityBackend v ~ DBBackend
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
        sum' <- lookupFile "summary"
        if any null [index,title,typ] || any null [time] || null fileinfo
          then invalidArgs ["failed/less and less"]
          else do
            keys <- liftHandlerT $ runDB $ selectKeysList [HtmIndex ==. ws2s index] []
            let t = read $ unpack $ head time
            text <- fileInfo fileinfo
            sum <- sumF sum'
            let h = Htm (ws2s index) (decodeUtf8 text) (head title) (head typ) sum t t
            case keys of
              [] -> liftHandlerT $ runDB $ insert' h
              xs -> mapM (up h) xs
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""
        where
          up h x = liftHandlerT $ runDB $ update x
            [ HtmIndex =. htmIndex h,HtmHtml =. htmHtml h, HtmTitle =. htmTitle h
            , HtmTyp =. htmTyp h,HtmUtime =. htmUtime h,HtmSum =. htmSum h
            ]
          sumF Nothing = return Nothing
          sumF (Just sum) = do
            rt <- fileInfo [sum]
            return $ Just $ decodeUtf8 rt


      insert' :: forall val (m :: * -> *).(MonadIO m, PersistStore (PersistEntityBackend val),PersistEntity val)
              => val
              -> ReaderT (PersistEntityBackend val) m [()]
      insert' i = do
        insert i
        return [()]

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
            let t = read $ unpack $ head time
            text <- fileInfo fileinfo
            let tx = Txt (ws2s index) (decodeUtf8 text) (head typ) t t
            keys <- liftHandlerT $ runDB $ selectKeysList [TxtIndex ==. ws2s index] []
            case keys of
              [] -> liftHandlerT $ runDB $ insert' tx
              xs -> mapM (up tx) xs
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""
        where
          up tx x = liftHandlerT $ runDB $ update x
            [ TxtIndex =. txtIndex tx, TxtTxt =. txtTxt tx
            , TxtContent =. txtContent tx, TxtUtime =. txtUtime tx
            ]

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
            keys <- liftHandlerT $ runDB $ selectKeysList [NavLabel ==. head label] []
            let t = read $ unpack $ head time
            let o = Just $ read $ read $ show $ head order
            let n = Nav (head label) o (head ref) t t
            case keys of
              [] -> liftHandlerT $ runDB $ insert' n
              xs -> mapM (up n) xs
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""
        where
          up n x = liftHandlerT $ runDB $ update x
            [NavLabel =. navLabel n,NavOrder =. navOrder n,NavRef =. navRef n,NavUtime =. navUtime n]
#ifdef WithMongoDB
      {-# WARNING postUpbinR "You can just upload the file which is smaller then 128k." #-}
#endif
      postUpbinR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpbinR = do
        index <- lookupPostParams "index"
        fileinfo' <- lookupFile "bin"
        let fileinfo = toList fileinfo'
        typ <- lookupPostParams "type"
        time <- lookupHeaders "UTCTime"
        if any null [index,typ] || null fileinfo
          then invalidArgs ["failed/less and less"]
          else do
            let t = read $ unpack $ head time
            text <- fileInfo fileinfo
#ifdef WithMongoDB
            let b =  Bin (ws2s index) (BC8.take (1024*128) text) (head typ) t t
#else
            let b =  Bin (ws2s index) text (head typ) t t
#endif
            keys <- liftHandlerT $ runDB $ selectKeysList [BinIndex ==. ws2s index] []
            case keys of
              [] -> liftHandlerT $ runDB $ insert' b
              xs -> mapM (up b) xs
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""
        where
          up b x =
            liftHandlerT $ runDB $ update x
              [ BinIndex =. binIndex b,BinBin =. binBin b
              , BinContent =. binContent b,BinUtime =. binUtime b
              ]

      postUpqryR :: Yesod master
                  => HandlerT Management (HandlerT master IO) TypedContent
      postUpqryR = do
        index <- lookupPostParams "index"
        txt <- lookupPostParams "txt"
        time <- lookupHeaders "UTCTime"
        if any null [index,txt]
          then invalidArgs ["failed/less and less"]
          else do
            let t = read $ unpack $ head time
            let q = Qry (ws2s index) (head txt) t t
            keys <- liftHandlerT $ runDB $ selectKeysList [QryIndex ==. ws2s index] []
            case keys of
              [] -> liftHandlerT $ runDB $ insert' q
              xs -> mapM (up q) xs
            selectRep $ provideRepType "application/json" $
              returnTJson $ rtMsg' "success" ""
        where
          up q x = liftHandlerT $ runDB $ update x
            [QryIndex =. qryIndex q,QryTxt =. qryTxt q,QryUtime =. qryUtime q]

      postSqlR :: Yesod master
               => HandlerT Management (HandlerT master IO) TypedContent

#ifdef WithMongoDB
      postSqlR = do
        fileinfo' <- lookupFile "js"
        case fileinfo' of
          Just fileInfo -> do
            text <- sourceToList $ fileSource fileInfo
            let cmd = b2t $ BC8.concat text
            liftHandlerT $ runDB $ runCommand1 cmd
            selectRep $ provideRepType "application/json" $ returnTJson $ rtMsg' "success" ""
          _ -> invalidArgs ["failed/less and less."]
#endif

#ifdef WithPostgres
      postSqlR = do
        fileinfo' <- lookupFile "sql"
        rawWays <- lookupPostParams "raw-way"
        case fileinfo' of
          Just fileinfo -> do
            text <- sourceToList $ fileSource fileinfo
            let cmd = b2t $ BC8.concat text
            case rawWays of
              "raw-sql":_ -> do
                trt <- liftHandlerT $ runDB $ rawSql' cmd
                rt $ show trt
              "raw-execute-cols":_ -> do
                c <- liftHandlerT $ runDB $ rawExecuteCount cmd []
                rt $ show c
              _ -> do
                liftHandlerT $ runDB $ rawExecuteCount cmd []
                rt ""
          _ -> invalidArgs ["failed/less and less."]
        where
          rt x = selectRep $ provideRepType "application/json" $ returnTJson $ rtMsg' "success" x
          rawSql' :: MonadIO m
                  => TI.Text -> ReaderT DBBackend m [Single PersistValue]
          rawSql' t = rawSql t []
#endif

      postStatR :: Yesod master
                => HandlerT Management (HandlerT master IO) TypedContent
      postStatR = do
        isDel <- lookupPostParam "isdel"
        case isDel of
          Just true -> delS
          _ -> addS
        where
          delS = do
            index <- lookupPostParams "index"
            let fn = t2s $ head index
            is <- liftIO $ doesFileExist fn
            if is
              then do
                liftIO $ removeFile fn
                selectRep $ provideRepType "application/json" $
                  returnTJson $ rtMsg' "success" ""
              else selectRep $ provideRepType "application/json" $
                returnTJson $ rtMsg' "failed" "no such file"
          addS = do
            index <- lookupPostParams "index"
            typ <- lookupPostParams "type"
            fileinfo' <- lookupFile "file"
            let fileinfo = toList fileinfo'
            if any null [index,typ] || null fileinfo
              then invalidArgs ["failed/less and less"]
              else do
                let path' = t2s $ ws2s index
                sp <- fmap (staticPath.configM) getYesod
                let path = sp ++ path'
                text <- fileInfo fileinfo
                liftIO $ createDirectoryIfMissing True $ reverse $ dropWhile (/='/') $ reverse path
                liftIO $ BC8.writeFile path text
                liftIO $ TIO.writeFile (path++".mime") $ head typ
                selectRep $ provideRepType "application/json" $
                  returnTJson $ rtMsg' "success" ""

      instance Yesod master => YesodSubDispatch  Management (HandlerT master IO) where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesManagement)
