




-- src/Glob/Sub/Background.hs

{-# LANGUAGE TemplateHaskell
           , RankNTypes
           , CPP
           , KindSignatures
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , FlexibleContexts
           , OverloadedStrings
           #-}

module Glob.Sub.Background
    ( module Glob.Sub.Background
    , module Glob.Sub.Background.Data
    ) where

      import Prelude as P
      import Import hiding (insert)
      import Import.Oth2
      import Glob.MDBS
      import Import.Text as T hiding (head,take,dropWhile,null,any,unpack,index,map)
      import Import.ByteString as B hiding (head,take,dropWhile,null,any,unpack,index,map)
      import Data.ByteString.Char8(unpack)
      import Yesod
      import Glob.Sub.Background.Data
      import Glob.Sub.Background.TH
      import Glob.Database
      import Glob.Foundation.Config
#ifdef WithMongoDB
      import qualified Data.ByteString.Char8 as BC8
#endif


      instance YesodPersist Background where
        type YesodPersistBackend Background = DBBackend
        runDB a = getYesod >>= (runWithPool a.bgConnPool)

      plItem :: ( Yesod master
                , PersistQuery (PersistEntityBackend v)
                , PersistEntity v
                , ToJSON v
                , PersistEntityBackend v ~ DBBackend
                )
             => [Filter v]
             -> HandlerT Background (HandlerT master IO) Value
      plItem fil = do
        da <- liftHandlerT $ runDB $ selectList fil []
        return $ toJSON $ map (\(Entity _ x)-> x) da

      pdItem :: ( Yesod master
                , PersistQuery (PersistEntityBackend v)
                , PersistEntity v
                , ToJSON v
                , PersistEntityBackend v ~ DBBackend
                )
             => [Filter v]
             -> HandlerT Background (HandlerT master IO) TypedContent
      pdItem fil = do
        liftHandlerT $ runDB $ deleteWhere fil
        selectRep $ provideRepValue $ rtMsg' "success" ""

      postListR :: Yesod master
                => HandlerT Background (HandlerT master IO) TypedContent
      postListR = do
        index <- lookupPostParams "index"
        typ <- lookupPostParams "type"
        mapM (mapTyp index) typ >>= (selectRep.provideRepValue)
        where
          mapTyp index typ = case typ of
            "html" -> plItem $ $(mkPLFunc "HtmIndex") index
            "nav"  -> plItem $ $(mkPLFunc "NavLabel") index
            "txt"  -> plItem $ $(mkPLFunc "TxtIndex") index
            "bin"  -> plItem $ $(mkPLFunc "BinIndex") index
            "qry"  -> plItem $ $(mkPLFunc "QryIndex") index
            _ -> invalidArgs ["failed/less and less"]

      postTagBR :: Yesod master
                => HandlerT Background (HandlerT master IO) TypedContent
      postTagBR = do
        tag <- liftHandlerT $ lookupPostParam "tag"
        typ <- liftHandlerT $ lookupPostParam "type"
        url <- liftHandlerT $ lookupPostParam "url"
        index <- liftHandlerT $ lookupPostParam "index"
        case (tag,typ,index) of
          (Just ta,Just ty,Just i) -> do
            liftHandlerT $ runDB $ insert $ Tag i ta ty url
            selectRep $ provideRepValue $ rtMsg' "success" ""
          _ -> invalidArgs ["failed/less and less"]

      postDelR :: Yesod master
               => HandlerT Background (HandlerT master IO) TypedContent
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
                  => HandlerT Background (HandlerT master IO) TypedContent
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
            selectRep $ provideRepValue $ rtMsg' "success" ""
        where
          up h x = liftHandlerT $ runDB $ update x
            [ HtmIndex =. htmIndex h
            , HtmHtml =. htmHtml h
            , HtmTitle =. htmTitle h
            , HtmTyp =. htmTyp h
            , HtmUtime =. htmUtime h
            , HtmSum =. htmSum h
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
        return []

      fileInfo :: (MonadResource a,MonadHandler a) => [FileInfo] -> a B.ByteString
      fileInfo [] = invalidArgs ["failed/less and less"]
      fileInfo (x:_) = do
        rt <- sourceToList $ fileSource x
        return $ B.concat rt

      postUptxtR :: Yesod master
                  => HandlerT Background (HandlerT master IO) TypedContent
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
            selectRep $ provideRepValue $ rtMsg' "success" ""
        where
          up tx x = liftHandlerT $ runDB $ update x
            [ TxtIndex =. txtIndex tx
            , TxtTxt =. txtTxt tx
            , TxtContent =. txtContent tx
            , TxtUtime =. txtUtime tx
            ]

      toList :: Maybe a -> [a]
      toList (Just x) = [x]
      toList _ = []

      postUpnavR :: Yesod master
                  => HandlerT Background (HandlerT master IO) TypedContent
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
            selectRep $ provideRepValue $ rtMsg' "success" ""
        where
          up n x = liftHandlerT $ runDB $ update x
            [ NavLabel =. navLabel n
            , NavOrder =. navOrder n
            , NavRef =. navRef n
            , NavUtime =. navUtime n
            ]
#ifdef WithMongoDB
      {-# WARNING postUpbinR "You can just upload the file which is smaller then 128k." #-}
#endif
      postUpbinR :: Yesod master
                  => HandlerT Background (HandlerT master IO) TypedContent
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
            selectRep $ provideRepValue $ rtMsg' "success" ""
        where
          up b x =
            liftHandlerT $ runDB $ update x
              [ BinIndex =. binIndex b
              , BinBin =. binBin b
              , BinContent =. binContent b
              , BinUtime =. binUtime b
              ]

      postUpqryR :: Yesod master
                  => HandlerT Background (HandlerT master IO) TypedContent
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
            selectRep $ provideRepValue $ rtMsg' "success" ""
        where
          up q x = liftHandlerT $ runDB $ update x
            [ QryIndex =. qryIndex q
            , QryTxt =. qryTxt q
            , QryUtime =. qryUtime q
            ]

      postRawR :: Yesod master
               => HandlerT Background (HandlerT master IO) TypedContent
      postRawR = liftHandlerT mdbsRaw


      postStatR :: Yesod master
                => HandlerT Background (HandlerT master IO) TypedContent
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
                selectRep $ provideRepValue $ rtMsg' "success" ""
              else
                selectRep $ provideRepValue $ rtMsg' "failed" "no such file"
          addS = do
            index <- lookupPostParams "index"
            typ <- lookupPostParams "type"
            fileinfo' <- lookupFile "file"
            let fileinfo = toList fileinfo'
            if any null [index,typ] || null fileinfo
              then invalidArgs ["failed/less and less"]
              else do
                let path' = t2s $ ws2s index
                sp <- fmap (globStaticPath.bgGlobConfig) getYesod
                let path = sp ++ path'
                text <- fileInfo fileinfo
                liftIO $ createDirectoryIfMissing True $ P.reverse $ dropWhile (/='/') $ P.reverse path
                liftIO $ B.writeFile path text
                liftIO $ writeFileT (path++".mime") $ head typ
                selectRep $ provideRepValue $ rtMsg' "success" ""

      instance Yesod master => YesodSubDispatch  Background (HandlerT master IO) where
        yesodSubDispatch = $(mkYesodSubDispatch resourcesBackground)
