




-- src/Glob/Data.hs

{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , ViewPatterns
           , TypeFamilies
           , OverloadedStrings
           #-}

module Glob.Data where

      import Prelude as P
      import Yesod
      import Database.Persist.Postgresql
      import Glob.Config
      import Glob.Management
      import Glob.Database
      import Glob.Common
      import Data.Text as T
      import Data.Time
      import Text.Blaze.Html
      import Text.Hamlet(hamletFile)
      import System.Environment
      import Data.Digest.Pure.SHA
      import Control.Monad(liftM)
      import qualified Data.ByteString as B
      import qualified Data.ByteString.Lazy as BL



      data Glob = Glob
        { conPool :: ConnectionPool
        , config :: Config
        , subManagement :: Management
        }


      mkYesodData "Glob" $(parseRoutesFile "src/Glob/QQ/glob.route")

      instance Yesod Glob where
        errorHandler x = selectRep $ do
          provideRepType "application/json" $ returnTJson x
          provideRep $ defaultLayout [whamlet|#{show x}|]
        isAuthorized (ManagementR _) _ = managementAuthorCheck
        isAuthorized _ _ = return Authorized
        defaultLayout = globLayout

      instance YesodPersist Glob where
        type YesodPersistBackend Glob = SqlBackend
        runDB a = do
          (Glob p _ _) <- getYesod
          runSqlPool a p

      managementAuthorCheck :: Handler AuthResult
      managementAuthorCheck = do
        token <- lookupHeaders "Tokens"
        day <- lookupHeaders "Day"
        time <- lookupHeaders "TimeOfDay"
        let td = read $ b2s $ P.head day
        let tt = read $ b2s $ P.head time
        (lTu,lTd) <- liftIO $ timeUD 6
        (Glob _ (Config _ _ _ _ _ _ env) _) <- getYesod
        stoken <- liftIO $ getEnv env
        let s1 = showDigest $ sha256 $ BL.fromStrict $ B.concat [s2b stoken,P.head day,P.head time]
        if   (s1 `elem` P.map b2s token)
          && (localDay lTu <= td)
          && (localTimeOfDay lTu <= tt)
          && (localDay lTd >= td)
          && (localTimeOfDay lTd >= tt)
          then return Authorized
          else return $ Unauthorized ":( Who are you!"
        where
          timeUD x = do
            ut <- getCurrentTime
            u <- toLocalTime $ addUTCTime (-x) ut
            d <- toLocalTime $ addUTCTime x ut
            return (u,d)
          toLocalTime ut =
            utcToLocalTime' ut `liftM` getTimeZone ut
          utcToLocalTime' = flip utcToLocalTime

      globLayout :: Widget -> Handler Html
      globLayout w = do
        Glob _ (Config _ _ _ ti _ _ _) _<- getYesod
        pc <- widgetToPageContent w
        [Entity _ (Htm _ topText _ _ _)] <- runDB $
          selectList [HtmIndex ==. "$page.frame.top" , HtmTyp ==. "home"] []
        [Entity _ (Htm _ bottomText _ _ _)] <- runDB $
          selectList [HtmIndex ==. "$page.frame.bottom", HtmTyp ==. "home"] []
        [Entity _ (Htm _ navText _ _ _)] <- runDB $
          selectList [HtmIndex ==. "$page.frame.nav", HtmTyp ==. "home"] []
        let topHtml = preEscapedToHtml topText
        let bottomHtml = preEscapedToHtml bottomText
        let navHtml = preEscapedToHtml navText
        withUrlRenderer $(hamletFile "src/Glob/QQ/layout.hamlet")
