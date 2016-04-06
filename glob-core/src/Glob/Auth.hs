




-- src/Glob/Auth.hs

{-# LANGUAGE OverloadedStrings
           , FlexibleContexts
           , TypeFamilies
           #-}

module Glob.Auth
    ( runToken
    , bgAuth
    ) where

      import Prelude as P
      import Import as I
      import qualified Import.ByteString as B
      import Data.Digest.Pure.SHA
      import Glob.MDBS
      import Yesod
      import Glob.Foundation.Config
      import Control.Concurrent(threadDelay)
      import Glob.Auth.Token

      bgAuth ::( Yesod site
               , YesodPersist site
               , YesodPersistBackend site ~ DBBackend
               )
             => String
             -> HandlerT site IO AuthResult
      bgAuth tEnv = do
        token <- lookupHeaders "Tokens"
        time <- lookupHeader "UTCTime"
        withTnT token time
        where
          withTnT token (Just time) = do
            let tt = read $ b2s time
            (lTu,lTd) <- liftIO $ timeUD 6
            stoken' <- liftIO $ getEnv tEnv
            let stoken = P.words stoken'
            let s1 =  runToken stoken $ b2s time
            if   (s2b s1 `elem` token)
              && (lTu <= tt)
              && (lTd >= tt)
              then return Authorized
              else do
                liftIO $ threadDelay 10000000
                return $ Unauthorized ":( Who are you!"
          withTnT _ _ = invalidArgs ["no token or utctime"]
          timeUD x = do
            ut <- getCurrentTime
            return (addUTCTime (-x) ut,addUTCTime x ut)
