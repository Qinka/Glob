




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

      runToken :: [String] -> String -> String
      runToken xs time = shaR $ fStBSL $ concat (loop xs ca) ++ timeS
        where
          fStBSL = B.fromStrictBS.s2b
          stdToken = showDigest $ sha256 $ fStBSL $ head xs ++ timeS
          ca' = foldr ((+).oct) 0 $ take (len `quot` 16 +1) stdToken
          ca = ca' `mod` len
          len = length xs
          timeS = showDigest $ sha256 $ fStBSL time
          shaR = take 56.case ca `mod` 4 of
            0 -> showDigest.sha224
            1 -> showDigest.sha256
            2 -> showDigest.sha384
            3 -> showDigest.sha512
      oct :: Char -> Int
      oct = fromMaybe 0.flip elemIndex "0123456789abcdef"
      loop :: [a] -> Int -> [a]
      loop xs i = let len = length xs in
        take len $ drop i $ concat $ replicate 2 xs
