{-|
Module        : Glob.Auth
Description   : The methods for Yesod for check token
Copyright     : Qinka 2017
License       : GPL-3
Maintainer    : qinka@live.com
                me@qinka.pro
Stability     : experimental
Portability   : x86/64

Include the methods to verification the token.
-}

module Glob.Auth
       ( module X
       , -- methods
         readPublicKey
       , globAuth
       )  where

import           Glob.Auth.Core         as X
import           Glob.Auth.Types        as X
import           Glob.Import
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import           Network.Wai.Internal
import           Yesod.Core







-- | fetch the client's public key file
readPublicKey :: Auth site -- ^ Has the @clientPublicKeyDir@ and @str2PublicKey@ method
              => HandlerT site IO PublicKey -- ^ The public key context
readPublicKey = do
  site <- getYesod
  dir <- clientPublicKeyDir site
  fn  <- lookupPostParam "pubtoken"
  case fn of
    Nothing -> invalidArgs ["PUBLIC TOKEN is missing"]
    Just fn' -> do
      pubBytes <- liftIO $ B.readFile $ dir ++ T.unpack fn'
      let  pub = str2PublicKey site pubBytes 
      case pub of
        Left i -> invalidArgs ["PUBLIC TOKEN is invalied!",i]
        Right p -> return p


-- | the method to verification
globAuth :: Auth site -- ^ Has the @clientPublicKeyDir@ method
         => HandlerT site IO AuthResult -- ^ return the result of the verification
globAuth = do  
  now <- liftIO getCurrentTime
  pub <- readPublicKey
  token <-  B.concat <$> lookupHeaders "token"
  stamp <-  B.concat <$> lookupHeaders "stamp"
  $logDebugS "token stamp" $ T.unlines $
    map T.decodeUtf8 [B.show token,B.show stamp]
  if checkTime stamp now
    then do pssp <- pssparam =<< getYesod
            let rt = verifyToken True token stamp pub pssp
            case rt of
              Right True -> return Authorized
              Right False -> return AuthenticationRequired
              Left e -> return $ Unauthorized $ T.show e
    else return $ Unauthorized $ "Who are you! The thing did not answer."
  where checkTime :: B.ByteString -> UTCTime -> Bool
        checkTime stamp now = let (ts,limit') = B.read stamp
                                  limit = toND limit'
                              in abs (ts `diffUTCTime` now) < limit
        toND = fromRational . (toRational  :: Double -> Rational)
          
