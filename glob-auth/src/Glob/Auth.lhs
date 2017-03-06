\begin{code}
module Glob.Auth
       ( module X
       , authlyPost
       , auth
       ) where

#ifndef WITHOUT_YESOD
import Yesod.Core
import Network.Wai.Internal
#endif

import Glob.Auth.Internal as X
#ifndef WITHOUT_YESOD
import Glob.Auth.Types    as X
#endif

import Glob.Import
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
\end{code}


function authlyPost
http post param:
    token:     bas64, signed, and encrypted token
    timestamp: Data.Time.UTCTime style
    pubtoken:  client side public  key name
    pritoken:  server side private key name
\begin{code}
#ifndef WITHOUT_YESOD
authlyPost :: Authly site => HandlerT site IO AuthResult
authlyPost = do
  now <- liftIO getCurrentTime
  pub <- fetchCliPubKey
  pri <- fetchSvrPriKey
  token     <-  T.concat <$> lookupPostParams "token"
  timestamp <-  T.concat <$> lookupPostParams "timestamp"
  $logDebugS "token timestamp" $ T.unlines [T.show token,T.show timestamp]
  if checkTime timestamp now
    then do rt <- liftIO $ verifyToken token timestamp pri pub
            case rt of
              Right True -> return Authorized
              Right False -> return AuthenticationRequired
              Left e -> return $ Unauthorized $ T.pack e
    else return $ Unauthorized $ "Who are you! The thing did not answer."
  where checkTime ::T.Text -> UTCTime -> Bool
        checkTime timestamp now = let (ts,limit') = T.read timestamp
                                      limit = toND limit'
                                  in abs (ts `diffUTCTime` now) < limit
        toND = fromRational . (toRational  :: Double -> Rational)
#else
authlyPost = error "This function SHOULD NOT BE CALLED\nOr reinstall glob-auth without flag: \"without-yesod\""
#endif
\end{code}



\begin{code}
#ifndef WITHOUT_YESOD
auth :: Authly site => HandlerT site IO AuthResult
auth = do
  me <- requestMethod <$> waiRequest
  case me of
    "GET" -> return Authorized
    _ -> authlyPost
#else
auth = error "This function SHOULD NOT BE CALLED\nOr reinstall glob-auth without flag: \"without-yesod\""
#endif
\end{code}
