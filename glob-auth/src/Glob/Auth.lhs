


\begin{code}
module Glob.Auth
       ( fetchCliPubKey
       , module X
       , authlyPost
       , auth
       ) where

import Yesod.Core
import Network.Wai.Internal
import Glob.Import
import Glob.Import.Aeson
import Glob.Auth.Types
import Glob.Auth.Core
import Glob.Auth.Types                  as X
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
\end{code}


\begin{code}
readFile__ :: FilePath -> T.Text -> HandlerT site IO B.ByteString
readFile__ dir fn = liftIO $ B.readFile $ dir ++ T.unpack fn
\end{code}

function fetchCliPubKey
return the public key of the client side's public key
\begin{code}
fetchCliPubKey :: (Authly site) => HandlerT site IO PublicKey
fetchCliPubKey = do
  dir <- clientPublicKeyDir <$> getYesod
  fn  <- lookupPostParam "pubtoken"
  case fn of
    Nothing -> invalidArgs ["PUBLIC TOKEN is missing"]
    Just fn' -> do
      pub <- decodeStrict <$> readFile__ dir fn'
      case pub of
        Nothing -> invalidArgs ["PUBLIC TOKEN is invalied!"]
        Just p -> return p
\end{code}






function authlyPost
http post param:
    token:     bas64, signed, and encrypted token
    timestamp: Data.Time.UTCTime style
    pubtoken:  client side public  key name
    pritoken:  server side private key name
\begin{code}
authlyPost :: Authly site => HandlerT site IO AuthResult
authlyPost = do
  now <- liftIO getCurrentTime
  pub <- fetchCliPubKey
  token     <-  T.concat <$> lookupPostParams "token"
  timestamp <-  T.concat <$> lookupPostParams "timestamp"
  $logDebugS "token timestamp" $ T.unlines [T.show token,T.show timestamp]
  if checkTime timestamp now
    then do let rt = verifyToken True token timestamp pub
            case rt of
              Right True -> return Authorized
              Right False -> return AuthenticationRequired
              Left e -> return $ Unauthorized $ T.show e
    else return $ Unauthorized $ "Who are you! The thing did not answer."
  where checkTime ::T.Text -> UTCTime -> Bool
        checkTime timestamp now = let (ts,limit') = T.read timestamp
                                      limit = toND limit'
                                  in abs (ts `diffUTCTime` now) < limit
        toND = fromRational . (toRational  :: Double -> Rational)
\end{code}



\begin{code}
auth :: Authly site => HandlerT site IO AuthResult
auth = do
  me <- requestMethod <$> waiRequest
  case me of
    "GET" -> return Authorized
    _ -> authlyPost
\end{code}

