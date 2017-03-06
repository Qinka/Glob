


\begin{code}
module Glob.Auth.Internal
       ( generateToken
       , verifyToken
       , fetchCliPubKey
       , fetchSvrPriKey 
       ) where

#ifndef WITHOUT_YESOD
import Yesod.Core
#endif

import Glob.Import
import Glob.Import.Aeson
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
#ifndef WITHOUT_YESOD
import Glob.Auth.Types
#endif
import Crypto.PubKey.RSA.PKCS15
import Crypto.PubKey.RSA.Types
import qualified Crypto.Hash.Algorithms as CHA
import qualified Data.ByteString.Base64 as Base64
\end{code}

function generateToken
inoput timestamp, Server side public key, and client side private key
return the base64, signed, and encrypted byteString Token
\begin{code}
generateToken :: String -> PrivateKey -> PublicKey -> IO (Either Error B.ByteString)
generateToken stamp' pri pub = do
  si <- signSafer (Just CHA.SHA256) pri stamp
  ec <- encryptItem pub si
  return $ case ec of
    Left e    -> Left e
    Right str -> Right $ Base64.encode $ str
  where stamp = B.pack stamp'
        encryptItem _ l@(Left _) = return l
        encryptItem pub (Right ctxt) = encrypt pub ctxt
\end{code}

function verifyToken
input text of token, server side private key, and client side public key
return True or False (for is right or not)
\begin{code}
verifyToken :: T.Text -> T.Text -> PrivateKey -> PublicKey -> IO (Either String Bool)
verifyToken token' base' pri pub = do
  case Base64.decode token of
    Left s -> return $ Left s
    Right str -> do
      es <- decryptSafer pri str
      return $ warpVerify es pub base
  where token = T.encodeUtf8 token'
        base  = T.encodeUtf8 base'
        showError (Left e) = Left $ show e
        showError r@(Right _) = r
        warpVerify (Left e) _ _ = Left $ show e
        warpVerify (Right str) pub base = Right $ verify (Just CHA.SHA256) pub base str
\end{code}


\begin{code}
#ifndef WITHOUT_YESOD
readFile__ :: FilePath -> T.Text -> HandlerT site IO B.ByteString
readFile__ dir fn = liftIO $ B.readFile $ dir ++ T.unpack fn
#endif
\end{code}

function fetchCliPubKey
return the public key of the client side's public key
\begin{code}
#ifndef WITHOUT_YESOD
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
#else
fetchCliPubKey = error "This function SHOULD NOT BE CALLED\nOr reinstall glob-auth without flag: \"without-yesod\""
#endif
\end{code}


function fetchSvrPriKey
return the private key of the server side
\begin{code}
#ifndef WITHOUT_YESOD
fetchSvrPriKey :: (Authly site) => HandlerT site IO PrivateKey
fetchSvrPriKey = do
  dir <- serverPrivateKetDir <$> getYesod
  fn  <- lookupPostParam "pritoken"
  case fn of
    Nothing -> invalidArgs ["PRIVATE TOKEN is missing"]
    Just fn' -> do
      pub <- decodeStrict <$> readFile__ dir fn'
      case pub of
        Nothing -> invalidArgs ["PRIVATE TOKEN is invalied!"]
        Just p -> return p
#else
fetchSvrPriKey = error "This function SHOULD NOT BE CALLED\nOr reinstall glob-auth without flag: \"without-yesod\""
#endif
\end{code}

instance aeson(json) for PublicKey and PrivateKey
\begin{code}
deriveJSON defaultOptions ''PublicKey
deriveJSON defaultOptions ''PrivateKey
\end{code}

