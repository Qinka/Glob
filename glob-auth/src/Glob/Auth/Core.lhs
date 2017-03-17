
\begin{code}
module Glob.Auth.Core
       ( generateToken
       , verifyToken
       , module X
       ) where


import Glob.Import
import Glob.Import.Aeson
import Crypto.PubKey.RSA as X
import Crypto.PubKey.RSA.PSS
import Crypto.Hash.Algorithms(SHA256(..))
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import qualified Data.ByteString.Base64 as Base64
\end{code}

function generateToken
inoput timestamp, Server side public key, and client side private key
return the base64, and signed byteString Token
\begin{code}
generateToken :: String -> PrivateKey -> IO (Either Error B.ByteString)
generateToken stamp' pri = (Base64.encode <$>) <$> signSafer dPP pri stamp
  where stamp = B.pack stamp'
\end{code}

function verifyToken
input text of token, server side private key, and client side public key
return True or False (for is right or not)
\begin{code}
verifyToken :: T.Text -> T.Text -> PublicKey -> Either String Bool
verifyToken token' base' pub = 
  (v <$>) . showError $ Base64.decode token
  where token = T.encodeUtf8 token'
        base  = T.encodeUtf8 base'
        showError (Left e) = Left $ show e
        showError r@(Right _) = r
        v = verify dPP pub base
\end{code}

default PSSParams
\begin{code}
dPP = defaultPSSParams SHA256
\end{code}



instance aeson(json) for PublicKey and PrivateKey
\begin{code}
deriveJSON defaultOptions ''PublicKey
deriveJSON defaultOptions ''PrivateKey
\end{code}