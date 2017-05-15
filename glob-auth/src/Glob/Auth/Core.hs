{-|
Module        : Glob.Auth.Core
Description   : Core method for encrypt and decrypt
Copyright     : Qinka 2017
License       : GPL-3
Maintainer    : qinka@live.com
                me@qinka.pro
Stability     : experimental
Portability   : x86/64

The core module for encrpyt and decrypt(verification)
-}


module Glob.Auth.Core
       ( module X
       , -- * generate token
         generateToken
       , -- * verification token
         verifyToken
       ) where



import           Crypto.Hash.Algorithms as X
import           Crypto.PubKey.RSA      as X
import           Crypto.PubKey.RSA.PSS

import qualified Data.ByteString.Base64 as Base64
import           Glob.Import
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import qualified Network.HTTP.Base      as H


-- | generate token
generateToken :: HashAlgorithm hash                   -- ^ The SHA
              => Bool                                 -- ^ Whether encode(HTTP)
              -> B.ByteString                         -- ^ The stamp string
              -> PrivateKey                           -- ^ The private key for encrypt
              -> PSSParams hash ByteString ByteString -- ^ The param of PSS
              -> IO (Either Error ByteString)         -- ^ Return the Error or Token
generateToken isEncode stamp priKey pssp =
  enUrl . enBa64 <$> signSafer pssp  priKey stamp
  where enUrl  = fmap (if isEncode then B.pack . H.urlEncode . B.unpack else id)
        enBa64 = fmap Base64.encode


-- | verify token
verifyToken :: Bool -- ^ Whether decode(HTTP)
            -> B.ByteString -- ^ the token to be verified
            -> B.ByteString -- ^ the source of the token
            -> PublicKey    -- ^ the public key of the client
            -> PSSParams hash ByteString ByteString -- ^ The param of PSS
            -> Either String Bool -- ^ the error or whether is value token
verifyToken isDecode token' base pubKey pssp =
  (verify' <$>) . showError $ deBa64 token
  where deUrl = (if isDecode then B.pack . H.urlDecode . B.unpack else id)
        deBa64 = Base64.decode
        verify' = verify pssp pubKey base
        showError (Left e) = Left $ show e
        showError r        = r
