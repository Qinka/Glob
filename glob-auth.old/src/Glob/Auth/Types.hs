{-|
Module        : Glob.Auth.Type
Description   : The type-class for Auth
Copyright     : Qinka 2017
License       : GPL-3
Maintainer    : qinka@live.com
                me@qinka.pro
Stability     : experimental
Portability   : x86/64

For a data which is instance of Auth, such a data can run some things.
-}

module Glob.Auth.Types
       ( -- * class
         Auth(..)
       ) where

import           Crypto.Hash.Algorithms (HashAlgorithm)
import           Crypto.PubKey.RSA      (PublicKey)
import           Crypto.PubKey.RSA.PSS  (PSSParams)
import           Glob.Import.ByteString (ByteString)
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import           Yesod.Core             (MonadHandler, Yesod)
import           Yesod.Core.Handler

-- | The class where has the method to get the public key directory.
class (Yesod a,HashAlgorithm (HashType a)) => Auth a where
  type HashType a :: *
  clientPublicKeyDir :: MonadHandler m => a -> m String
  pssparam :: MonadHandler m
           => a -> m (PSSParams (HashType a) ByteString ByteString)
  str2PublicKey :: a -> B.ByteString -> Either T.Text PublicKey


