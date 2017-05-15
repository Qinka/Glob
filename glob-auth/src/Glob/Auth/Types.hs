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

import           Crypto.PubKey.RSA      (RSA)
import qualified Glob.Import.ByteString as B
import           Yesod.Core             (Yesod)
import           Yesod.Core.Handler

-- | The class where has the method to get the public key directory.
class Yesod a => Auth a where
  clientPublicKeyDir :: MonadHandler m => a -> m String
  str2PublicKey :: a -> B.ByteString -> Either T.Text PublicKey


