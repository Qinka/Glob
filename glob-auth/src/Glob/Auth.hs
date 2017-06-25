{-|
Module:       Glob.Auth
Description:  The methods for authentication
Copyright:    (C) Qinka 2017
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The methods about authentication for Glob (using Spock)
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Glob.Auth
       ( -- * auth hook for spock
         authHook
       , -- * Type Class to limit
         Auth(..)
       , -- * reexport module
         module Glob.Auth.Core
       ) where

import           Control.Monad.IO.Class
import           Glob.Auth.Core
import           Glob.Import.ByteString (ByteString)
import qualified Glob.Import.ByteString as B
import           Yesod.Core

-- | The class to limit the an application with it hash algorithm
--   and the password of the site.
class HashAlgorithm a => Auth ctx a | ctx -> a where
  -- | to get the HASH
  tokenHash :: ctx -> a
  -- | to get the password
  tokenItem :: ctx -> ByteString

-- | The hook for authentication
authHook :: (HashAlgorithm a, Auth ctx a, MonadIO m) => ActionCtxT ctx m ctx
authHook = do
  ctx   <- getContext
  token <- rawHeader "token"
  let hash = tokenHash ctx
      item = tokenItem ctx
      rest = verifyHash hash item <$> token
  case rest of
    -- success
    Just True -> return ctx
    -- failure
    _         -> text "Who are you! The thing did not answer."

