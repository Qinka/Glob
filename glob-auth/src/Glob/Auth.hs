{-|
Module:       Glob.Auth
Description:  The methods for authentication
Copyright:    (C) Qinka 2017
License:      GPL3
Maintainer:   me@qinka.pro
Stability:    experimental
Portability:  unknown

The methods about authentication for Glob (using Yesod)
-}

{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}

module Glob.Auth
       ( -- * check whether it is authorized
         checkAuth
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
class HashAlgorithm a => Auth site a | site -> a where
  -- | to get the HASH
  tokenHash :: MonadIO m => site -> m a
  -- | to get the password
  tokenItem :: MonadIO m => site -> m ByteString

checkAuth :: Auth site hash -- ^ need token hash and token item
             => HandlerT site IO AuthResult -- ^ return result
checkAuth = do -- Handler _ IO
  site  <- getYesod
  item  <- tokenItem site
  hash  <- tokenHash site
  token <- lookupHeader "token"
  case verifyHash hash item <$> token of
    -- success
    Just True -> return   Authorized
    -- failure
    _         -> return $ Unauthorized "Who are you! The thing did not answer."

