{-|
Module      : Glob.Core.Exception
Description : The exception part(and handler) of Core
Copyright   : (C) Qinka 2017
License     : GPL-3
Maintainer  : me@qinka.pro
Stability   : expreimental
Portability : unknown

The module for defining and handling the exception in the glob.
-}

module Glob.Core.Exception
       ( ErrorResponse(..)
       ) where

import           Network.HTTP.Types.Status (Status (..))

-- | Handler the error status
class ErrorResponse a where
  -- ^ The method to handler the error.
  err_rp_handler :: Status -> ActionCtxT () IO ()
  not_found :: Text -> 
  

