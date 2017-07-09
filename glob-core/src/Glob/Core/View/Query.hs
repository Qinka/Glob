{-|
Module        : Glob.Core.View.Query
Description   : The view for query and nav
Copyright     : (C) Qinka 2017
License       : GPLv3+
Maintainer    : me@qinka.pro
Stability     : experimental
Portability   : unknown

The View part for query command, nav query.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}


module Glob.Core.View.Query
       ( -- * querys
         -- ** query version
         query_version
       , query_version_author
       , query_version_utils
       , -- ** name
         query_name
       , -- ** build info
         query_build_info
       , -- ** server time
         query_server_time
       , -- ** navbar
         query_nav
       , -- ** index
         query_index
       ) where

import           Data.Time
import           Glob.Auth.Info
import           Glob.Core.Info
import           Glob.Core.Model.Internal    (Nav, ResT)
import           Glob.Core.View.Internal
import           Glob.Core.View.Query.Parsec
import           Glob.Import
import           Glob.Import.Aeson
import qualified Glob.Import.Text            as T
import           Glob.Utils.Info
import           Yesod.Core

query_version :: Hamletic a (HandlerT a IO)
                 => HandlerT a IO TypedContent
query_version = get_version >>= respondSource "text/plain" . sendChunkText

query_version_author :: HandlerT a IO TypedContent
query_version_author = respondSource "text/plain" $ sendChunkText $glob_auth_version_quote

query_version_utils :: HandlerT a IO TypedContent
query_version_utils = respondSource "text/plain" $ sendChunkText $glob_utils_version_quote

query_version_core :: HandlerT a IO TypedContent
query_version_core = respondSource "text/plain" $ sendChunkText $glob_core_version_quote

query_name :: HandlerT a IO TypedContent
query_name = respondSource "text/plain" $ sendChunkText "Glob"

query_build_info :: HandlerT a IO TypedContent
query_build_info = respondSource "text/plain" $ sendChunkText $glob_build_info_quote

query_server_time :: HandlerT a IO TypedContent
query_server_time = T.show <$> (liftIO getCurrentTime) >>= respondSource "text/plain" . sendChunkText

query_nav :: [Nav] -- ^ navs
             -> HandlerT a IO TypedContent
query_nav = respondSource "application/json" . sendChunkLBS . encode

query_index :: String -- ^ parameters
            -> [ResT] -- ^ resources index
            -> HandlerT a IO TypedContent
query_index t = respondSource "application/json" . sendChunkLBS . encode . (run $ run_qp t)
  where run (Left e)  = error $ show e
        run (Right i) = i


