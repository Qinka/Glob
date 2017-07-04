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

import Yesod.Core

query_version :: HandlerT a IO TypedContent
query_version = 
