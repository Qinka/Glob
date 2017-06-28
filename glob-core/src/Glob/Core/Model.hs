{-|
Module       : Glob.Core.Model
Description  : The module for model
Copyright    : (C) Qinka 2017
Maintainer   : me@qinka.pro
License      : GPL3
Stability    : experimenta
Portability  : unknown

The codes for model
-}
{-# LANGUAGE TemplateHaskell #-}


module Glob.Core.Model
       (
       ) where


import           Data.Pool
import           Glob.Core.Model.Internal
import           Glob.Core.Model.TH
import           Glob.Import.ByteString   (ByteString (..))
import           Glob.Import.Text         (Text (..))
import           Glob.Utils.Handler
import           Text.Blaze.Html          (Html (..), preEscapedToHtml)
-- about frame
make_fetch 'preEscapedToHtml "frame" ''Html "html" "frame"
make_update                  "frame" ''Html "html" "frame"

-- about post
make_fetch 'preEscapedToHtml "post" ''Html "html" "post"
make_update                  "post" ''Html "html" "post"

-- about text resource
make_fetch 'id "resource_t" ''Text "text" "resource"
make_update    "resource_t" ''Text "text" "resource"

-- about binary resource
make_fetch 'fromBinary "resource_b" ''ByteString "binary" "resource"
make_update            "resource_b" ''ByteString "binary" "resource"

-- about static
make_fetch 'id "static" ''Text "url" "static"
make_update    "static" ''Text "url" "static"

-- about static
make_fetch 'id "query" ''Text "var" "query"
make_update    "query" ''Text "var" "query"



