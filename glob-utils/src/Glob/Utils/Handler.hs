{-|
Module       : Glob.Utils.Handler
Description  : The some methods used in server
Copyright    : (C) Qinka, 2017
License      : GPL3
Maintainer   : me@qinka.pro
Stability    : experimental
Portability  : unknown

The method which might be useful for server
-}

{-# LANGUAGE OverloadedStrings #-}

module Glob.Utils.Handler
  ( -- * about exception
    catchH
  , handlerH
  , tryH
  , -- * about functor and moand
    (<#>)
  , (<%>)
  , -- * return the exception
    return_e
  , return_e_t
  , return_e_h
  , -- * others
    show_js
  , from_bin_to_bytestr
  , LogPath(..)
  ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Either
import           Database.MongoDB
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import           Text.Julius            (RawJavascript, rawJS)
import           Yesod.Core


-- | The method to catch the exception (in HadnlerT)
catchH :: Exception e
       => HandlerT site IO a        -- ^ the action
       -> (e -> HandlerT site IO a) -- ^ the exception handler
       -> HandlerT site IO a        -- ^ return
catchH m h = handlerToIO >>=
  (\hio -> liftIO $ catch (hio m) (hio.h))

-- | The method for handler, equal to @flip catchH@
handlerH :: Exception e
         => (e -> HandlerT site IO a) -- ^ the handler for exceptions
         -> HandlerT site IO a -- ^ operation
         -> HandlerT site IO a
handlerH = flip catchH

-- | The method for a try
tryH :: Exception e
     => HandlerT site IO a            -- ^ action
     -> HandlerT site IO (Either e a) -- ^ return the result or the exception
tryH m = handlerToIO >>= (\hio -> liftIO . try $ hio m)

-- | the operator just like <$>
infixl 4 <#>, <%>
(<#>) :: (Functor f1,Functor f2)
      => (a -> b)             -- ^ func
      -> f1 (f2 a)            -- ^ item
      -> f1 (f2 b)            -- ^ eq to (func <$>) <$> item
(<#>) f = ((f <$>) <$>)
(<%>) :: (Monad m, Functor f)
      => (a -> m b)           -- ^ func
      -> f (m a)              -- ^ item
      -> f (m b)              -- ^ eq to (func =<<) <$> item
(<%>) f = ((f =<<) <$>)

-- | a method to return a ``exception'' when it catched
return_e :: (Monad m,Exception e)
         => e                         -- ^ the exception
         -> m String                  -- ^ return as string
return_e = pure . (\str -> "{\"error\":\"exception\",\"context\":\"" ++ str ++ "\"}") . show

-- | the text-returned version for return_e
return_e_t :: (Monad m,Exception e)
           => e -- ^ exception
           -> m T.Text
return_e_t = (fmap T.pack) . return_e

-- | the HandlerT version for return_e and return_et
return_e_h :: SomeException -- ^ exception
           -> HandlerT site IO TypedContent
return_e_h e = return_e e >>=
  (\str -> respondSource "application/json" $ do
      sendChunk str
      sendFlush
  )

-- | show the js
show_js :: Show a => a -> RawJavascript
show_js = rawJS . T.show


-- | from binary to bytestring
from_bin_to_bytestr :: Binary -> B.ByteString
from_bin_to_bytestr (Binary x) = x

-- | the path for logger
data LogPath = LogFile FilePath -- ^ using files
             | LogStdout        -- ^ using stdout
             | LogStderr        -- ^ using stderr

instance FromJSON LogPath where
  parseJSON (Yesod.Core.String v) = pure $ case T.toLower v of
    "stdout" -> LogStdout
    "stderr" -> LogStderr
    _        -> LogFile $ T.unpack v


-- | instance the error response to json
instance ToJSON ErrorResponse where
  toJSON NotFound =
    object ["error" .= ("not found" ::String)]
  toJSON (InternalError e) =
    object [ "error" .= ("internal error"::String)
           , "content" .= e
           ]
  toJSON (InvalidArgs es) =
    object [ "error" .= ("invalid args"::String)
           , "content" .= es
           ]
  toJSON NotAuthenticated =
    object ["error" .= ("not authenticated!"::String)]
  toJSON (PermissionDenied msg) =
    object [ "error" .= ("permission denied"::String)
           , "content" .= msg
           ]
  toJSON (BadMethod m) =
    object [ "error" .= ("bad method" :: String)
           , "content" .= show m
           ]

