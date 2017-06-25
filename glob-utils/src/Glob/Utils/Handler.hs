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
         showJS
       ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Either
import           Text.Julius            (RawJavascript, rawJS)
import           Yesod.Core


import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T


-- | The method to catch the exception (in HadnlerT)
catchH :: Exception e                  -- ^ a exception
          => HandlerT site IO a        -- ^ the action
          -> (e -> HandlerT site IO a) -- ^ the exception handler
          -> HandlerT site IO a        -- ^ return
catchH m h = handlerToIO >>=
  (\hio -> liftIO $ catch (hio m) (hio.h))

-- | The method for handler, equal to @flip catchH@
handlerH :: Exception e
            => (e -> HandlerT site IO a)
            -> HandlerT site IO a
            -> HandlerT site IO a
handlerH = flip catchH

-- | The method for a try
tryH :: Exception e                      -- ^ a exception
        => HandlerT site IO a            -- ^ action
        -> HandlerT site IO (Either e a) -- ^ return the result or the exception
tryH m = handlerToIO >>= (\hio -> liftIO . try $ hio m)

-- | the operator just like <$>
infixl 4 <#>, <%>
(<#>) :: (Functor f1,Functor f2) -- ^ two functors: f1 and f2
         => (a -> b)             -- ^ func
         -> f1 (f2 a)            -- ^ item
         -> f1 (f2 b)            -- ^ eq to (func <$>) <$> item
(<#>) f = ((f <$>) <$>)
(<%>) :: (Monad m, Functor f)    -- ^ a monad: m, and a functor: f
         => (a -> m b)           -- ^ func
         -> f (m a)              -- ^ item
         -> f (m b)              -- ^ eq to (func =<<) <$> item
(<%>) f = ((f =<<) <$>)

-- | a method to return a ``exception'' when it catched
return_e :: (Monad m,Exception e)        -- ^ a monad m, and exception e
            => e                         -- ^ the exception
            -> m String                  -- ^ return as string
return_e = pure . (\str -> "{\"error\":\"exception\",\"context\":\"" ++ str ++ "\"}") . show

-- | the text-returned version for return_e
return_e_t :: (Monad m,Exception e)
              => e
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
showJS :: Show a => a -> RawJavascript
showJS = rawJS . T.show
