




-- src/GLob/Tls.hs

{-# LANGUAGE CPP
           , TemplateHaskell
           , OverloadedStrings
           #-}

module Glob.Tls
    ( warpTls
    ) where

      import Yesod.Core
      import Yesod.Core.Dispatch
      import Control.Monad.Logger
      import Control.Monad(when)
      import qualified Network.Wai.Handler.WarpTLS as W
      import qualified Network.Wai.Handler.Warp as Warp
      import qualified Data.ByteString.Char8 as S8
      import Language.Haskell.TH.Syntax(qLocation)

      warpTls :: YesodDispatch site => FilePath -> FilePath -> Int -> site -> IO ()
#ifdef WithTls
      warpTls certPath keyPath port site = do
        logger <- makeLogger site
        toWaiApp site >>= W.runTLS
          (W.tlsSettings certPath keyPath)(
            Warp.setPort port $
            Warp.setServerName serverValue$
            Warp.setOnException (\_ e ->
              when (shouldLog' e) $
              messageLoggerSource
                site
                logger
                $(qLocation >>= liftLoc)
                "yesod-core"
                LevelError
                (toLogStr $ "Exception from Warp: "++ show e)) $
                Warp.defaultSettings)
        where
          shouldLog' = Warp.defaultShouldDisplayException
          serverValue :: S8.ByteString
          serverValue = "Warp/Yesod/(core)/Glob-tls"
#else
      warpTls _ _ = warp
#endif
