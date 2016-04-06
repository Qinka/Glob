




-- src/Glob/Warp.hs
{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           #-}

module Glob.Warp
    ( warpTls
    , warp
    , warpStd
    , runStdoutLoggingT
    ) where

      import Yesod.Core
      import Yesod.Core.Dispatch
      import Control.Monad.Logger
      import Control.Monad(when)
      import qualified Network.Wai.Handler.WarpTLS as W
      import qualified Network.Wai.Handler.Warp as Warp
      import qualified Data.ByteString.Char8 as S8
      import Language.Haskell.TH.Syntax(qLocation)
      import Paths_glob_main
      import Glob.Foundation.Common
      import Data.Version(showVersion)

      warpTls :: YesodDispatch site => FilePath -> FilePath -> Int -> site -> IO ()
      warpTls certPath keyPath port site = do
        logger <- makeLogger site
        toWaiApp site >>= W.runTLS
          (W.tlsSettings certPath keyPath)(
            Warp.setPort port $
            Warp.setServerName serverValue$
            Warp.setOnException (\_ e ->
              when (shouldLog' e) (mLS site logger e))
            Warp.defaultSettings)


      shouldLog' = Warp.defaultShouldDisplayException
      serverValue :: S8.ByteString
      serverValue = S8.pack $ concat
        [ "Warp "
        , Warp.warpVersion
        , " /Yesod "
        , yesodVersion
        , " /glob "
        , $(globVersion)
        , "glob-main"
        , showVersion version
        ]

      warpStd :: YesodDispatch site => Int -> site -> IO ()
      warpStd port site = do
        logger <- makeLogger site
        toWaiApp site >>= Warp.runSettings (
          Warp.setPort port $
          Warp.setServerName serverValue $
          Warp.setOnException (\_ e ->
            when (shouldLog' e) (mLS site logger e))
          Warp.defaultSettings)

      mLS site logger e = messageLoggerSource
            site
            logger
            $(qLocation >>= liftLoc)
            "glob-main"
            LevelError
            (toLogStr $ "Exception from Glob-Main: "++ show e)
