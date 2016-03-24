




-- src/Glob/Default.hs

module Glob.Default
    ( defaultMain
    , defaultMainTls
    , defaultMainWithString
    , defaultMainTlsWithString
    , defaultMainWithConfig
    , defaultMainTlsWithConfig
    ) where

      import Glob
      import Glob.Tls
      import Glob.Foundation.Common
      import Data.Yaml
      import Data.Maybe

      defaultMain :: IO ()
      defaultMain =
        getContents >>= defaultMainTlsWithString

      defaultMainTls :: IO ()
      defaultMainTls =
        getContents >>= defaultMainWithString

      defaultMainWithString :: String -> IO ()
      defaultMainWithString =
        defaultMainWithConfig.fromMaybe (error "glob: Can not decode config context.").decode.s2b

      defaultMainTlsWithString :: String -> IO ()
      defaultMainTlsWithString =
        defaultMainTlsWithConfig.fromMaybe (error "glob: Can not decode config context.").decode.s2b

      defaultMainWithConfig :: GlobConfig -> IO ()
      defaultMainWithConfig config =
        runStderrLoggingT $ withConfigAndPool dbconfig $
          \pool -> liftIO $ warp
              (globPort config)
              (Glob pool config (Background pool config))
        where
          dbconfig = globDb config

      defaultMainTlsWithConfig :: GlobConfig -> IO ()
      defaultMainTlsWithConfig config =
        runStderrLoggingT $ withConfigAndPool dbconfig $
          \pool -> liftIO $ warpTls
              (globCertPath config)
              (globKeyPath config)
              (globPort config)
              (Glob pool config (Background pool config))
        where
          dbconfig = globDb config
