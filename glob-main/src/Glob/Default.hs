




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
      import Glob.Warp
      import Glob.Foundation.Common
      import Glob.Foundation.Base
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
      defaultMainWithConfig config = do
        lg <- globMkLogger False (Glob undefined config undefined undefined)
        runStdoutLoggingT $ withConfigAndPool dbconfig $
          \pool -> liftIO $ warpStd
              (globPort config)
              (Glob pool config (Background pool config) lg)
        where
          dbconfig = globDb config

      defaultMainTlsWithConfig :: GlobConfig -> IO ()
      defaultMainTlsWithConfig config = do
        lg <- globMkLogger False (Glob undefined config undefined undefined)
        runStdoutLoggingT $ withConfigAndPool dbconfig $
          \pool -> liftIO $ warpTls
              (globCertPath config)
              (globKeyPath config)
              (globPort config)
              (Glob pool config (Background pool config) lg)
        where
          dbconfig = globDb config
