{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Glob.Launch
       ( createRain
       , warp
       , module Glob.Launch.Internal
       ) where

import           Glob.Core.Control
import           Glob.Core.Model
import qualified Glob.Import.Text     as T
import           Glob.Launch.Internal
import           System.IO
import           Yesod.Core.Dispatch


mkYesodDispatch "Rain" resourcesRain

createRain :: [RainConfig]
           -> IO (Maybe Rain)
createRain rcs = do
  case (filter rain_config_isServer rcs, filter (not.rain_config_isServer) rcs) of
    (RCServer{..}:_,RCDatabase{..}:_) -> do
      cp <- createPool (connect $ readHostPort rcdDBAddr) close 10 20 1000
      return $ Just $ Rain { rainTitle    = rcsTitle
                           , rainDb       = rcdDB
                           , rainDBUP     = (T.pack rcdUser,T.pack rcdPass)
                           , rainConnPool = cp
                           , rainPort     = rcsPort
                           }
    _ -> do
      hPutStrLn stderr "invaild config"
      return Nothing




{-


data Path = PathFile    FilePath
          | PathStdout
          | PathStderr
          | PathStdin
          deriving (Show,Eq)

instance IsString Path where
  fromString str = case lowerStr of
    "stdin"  -> PathStdin
    "stdout" -> PathStdout
    "stderr" -> PathStderr
    _        -> PathFile str
    where
      lowerStr = toLower <$> str

parseCfgFileJSON :: ToConfig a => String -> Maybe a
parseCfgFileJSON str = A.decode blStr
  where blStr = BL.fromStrict $ TE.encodeUtf8 $ T.pack str
parseCfgFileYAML :: ToConfig a => String -> Maybe a
parseCfgFileYAML str = Y.decode blStr
  where  blStr = TE.encodeUtf8 $ T.pack str
parseCfgFile :: ToConfig a => String -> Maybe a
parseCfgFile str = parseCfgFileYAML str <|> parseCfgFileJSON str
readFromPath :: Path -> IO String
readFromPath (PathFile x) = readFile x
readFromPath PathStdin    = getContents
readFromPath _            = error "can not read from write-only handles"
readFromFile :: ToConfig a => FilePath -> IO (Maybe a)
readFromFile = (parseCfgFile <$>)
  . readFromPath
  . fromString


runToConfig :: ( ToConfig a
               , CfgD a ~ b
               , Yesod b
               , YesodDispatch b
               )
            => a -> IO ()
runToConfig cfg = toCfgD cfg >>= toWaiApp >>= run
  where
    settings = setSettings cfg
    run = runSettings (settings defaultSettings).glob_middleware

globLaunchVersion :: Version
globLaunchVersion = version
globLaunchVersionQuote :: Q Exp
globLaunchVersionQuote = stringE $ showVersion version
globLaunchGitBranchQuote :: Q Exp
globLaunchGitBranchQuote = gitBranch
globLaunchGitCommitQuote :: Q Exp
globLaunchGitCommitQuote = gitHash

glob_middleware :: Middleware
glob_middleware = debugItem.gzipItem
  where
    debugItem = if CoreInfos.isDebug then logStdoutDev else id
    gzipItem =
#ifdef WithMiddlewareGzip
       gzip def
#else
       id
#endif
-}
