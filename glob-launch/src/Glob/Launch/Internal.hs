{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Glob.Launch.Internal
       ( Rain(..)
       , RainConfig(..)
       , Route(..)
       , resourcesRain
       , rain_config_isServer
       ) where

import           Control.Monad.IO.Class
import           Data.Char
import           Data.String
import           Data.Version
import           Development.GitRev
import           Glob.Auth
import           Glob.Core.Control
import qualified Glob.Core.Info              as CInfo
import           Glob.Core.Model
import           Glob.Core.View
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString      as B
import qualified Glob.Import.ByteString.Lazy as BL
import qualified Glob.Import.Text            as T
import           Glob.Import.TH
import qualified Glob.Utils.Info             as UInfo
import           Network.Wai
import           Paths_glob_launch
import           System.Environment
import           System.IO
import           Yesod.Core


-- | basic config
data RainConfig = RCServer
                  { rcsPort  :: Int
                  , rcsTitle :: T.Text
                  }
                | RCDatabase
                  { rcdDB     :: T.Text
                  , rcdDBAddr :: String
                  , rcdUser   :: String
                  , rcdPass   :: String
                  }
                  deriving (Show,Eq)
deriveJSON defaultOptions ''RainConfig
rain_config_isServer :: RainConfig -> Bool
rain_config_isServer RCServer{..}   = True
rain_config_isServer RCDatabase{..} = False


data Rain = Rain { rainTitle    :: T.Text
                 , rainDb       :: T.Text
                 , rainDBUP     :: (T.Text, T.Text)
                 , rainConnPool :: ConnectionPool
                 , rainPort     :: Int
                 }

mkYesodData "Rain" [parseRoutes| /*Texts UrlR GET PUT DELETE |]

instance Yesod Rain where
  errorHandler er = selectRep $ do
    provideRepType "application/json" . return . T.decodeUtf8 . BL.toStrict $ encode er
    provideRep $ defaultLayout [whamlet| <h1> error
                                         <p> #{T.show er}
                                         |]
  isAuthorized (UrlR _) _ = do
    me <- requestMethod <$> waiRequest
    case me of
      "GET" -> return Authorized
      _     -> checkAuth
  defaultLayout = glob_layout
  maximumContentLength _ _ = Nothing

instance Auth Rain SHA256 where
  tokenItem _ = liftIO $ B.pack <$> getEnv "RAIN_ENV"
  tokenHash _ = return SHA256

instance Hamletic Rain (HandlerT Rain IO) where
  get_title = rainTitle <$> getYesod
  get_frame_prefix = return ".frame"
  get_version = return $(stringE (show version))
  get_raw = return False

instance Mongodic Rain (HandlerT Rain IO) where
  get_default_access_mode = return master
  get_default_db = rainDb <$> getYesod
  get_db_u_p = rainDBUP <$> getYesod
  get_pool = rainConnPool <$> getYesod

instance Controly Rain
