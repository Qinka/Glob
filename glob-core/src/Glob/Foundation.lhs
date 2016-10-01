% src/Glob/Foundation.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Foundation.lhs}
  \CodeInfo{Foundation of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  \CodeChangeLog{2016-09-25}{0.0.10.25}{a new way for query, and add rss}
  \CodeChangeLog{2016-10-01}{0.0.10.22}{config has "default"}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Foundation
       ( Glob(..)
       , Handler(..)
       , Route(..)
       , resourcesGlob
       , globLayout
       , GlobCfg(..)
       , DbCfg(..)
       ) where

import Data.Pool
import Glob.Auth
import Glob.Common
import Glob.Config
import Glob.Model
import Glob.Types
import Text.Blaze.Html(preEscapedToHtml)
import Yesod.Core
       
import Import
import Import.Logger hiding (LogType(..))
import Import.TH
import Import.Wai
import qualified Import.Text as T
import qualified Import.ByteStringUtf8 as B
\end{code}


data Glob
\begin{code}
data Glob = Glob
            { globPskEnvToken :: String
            , globTitle       :: T.Text
            , globDb          :: T.Text
            , globAM          :: AccessMode
            , globDBUP        :: (T.Text,T.Text)
            , globCP          :: ConnectionPool
            , globLogger      :: Logger
            }
\end{code}

the route of Glob
\begin{code}
mkYesodData "Glob" [parseRoutes|
                               /*Texts   UrlR      GET PUT DELETE
                               |]
\end{code}

instance Yesod class
\begin{code}
instance Yesod Glob where
  errorHandler er = selectRep $ do
    provideRepType "application/json".return.b2tUtf8.B.toStrictBS $ encode er
    provideRep $
      defaultLayout [whamlet|
                            <h1> error
                            <p> #{T.showT er}
                            |]
  isAuthorized (UrlR _) _ = do
    me <- requestMethod <$> waiRequest
    case me of
      "GET" -> return Authorized
      _ -> bgAuth =<< globPskEnvToken <$> getYesod
  makeLogger = return.globLogger
  defaultLayout =globLayout
\end{code}

Glob's Layout
\begin{code}
globLayout :: Widget -> Handler Html
globLayout w = do
  Glob{..} <- getYesod
  pc <- widgetToPageContent w
  topHtml    <- runDB' $ fetchFrame' ["~@123top"]
  bottomHtml <- runDB' $ fetchFrame' ["~@123bottom"]
  navHtml    <- runDB' $ fetchFrame' ["~@123nav"]
  case[topHtml,bottomHtml,navHtml] of
    Just tH:Just bH:Just nH:_ ->
      withUrlRenderer [hamlet|
                             $newline never
                             $doctype 5
                             <html>
                               <head>
                                 <title> #{pageTitle pc} - #{globTitle}
                                 <meta charset=utf-8>
                                 <meta name=viewport content="width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no">
                                 ^{pageHead pc}
                               <body>
                                 #{tH}
                                 #{nH}
                                 ^{pageBody pc}
                                 #{bH}
                             |]
    _ -> notFound -- withUrlRenderer [hamlet|all lost !|]
\end{code}

instance Mongoble
\begin{code}
instance Mongoble Glob where
  getDBCPool = globCP
  getDefaultAM = globAM
  getDefaultDB = globDb
  getDBUP = globDBUP
\end{code}

the configure of Glob
\begin{code}
data GlobCfg = GlobCfg
               { cfgPort         :: Port
               , cfgDb           :: DbCfg
               , cfgTitle        :: T.Text
               , cfgPskEnvToken  :: String
               , cfgLogPath      :: LogPath
               , cfgListenType   :: String
               }
\end{code}
the instance of FromJSON for Glob
\begin{code}
instance FromJSON GlobCfg where
  parseJSON (Object v) = GlobCfg
    <$> v .:? "port"                          .!= 3000
    <*> v .:  "database"
    <*> v .:  "title"                         .!= "Glob"
    <*> v .:  "password-environment-variable"  .!= "GLOB_PSK"
    <*> v .:  "log-path"                       .!= LogStdout
    <*> v .:  "listen-type"                    .!= "*"
\end{code}
the instance of ToConfig
\begin{code}
instance ToConfig GlobCfg where
  type CfgD GlobCfg = Glob
  getPort = cfgPort
  getLogPath = cfgLogPath
  getTimeout _ = 30
  getListenType = cfgListenType
  toCfgD GlobCfg{..} = Glob cfgPskEnvToken cfgTitle dbDatabase am  (dbUserName,dbPassword)
    <$> cpIO
    <*> logIO
    where
      am = case T.toLower dbAccessMd of
        "master" -> master
        "slaveok" -> slaveOk
      DbCfg{..} = cfgDb
      logIO = do
        (getter,_) <- clockDateCacher
        ls <- case cfgLogPath of
          LogFile x -> newFileLoggerSet defaultBufSize x
          LogStdout -> newStdoutLoggerSet defaultBufSize
          LogStderr -> newStderrLoggerSet defaultBufSize
        return $! Logger ls getter
      cpIO = createPool
        (connect $ readHostPort dbHostAddr)
        close
        dbPoolStrp
        (fromRational $ toRational dbPoolKpTm)
        (fromIntegral dbPoolStpM)
\end{code}

the configure of database (Mongo)
\begin{code}
data DbCfg = DbCfg
             { dbHostAddr :: String
             , dbDatabase :: T.Text
             , dbAccessMd :: T.Text
             , dbUserName :: T.Text
             , dbPassword :: T.Text
             , dbPoolStrp :: Int
             , dbPoolKpTm :: Double
             , dbPoolStpM :: Int
             }
\end{code}
the instance of FromJSON for DbCfg
\begin{code}
instance FromJSON DbCfg where
  parseJSON (Object v) = DbCfg
    <$> v .:? "hostaddr"        .!= "localhost:271017"
    <*> v .:? "database"        .!= "local"
    <*> v .:? "access-mode"     .!= "master"
    <*> v .:? "username"        .!= "root"
    <*> v .:? "password"        .!= "admin"
    <*> v .:? "pool-stripes"    .!= 6
    <*> v .:? "pool-kept-time"  .!= 6
    <*> v .:? "pool-strp-max"   .!= 6
\end{code}
