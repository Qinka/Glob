
\begin{code}
module Types
       ( Res(..)
       , Path
       , Title
       , Tag
       , Tags
       , CTime
       , Sum
       , Whose
       , Id
       , Content(..)
       , GlobalSettings(..)
       , fetchContent
       , toHtml
       ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Time

import System.Console.CmdArgs
import System.Console.CmdArgs.Default
import System.IO

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
\end{code}


Define the post infos
\begin{code}
type Path = String
type Title = String
type Tag = String
type Tags = [Tag]
type CTime = UTCTime
type Sum = Either FilePath String
type Whose = Maybe String
type Id = String
type MIME = Maybe String 
data Res = Res Id Path Title [Tag] Sum Content CTime Whose MIME
           deriving (Eq,Show)
data Content = Post FilePath
             | Frame FilePath
             | Binary FilePath
             | TextFile FilePath
             | Static String
             | Query String
             deriving (Show,Eq)

fetchContent :: Content -> String
fetchContent i = case i of
  Post p -> toHtml p
  Frame f -> toHtml f
  Binary b -> b
  TextFile t -> t
  Static u -> u
  Query v -> v

toHtml :: String -> String
toHtml fn = if ext == "html"
            then fn
            else ".ignore/" ++ fn' ++ ".html"
  where nf' = takeWhile (/='/') $ reverse $ fn
        ext = reverse $ takeWhile (/='.') nf'
        fn' = reverse $ tail $ dropWhile (/='.') nf'


instance ToJSON Res where
  toJSON (Res i p t ts s c ct w m) = object $ 
    [ "id"    .= i
    , "type"  .= (resType :: String)
    , "path"  .= p
    , "title" .= t
    , "tags"  .= ts
    , "create-time" .= ct
    , "whose" .= w
    , "summary" .= s
    , "mime"  .= m
    ] ++ typRlt
    where
      (resType,typRlt) = case c of
        (Post p) -> ("post",["html" .= p])
        (Frame f) -> ("frame", ["html" .= f])
        (Binary b) -> ("binary",["binary" .= b])
        (TextFile t) -> ("text",["text" .= t])
        (Static s) -> ("static",["url" .= t])
        (Query v) -> ("query",["var" .= v])

instance FromJSON Res where
  parseJSON (Object v) = Res
    <$> v .:  "id"
    <*> v .:  "path"
    <*> v .:  "title"
    <*> v .:  "tags"
    <*> v .:  "summary"
    <*> resP v
    <*> v .:  "create-time"
    <*> v .:? "whose"
    <*> v .:? "mime"
    where resP v = do
            t <- v .: "type" :: Parser String
            case t of
              "post" -> v .: "html" >>= (\post -> return $ Post post)
              "frame" -> v .: "html" >>= (\frame -> return $ Frame frame)
              "binary" -> v .: "binary" >>= (\bin -> return $ Binary bin)
              "text" -> v .: "text" >>= (\txt -> return $ TextFile txt)
              "static" -> v .: "url" >>= (\url -> return $ Static url)
              "query" -> v .: "var" >>= (\qu -> return $ Query qu)
              y -> mempty
\end{code}


the basic settings
\begin{code}
data GlobalSettings = GlobalSettings
                      { curlPath      :: String
                      , curlDetail    :: String
                      , shellKind     :: String
                      , echoKind      :: String
                      , siteUrl       :: String
                      , privateKey    :: String
                      , siteDelta     :: String
                      , ihPath        :: String
                      , ihDelay       :: String
                      , ihNow         :: String
                      , timeCheckPath :: String
                      , owener        :: String
                      } deriving (Eq,Show)
                      
instance ToJSON GlobalSettings where
  toJSON GlobalSettings{..} = object
    [ "curl-path" .= curlPath
    , "curl-detail" .= curlDetail
    , "shell" .= shellKind
    , "echo" .= echoKind
    , "site-url" .= siteUrl
    , "private-key" .= privateKey
    , "ih-path" .= ihPath
    , "ih-delay" .= ihDelay
    , "ih-now" .= ihNow
    , "time-check-path" .= timeCheckPath
    , "owener" .= owener
    , "site-delta" .= siteDelta
    ]

instance FromJSON GlobalSettings where
  parseJSON (Object v) = GlobalSettings
    <$> v .: "curl-path"
    <*> v .: "curl-detail"
    <*> v .: "shell"
    <*> v .: "echo"
    <*> v .: "site-url"
    <*> v .: "private-key"
    <*> v .: "site-delta"
    <*> v .: "ih-path"
    <*> v .: "ih-delay"
    <*> v .: "ih-now"
    <*> v .: "time-check-path"
    <*> v .: "owener"
\end{code}

commands
\begin{code}
type Url = String
type Label = String
type Index = Int
data Nav = Nav Url Label Index

instance ToJSON Nav where
  toJSON (Nav u l i) = object
    [ "index" .= i
    , "label" .= l
    , "url"   .= u
    ]

instance FromJSON Nav where
  parseJSON (Object v) = Nav
    <$> v .: "index"
    <*> v .: "label"
    <*> v .: "url"
\end{code}

