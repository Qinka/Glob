% src/Glob/Types.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Types.lhs}
  \CodeInfo{Types definetions of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Types
    ( HtmlRow(..)
    , doc2HR , hr2Doc
    , LogPath(..)
    , Rc(..)
    , rc2Doc , doc2Rc
    , Nav(..)
    , doc2Nav , nav2Doc
    ) where

      import Data.Char(toLower)
      import Database.MongoDB as MG
      import Yesod.Core

      import Import
      import qualified Import.Text as T
      import qualified Import.ByteStringUtf8 as B
\end{code}

fromBinary
\begin{code}
      fromBinary :: Binary -> B.ByteString
      fromBinary (Binary x) = x
\end{code}

instance ErrorResponse JSON
\begin{code}
      instance ToJSON ErrorResponse where
        toJSON NotFound = object ["error" .= ("not found" ::String)]
        toJSON (InternalError e) = object [ "error" .= ("internal error"::String)
                                          , "content" .= e
                                          ]
        toJSON (InvalidArgs es) = object [ "error" .= ("invalid args"::String)
                                         , "content" .= es
                                         ]
        toJSON NotAuthenticated = object ["error" .= ("not authenticated!"::String)]
        toJSON (PermissionDenied msg) = object [ "error" .= ("permission denied"::String)
                                               , "content" .= msg
                                               ]
        toJSON (BadMethod m) = object [ "error" .= ("bad method" :: String)
                                      , "content" .= show m
                                      ]
\end{code}

data for storied html
\begin{code}
      data HtmlRow = HtmlFrame
          { hrfIndex      :: [T.Text]
          , hrfHtml       :: T.Text
          , hrfUpdateTime :: UTCTime
          }
        | HtmlPage
          { hrpIndex      :: [T.Text]
          , hrpHtml       :: T.Text
          , hrpTitle      :: T.Text
          , hrpSummary    :: T.Text
          , hrpUpdateTime :: UTCTime
          , hrpCreateTime :: UTCTime
          }
        | HtmlBlog
          { hrbIndex      :: [T.Text]
          , hrbHtml       :: T.Text
          , hrbTitle      :: T.Text
          , hrbSummary    :: T.Text
          , hrbUpdateTime :: T.Text
          , hrbCreateTime :: T.Text
          , hrbTags       :: [T.Text]
          }
\end{code}
transform between HtmlRow and  Document
\begin{code}
      doc2HR :: Document -> Maybe HtmlRow
      doc2HR doc = case typ of
        "frame" -> HtmlFrame
          <$> doc !? "_id"
          <*> doc !? "html"
          <*> doc !? "update-time"
        "page" -> HtmlPage
          <$> doc !? "_id"
          <*> doc !? "html"
          <*> doc !? "title"
          <*> doc !? "summary"
          <*> doc !? "update-time"
          <*> doc !? "create-time"
        "blog" -> HtmlBlog
          <$> doc !? "_id"
          <*> doc !? "html"
          <*> doc !? "title"
          <*> doc !? "summary"
          <*> doc !? "update-time"
          <*> doc !? "create-time"
          <*> doc !? "tags"
        _ -> Nothing
        where
          Just typ = MG.lookup ("type") doc :: Maybe T.Text
      hr2Doc :: HtmlRow -> Document
      hr2Doc HtmlFrame{..} =
        [ "_id"         =: hrfIndex
        , "html"        =: hrfHtml
        , "update-time" =: hrfUpdateTime
        , "type"        =: ("frame"::T.Text)
        ]
      hr2Doc HtmlPage{..} =
        [ "_id"         =: hrpIndex
        , "html"        =: hrpHtml
        , "title"       =: hrpTitle
        , "summary"     =: hrpSummary
        , "update-time" =: hrpUpdateTime
        , "create-time" =: hrpCreateTime
        , "type"        =: ("page"::T.Text)
        ]
      hr2Doc HtmlBlog{..} =
        [ "_id"         =: hrbIndex
        , "html"        =: hrbHtml
        , "title"       =: hrbTitle
        , "summary"     =: hrbSummary
        , "update-time" =: hrbUpdateTime
        , "create-time" =: hrbCreateTime
        , "type"        =: ("blog" ::T.Text)
        ]
\end{code}


LogPath : The data of the logger's path
\begin{code}
      data LogPath = LogFile FilePath
                   | LogStdout
                   | LogStderr
\end{code}

instance FromJSON
\begin{code}
      instance FromJSON LogPath where
        parseJSON (Import.String v) = pure$ case T.toLower v of
          "stdout" -> LogStdout
          "stderr" -> LogStderr
          _ -> LogFile $ T.unpack v
\end{code}

resource
\begin{code}
      data Rc = RcTxt
          { rctIndex :: [T.Text]
          , rctTxt   :: T.Text
          , rctMIME  :: T.Text
          }
        | RcBin
          { rcbIndex  :: [T.Text]
          , rcbBinary :: B.ByteString
          , rcbMIME   :: T.Text
          }
\end{code}
transform between Rc and Document
\begin{code}
      rc2Doc :: Rc -> Document
      rc2Doc RcTxt{..} =
        [ "_id" =: rctIndex
        , "content" =: rctTxt
        , "MIME" =: rctMIME
        , "type" =: ("txt" ::T.Text)
        ]
      rc2Doc RcBin{..} =
        [ "_id" =: rcbIndex
        , "content" =: Binary rcbBinary
        , "MIME" =: rcbMIME
        , "type" =: ("binary" :: T.Text)
        ]
      doc2Rc :: Document -> Maybe Rc
      doc2Rc doc = case typ of
        "txt" -> RcTxt
          <$> doc !? "_id"
          <*> doc !? "content"
          <*> doc !? "MIME"
        "binary" -> RcBin
          <$> doc !? "_id"
          <*> (fromBinary <$> (doc !? "content"))
          <*> doc !? "MIME"
        _ -> Nothing
        where
          Just typ = MG.lookup "type" doc :: Maybe T.Text
\end{code}

nav
\begin{code}
      data Nav = Nav
        { navUrl   :: T.Text
        , navLabel :: T.Text
        , navOrder :: Int
        }
\end{code}

transform bewteen Nav and Document
\begin{code}
      nav2Doc :: Nav -> Document
      nav2Doc Nav{..} =
        [ "_id"   =: navLabel
        , "url"   =: navUrl
        , "order" =: navOrder
        ]
      doc2Nav :: Document -> Maybe Nav
      doc2Nav doc = Nav
        <$> doc !? "url"
        <*> doc !? "_id"
        <*> doc !? "order"
\end{code}
instance ToJSON
\begin{code}
      instance ToJSON Nav where
        toJSON Nav{..} = object
          [ "label" .= navLabel
          , "url" .= navUrl
          , "order" .= navOrder
          ]
\end{code}
