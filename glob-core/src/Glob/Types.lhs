% src/Glob/Types.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Types.lhs}
  \CodeInfo{Types definetions of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{0.0.9.25}{2016.08.08}{
    Add the author to blogs and pages.
    And moved a subfunction to Glob.Common
    }
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-08-19}{0.0.10.15}{delete some out-date things}
\end{codeinfo}

\begin{code}
module Glob.Types
    ( LogPath(..)
    , Nav(..)
    , doc2Nav , nav2Doc
    , Rest(..)
    , doc2Rest, rest2Doc
    , fromBinary
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
        [ "index"   =: navLabel
        , "url"   =: navUrl
        , "order" =: navOrder
        ]
      doc2Nav :: Document -> Maybe Nav
      doc2Nav doc = Nav
        <$> doc !? "url"
        <*> doc !? "index"
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

\begin{code}
      data Rest = Rest
          { rIndex   :: [T.Text]
          , rRest    :: ObjectId
          , rType    :: T.Text
          , rCtime   :: UTCTime
          , rUtime   :: UTCTime
          , rTitle   :: T.Text
          , rSummary :: Maybe T.Text
          , rWhose   :: Maybe T.Text
          , rMIME    :: Maybe T.Text
          , rTags    :: [T.Text]
          } deriving (Eq,Show)
\end{code}
transform bewteen Rest and Document
\begin{code}
      rest2Doc :: Rest -> Document
      rest2Doc Rest{..} =
        [ "index"       =: rIndex
        , "rest"        =: rRest
        , "type"        =: rType
        , "create-time" =: rCtime
        , "update-time" =: rUtime
        , "title"       =: rTitle
        , "summary"     =: rSummary
        , "whose"       =: rWhose
        , "mime"        =: rMIME
        , "tags"        =: rTags
        ]

      doc2Rest :: Document -> Maybe Rest
      doc2Rest doc = Rest
        <$>       doc !? "index"
        <*>       doc !? "rest"
        <*>       doc !? "type"
        <*>       doc !? "create-time"
        <*>       doc !? "update-time"
        <*>       doc !? "title"
        <*> Just (doc !? "summary")
        <*> Just (doc !? "whose")
        <*> Just (doc !? "mime")
        <*>  m2l (doc !? "tags")
        where
          m2l (Just xs) = Just xs
          m2l _         = Just []
\end{code}

toJSON
\begin{code}
      instance ToJSON Rest where
        toJSON Rest{..} = object
          [ "index"       .= rIndex
          , "type"        .= rType
          , "create-time" .= rCtime
          , "update-time" .= rUtime
          , "title"       .= rTitle
          , "summary"     .= rSummary
          , "whose"       .= rWhose
          , "mime"        .= rMIME
          , "tags"        .= rTags
          ]
\end{code}
