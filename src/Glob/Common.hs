




-- src/GLob/Common.hs

{-# LANGUAGE OverloadedStrings #-}


module Glob.Common
    ( returnTJson
    , t2s
    , s2t
    , b2s
    , s2b
    , b2t
    , t2b
    , ws2s
    , s2ws
    ) where

      import Data.Aeson
      import Yesod hiding (ToJSON)
      import Data.Text as T
      import Data.ByteString.Internal as BI
      import Data.Text.Lazy(toStrict)
      import qualified Data.Text.Encoding as TE
      import Data.Text.Lazy.Encoding(decodeUtf8)

      returnTJson :: (Monad m,ToJSON a)
                 => a
                 -> HandlerT site m Text
      returnTJson = return.toStrict.decodeUtf8.encode

      instance ToJSON ErrorResponse where
        toJSON NotFound = object ["error" .= ("NotFound" ::String)]
        toJSON (InternalError x) = object
          [ "error" .= ("InternalError" :: String)
          , "content" .= x
          ]
        toJSON (InvalidArgs x) = object
          [ "error" .= ("InvalidArgs" ::String)
          , "content" .= x
          ]
        toJSON NotAuthenticated = object ["error" .= ("NotAuthenticated" ::String)]
        toJSON (PermissionDenied x) = object
          [ "error" .= ("PermissionDenied" :: String)
          , "content" .= x
          ]
        toJSON (BadMethod x) = object
          [ "error" .= ("BadMethod" :: String)
          , "content" .= show x
          ]


      t2s :: Text -> String
      t2s = T.unpack

      s2t :: String -> Text
      s2t = T.pack

      t2b :: Text -> ByteString
      t2b = TE.encodeUtf8

      b2t :: ByteString -> Text
      b2t = TE.decodeUtf8

      b2s :: ByteString -> String
      b2s = t2s.b2t

      s2b :: String ->ByteString
      s2b = t2b.s2t

      ws2s :: [Text] -> Text
      ws2s [] = ""
      ws2s (x:xs) = T.concat ["/",x,ws2s xs]

      s2ws :: Text -> [Text]
      s2ws = Prelude.tail.splitOn "/"
