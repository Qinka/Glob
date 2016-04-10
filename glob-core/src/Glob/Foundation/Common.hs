




-- src/Glob/Foundation/Common.hs

{-# LANGUAGE OverloadedStrings
           , RecordWildCards
           #-}

module Glob.Foundation.Common
    ( returnTJson
    , provideRepValue
    , object'
    , errorObj
    , (.=.)
    , t2s
    , s2t
    , b2s
    , s2b
    , b2t
    , t2b
    , ws2s
    , s2ws
    , parts
    , globVersion
    , RtMsg(..)
    , rtMsg'
    ) where

      import Data.Aeson
      import Data.Aeson.Types
      import Yesod
      import Import.Common
      import Import.Text as T
      import Import.ByteString as B
      import Import.Oth1
      import Import.TH

      returnTJson :: (Monad m,ToJSON a)
                  => a                        -- 返回的数据
                  -> m Text
      returnTJson = return.toStrictText.lazyDecodeUtf8.encode

      provideRepValue :: (Monad m,ToJSON a)
                      => a
                      -> Writer (Endo [ProvidedRep m]) ()
      provideRepValue = provideRepType "applcation/json".returnTJson
        {-where
          rtj :: Yesod site => HandlerT site IO Text
          rtj = returnTJson-}

      object' :: Text -> String -> Value
      object' x y = object [x .= y]

      errorObj :: String -> Value
      errorObj = object' "error"

      infixr 8 .=.
      (.=.) :: Text -> String -> Pair
      (.=.) = (.=)

      instance ToJSON ErrorResponse where
        toJSON NotFound = errorObj "NotFound"
        toJSON (InternalError x) = object
          [ "error" .=. "InternalError"
          , "content" .= x
          ]
        toJSON (InvalidArgs x) = object
          [ "error" .=. "InvalidArgs"
          , "content" .= x
          ]
        toJSON NotAuthenticated = errorObj "NotAuthenticated"
        toJSON (PermissionDenied x) = object
          [ "error" .=. "PermissionDenied"
          , "content" .= x
          ]
        toJSON (BadMethod x) = object
          [ "error" .=. "BadMethod"
          , "content" .= show x
          ]

      t2s :: Text -> String
      t2s = T.unpack

      s2t :: String -> Text
      s2t = T.pack

      t2b :: Text -> ByteString
      t2b = T.encodeUtf8

      b2t :: ByteString -> Text
      b2t = T.decodeUtf8

      b2s :: ByteString -> String
      b2s = t2s.b2t

      s2b :: String -> ByteString
      s2b = t2b.s2t

      ws2s :: [Text] -> Text
      ws2s [] = ""
      ws2s (x:xs) = T.concat ["/",x,ws2s xs]

      s2ws :: Text -> [Text]
      s2ws = Prelude.tail.splitOn "/"

      parts :: Int -> ByteString -> [ByteString]
      parts _ "" = []
      parts i b = let (l,r) = B.splitAt i b in l:parts i r

      globVersion :: Q Exp
      globVersion = stringE $ showVersion version

      data RtMsg a = RtMsg
        { rtStatus :: String
        , rtMsg :: a
        }

      instance ToJSON a =>  ToJSON (RtMsg a) where
        toJSON RtMsg{..} = object
          [ "status" .= rtStatus
          , "msg" .= rtMsg
          ]
      rtMsg' :: String -> String -> RtMsg String
      rtMsg' = RtMsg
