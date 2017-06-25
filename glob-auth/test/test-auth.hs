{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where

import           Glob.Auth
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import           Network.Wai.Internal
import           Test.Hspec
import qualified Test.Hspec.Wai         as Test
import           Yesod.Core
import           Yesod.Test

data App = App

mkYesod "App" [parseRoutes| auth AuthR GET POST |]

instance Yesod App where
  isAuthorized _ _ = do -- HandlerT _ IO
    method <- requestMethod <$> waiRequest
    case method of
      "GET" -> return Authorized
      _     -> checkAuth

instance Auth App SHA1 where
  tokenHash _ = return SHA1
  tokenItem _ = return "12345qwert"


getAuthR :: Handler T.Text
getAuthR = return "get"

postAuthR :: Handler T.Text
postAuthR = return "post"

appSpec :: Spec
appSpec = before (return (App,id)) $ do -- YesodExample
  describe "glob-auth test(GET)" $ do
    it "load via get method with token" $ do
      request $ do -- RequestBuilder _
        setMethod "GET"
        setUrl AuthR
        hash  <- tokenHash App
        token <- generateHash hash <$> tokenItem App
        addRequestHeader ("token",token)
      statusIs 200
    it "load via get without token" $ do
      get AuthR
      statusIs 200
  describe "glob-auth test(POST)" $ do
    it "load via post method with token" $ do
      request $ do -- RequestBuilder _
        setMethod "POST"
        setUrl AuthR
        hash  <- tokenHash App
        token <- generateHash hash <$> tokenItem App
        addRequestHeader ("token",token)
      statusIs 200
    it "load via post methdo with token" $ do
      post AuthR
      statusIs 403

main :: IO ()
main = hspec appSpec

