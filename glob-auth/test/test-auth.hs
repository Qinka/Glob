{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import           Glob.Auth
import           Test.Hspec
import qualified Test.Hspec.Wai as Test
import           Web.Spock.Core


instance Auth () SHA1 where
  tokenHash _ = SHA1
  tokenItem _ = "12345qwert"

app :: SpockT IO ()
app = do
  prehook authHook $
    get "auth"   $ text "ok"
  get "noauth" $ text "ok"


appSpec :: Spec
appSpec = do
  describe "Without token" $ Test.with (spockAsApp $ spockT id app) $ do
    it "try to access auth" $
      Test.request "GET" "auth"   [] "" `Test.shouldRespondWith` "Who are you! The thing did not answer."
    it "try to access noauth" $
      Test.request "GET" "noauth" [] "" `Test.shouldRespondWith` "ok"
  describe "With token" $ let token = generateHash SHA1 "12345qwert" in
    Test.with (spockAsApp $ spockT id app) $ do
    it "try to access auth" $
      Test.request "GET" "auth"    [("token",token)] "" `Test.shouldRespondWith` "ok"
    it "try to access noauth" $
      Test.request "GET" "noauth"  [("token",token)] "" `Test.shouldRespondWith` "ok"


main :: IO ()
main = hspec appSpec

