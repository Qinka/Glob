module AuthSpec
       ( spec
       ) where

import           Glob.Auth.Core
import           Glob.Import
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text       as T
import qualified Network.HTTP.Base      as H
import           TestImport

spec :: Spec
spec = withApp $ do -- SpecM
  describe " Auth Test GET and POST" $ do
    it "load via get method" $ do
      get AuthR
      statusIs 200
    it "load via post method" $ do
      request $ do
        setMethod "POST"
        setUrl AuthR
        (stamp,Right token) <- liftIO $ do -- IO
          now <- getCurrentTime
          let limit = 6
              s = B.pack $ show (now,limit)
          Just pri <- decodeStrict <$> B.readFile "prikey"
          t <-  generateToken True s pri defaultPSSParamsSHA1
          print t
          return (s,t)
        addRequestHeader ("token",token)
        addRequestHeader ("stamp",stamp)
        addPostParam "pubtoken" "pubkey"
      printBody
      statusIs 200
