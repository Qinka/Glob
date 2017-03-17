module AuthSpec
       ( spec
       ) where
       
import TestImport
import Glob.Import
import Glob.Auth.Core
import qualified Glob.Import.Text as T

spec :: Spec
spec = withApp $ do -- SpecM
  describe " Auth Test Get" $ do
    it "load via get method" $ do
      get AuthR
      statusIs 200
    it "load via post method" $ do
      request $ do
        setMethod "POST"
        setUrl AuthR
        (stamp,token) <- liftIO $ do -- IO
          now <- getCurrentTime
          let limit = 6
              s = show (now,limit)
          pri <- decodeStrict <$> B.readFile "prikey"          
          t <- generateToken s pri
          return (s,t) 
        addPostParam "token" $ T.decodeUtf8 token
        addPostParam "timestamp" $ T.pack stamp
        addPostParam "pubtoken" "pubkey"
        staticIs 200
        