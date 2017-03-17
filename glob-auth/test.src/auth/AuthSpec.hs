module AuthSpec
       ( spec
       ) where
       
import TestImport
import Glob.Import
import Glob.Auth.Core
import Glob.Import.Aeson
import qualified Glob.Import.ByteString as B
import qualified Glob.Import.Text as T

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
              s = show (now,limit)
          Just pri <- decodeStrict <$> B.readFile "prikey"          
          t <- generateToken s pri
          print $ T.decodeUtf8' <$> t
          return (s,t) 
        addPostParam "token" $ T.decodeUtf8 token
        addPostParam "timestamp" $ T.pack stamp
        addPostParam "pubtoken" "pubkey"
       -- addFile "asd" "prikey" "text"
      printBody
      statusIs 200
