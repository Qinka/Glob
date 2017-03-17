module TestImport
               ( module TestImport
               , module X
               ) where
               
import Yesod.Core as X
import Yesod.Test   as X
import Test.Hspec  as X
import Glob.Auth   as X
import Glob.Import.Aeson
import Crypto.PubKey.RSA
import System.Directory

import qualified Data.Text as T
import qualified Data.ByteString as B
import qualified Data.ByesString.Lazy as BL

-- App

data App = App PrivateKey

mkYesod "App" [parseRoutes|
                            /auth AuthR GET PUT
                            |]

instance Yesod App where
  authRoute _ = auth
  
instance Authly App where
  clientPublicKeyDir _ = "."
  
getAuthR :: Handler Text
getAuthR = return "get"

postAuthR :: Handler Text
podtAuthR = return "post"

withApp :: SpecWith (TestApp App) -> Spec
withApp x = do -- SpecM
  -- genrate private key
  prikey <- runIO $ do -- IO 
    (pub,pri) <- generate 300 10001
    BL.writeFile "pubkey" $ encode pub
    return pri
    
  afterAll $ do
    
  where berforDo = before $ do -- IO
          let app = App prikey
          logWare <- liftIO $ makeLogWare app
          return (app logWare)
        afterDo = afterAll_ $ do -- IO
          
          