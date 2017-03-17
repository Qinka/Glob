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

import qualified Glob.Import.Text            as T
import qualified Glob.Import.ByteString      as B
import qualified Glob.Import.ByesString.Lazy as BL

-- App

data App = App

mkYesod "App" [parseRoutes|
                           auth AuthR GET PUT
                           |]

instance Yesod App where
  authRoute _ = auth
  
instance Authly App where
  clientPublicKeyDir _ = ""
  
getAuthR :: Handler Text
getAuthR = return "get"

postAuthR :: Handler Text
podtAuthR = return "post"

withApp :: SpecWith (TestApp App) -> Spec
withApp x = do -- SpecM
  -- genrate private key
  runIO $ do -- IO 
    (pub,pri) <- generate 300 10001
    BL.writeFile "prikey" $ encode pri
    BL.writeFile "pubkey" $ encode pub
  -- test
  beforeDo $ afterDo x 
    
  where beforeDo = before $ do -- IO
          let app = App
          logWare <- liftIO $ makeLogWare app
          return (app logWare)
        afterDo = afterAll_ $ -- IO
          removeFile "pubkey"
          removeFile "prikey"
                   
          