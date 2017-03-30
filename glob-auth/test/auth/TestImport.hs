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
import qualified Glob.Import.ByteString.Lazy as BL

-- App

data App = App

mkYesod "App" [parseRoutes|
                           auth AuthR GET POST
                           |]

instance Yesod App where
  isAuthorized _ _ = auth
  
instance Authly App where
  clientPublicKeyDir _ = ""
  
getAuthR :: Handler T.Text
getAuthR = return "get"

postAuthR :: Handler T.Text
postAuthR = return "post"

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
          return (app,id)
        afterDo = afterAll_ $ do -- IO
          removeFile "pubkey"
          removeFile "prikey"
                   
          
