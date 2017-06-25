module TestImport
       ( module TestImport
       , module X
       ) where
               
import Yesod.Core as X
import Yesod.Test   as X
import Test.Hspec  as X
import Glob.Auth   as X
import Glob.Import.Aeson
import Crypto.Hash.Algorithms
import Crypto.PubKey.RSA
import System.Directory
import Network.Wai.Internal

import qualified Glob.Import.Text            as T
import qualified Glob.Import.ByteString      as B
import qualified Glob.Import.ByteString.Lazy as BL

-- App

data App = App

mkYesod "App" [parseRoutes|auth AuthR GET POST|]

instance Yesod App where
  isAuthorized _ _ =  do
    me <- requestMethod <$> waiRequest
    case me of
      "GET" -> return Authorized
      _ -> globAuth
  
instance Auth App where
  type HashType App = SHA1
  clientPublicKeyDir _ = return ""
  pssparam _ = return defaultPSSParamsSHA1
  str2PublicKey _ str = case eitherDecodeStrict str of
    Left t -> Left $ T.pack t
    Right x -> Right x
  
  
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
          
deriveJSON defaultOptions ''PublicKey
deriveJSON defaultOptions ''PrivateKey
