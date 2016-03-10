




-- src/Glob/Static.hs


module Glob.Static
    ( getStaticR
    ) where

      import Glob.Data
      import Glob.Common
      import Glob.Config
      import Yesod
      import System.Directory
      import Data.Text
      import qualified Data.ByteString as BIO

      getStaticR :: [Text] ->  Handler TypedContent
      getStaticR sp' = do
        path' <- fmap (staticPath.config) getYesod
        let sp = ws2s sp'
        let path = path' ++ t2s sp
        fileExist path $ do
          fileMIME <- liftIO $ BIO.readFile $ path ++ ".mime"
          sendFile fileMIME path
        where
          fileExist path doItem = do
            isE <- liftIO $ doesFileExist path
            if isE
              then doItem
              else notFound
