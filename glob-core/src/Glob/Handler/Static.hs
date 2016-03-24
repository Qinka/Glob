




-- src/Glob/Handler/Static.hs

module Glob.Handler.Static
    ( getStaticR
    ) where

      import Import
      import Import.Handler
      import Import.ByteString as B
      import Import.Text as T
      import System.Directory


      getStaticR :: [Text] ->  Handler TypedContent
      getStaticR sp' = do
        path' <- fmap (globStaticPath.globConfig) getYesod
        let sp = ws2s sp'
        let path = path' ++ t2s sp
        fileExist path $ do
          fileMIME <- liftIO $ B.readFile $ path ++ ".mime"
          sendFile fileMIME path
        where
          fileExist path doItem = do
            isE <- liftIO $ doesFileExist path
            if isE
              then doItem
              else notFound
