




-- src/Glob/Foundation/Base.hs

{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , TypeFamilies
           , ViewPatterns
           , OverloadedStrings
           , FlexibleContexts
           #-}

module Glob.Foundation.Base where

      import Prelude as P
      import Glob.Database
      import Yesod
      import Import.TH
      import Import.Monad
      import Import.Text as T
      import Glob.Auth
      import Glob.MDBS
      import Glob.Foundation.Config as X
      import Glob.Foundation.Common as X
      import Glob.Sub.Background

      data Glob = Glob
        { globConnPool :: ConnectionPool
        , globConfig :: GlobConfig
        , globSubBg :: Background
        }

      mkYesodData "Glob" $(parseRoutesFileC "glob.route")

      instance Yesod Glob where
        errorHandler x = selectRep $ do
          provideRepValue x
          provideRep $ defaultLayout [whamlet|#{show x}|]
        isAuthorized (BackgroundR _) _ = getYesod >>= (bgAuth.globTokenEnv.globConfig)
        isAuthorized _ _ = return Authorized
        defaultLayout = globLayout

      instance YesodPersist Glob where
        type YesodPersistBackend Glob = DBBackend
        runDB a = getYesod >>= (runWithPool a.globConnPool)

      globLayout :: Widget -> Handler Html
      globLayout w = do
        globItem <- getYesod
        let globC = globConfig globItem
        pc <- widgetToPageContent w
        topHtml <- selectHtm ["page","frame","top"]
        bottomHtml <- selectHtm ["page","frame","bottom"]
        navHtml <- selectHtm ["page","frame","nav"]
        withUrlRenderer $(hamletFileT "layout.hamlet")
        where
          selectHtm x =entHtm' `liftM`
            liftHandlerT (runDB $ selectList [HtmIndex==. ws2s ("home":x),HtmTyp==."home"] [])
          entHtm (Entity _ htmT) = htmHtml htmT
          entHtm' = P.head .P.map  (preEscapedToHtml.entHtm)
