




-- src/Glob/Sub/Background/Data.hs

{-# LANGUAGE TemplateHaskell
           , TypeFamilies
           , OverloadedStrings
           #-}

module Glob.Sub.Background.Data where

      import Import
      import Glob.MDBS
      import Glob.Foundation.Config
      import Import.TH
      import Yesod

      data Background = Background
        { bgConnPool :: ConnectionPool
        , bgGlobConfig :: GlobConfig
        }

      mkYesodSubData "Background" $(parseRoutesFileC "background.route")
