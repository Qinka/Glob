




-- src/Glob/Database.hs

{-# LANGUAGE TemplateHaskell
           , FlexibleInstances
           , TypeFamilies
           , MultiParamTypeClasses
           , GADTs
           , GeneralizedNewtypeDeriving
           #-}

module Glob.Database where

      import Prelude hiding (String)
      import Import.Text as T
      import Yesod
      import Glob.MDBS
      import Import.TH
      import Import
      import Import.ByteString as B

      instance FromJSON B.ByteString where
        parseJSON (String x) = pure $ encodeUtf8 x
      instance ToJSON B.ByteString where
        toJSON = String . decodeUtf8

      share shareEntitDef $(persistFileWithC lowerCaseSettings "database")
