




-- src/GLob.hs

{-# LANGUAGE OverloadedStrings
           , TemplateHaskell
           , ViewPatterns
           #-}

module Glob
    ( module Glob
    , module X
    ) where

      import Prelude as P
      import Glob.Config as X
      import Glob.Management as X
      import Glob.Static as X
      import Yesod as X
      import Glob.Handler as X
      import Glob.Data as X

      mkYesodDispatch "Glob" resourcesGlob
