




-- src/Management/TH.hs
{-# LANGUAGE TemplateHaskell
           , QuasiQuotes
           , OverloadedStrings
           #-}

module Glob.Management.TH
    ( ent
    , a2o
    , mkPLFunc
    ) where

      import Prelude as P
      import Language.Haskell.TH
      import Yesod

      ent :: [Entity x] -> [x]
      ent = P.map ent'
        where
          ent' (Entity _ x) = x

      mkPLFunc :: String -> Q Exp
      mkPLFunc n = [e| (a2o. P.map (\x -> [ $(conE $ mkName n) ==. x ])) |]

      a2o :: [[Filter v]] -> [Filter v]
      a2o  = P.foldr (||.) []
