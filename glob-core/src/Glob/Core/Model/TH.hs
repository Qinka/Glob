{-|
Module        : Glob.Core.Model.TH
Description   : To generate the codes about module
Copyright     : (C) Qinka, 2017
Maintainer    : me@qinka.pro
License       : GPL3
Stability     : experimental
Portability   : unknown

The codes for generate the codes about modules
-}


{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Glob.Core.Model.TH
       ( make_fetch
       , make_update
       ) where

import           Control.Monad.IO.Class
import           Database.MongoDB         as Mongo
import           Glob.Core.Model.Internal
import           Glob.Import.TH


-- | To create fetch_xxxx
make_fetch :: Name   -- ^ filter   name
           -> String -- ^ function name
           -> Name   -- ^ type
           -> String -- ^ field name
           -> String -- ^ collection
           -> Q [Dec]
make_fetch func n kind field collection = do
  let name = mkName $ "fetch_" ++ n
      m    = mkName  "m"
      dec = SigD name $ ForallT [PlainTV m] [AppT (ConT ''MonadIO) (VarT m)]
        (AppT
         (AppT ArrowT (ConT ''ResT))
         (AppT (AppT (ConT ''Action) (VarT m)) (AppE (ConT ''Maybe) (ConT kind))))
      resv = mkName "res"
      body = FunD name
        [Clause [VarP resv] (NormalB $
                             (AppE (VarE $ mkName "<#>")
                              (AppE (VarE func)
                               (AppE (AppE (AppE (VarE 'fetch_context)
                                            (LitE $ StringL field))
                                      (VarE resv))
                                (LitE $ StringL collection))))) []]
  return [dec,body]


-- | to create update_xxx
make_update :: String -- ^ function name
            -> Name   -- ^ type
            -> String -- ^ field
            -> String -- ^ collection
            -> Q [Dec]
make_update n kind f c = do
  let field = LitE $ StringL f
      coll  = LitE $ StringL c
      name  = mkName $ "update_" ++ n
      m     = mkName "m"
      dec   = SigD name $ ForallT [PlainTV m] [AppT (ConT ''MonadIO) (VarT m)]
        (AppT
         (AppT ArrowT (ConT kind))
         (AppT (AppT ArrowT (ConT ''ResT))
          (AppT (AppT (ConT ''Action) (VarT m)) (ConT ''()))))
      body  = FunD name
        [Clause [] (NormalB $
                    (AppE (AppE (VarE 'update_item) field)
                     coll)) []]
  return [dec,body]
