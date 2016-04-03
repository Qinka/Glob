




-- src/MDBS/Postgres.hs

{-# LANGUAGE TemplateHaskell
           #-}

module MDBS.Mongo
    ( dbCon
    , dbField
    , dbStmt
    ) where

      import Language.Haskell.TH
      import Language.Haskell.TH.Syntax
      import Main.CmdArgs.TH


      dbCon :: [VarStrictType]
      dbCon = map (mkVarNotStrictT "String")
        [ "dbPort"
        , "dbAddr"
        , "dbName"
        , "dbPsk"
        , "dbUsr"
        , "dbPS"
        , "dbSC"
        ]

      dbField :: [FieldExp]
      dbField = map (mkFieldExp "MongoDB Connection Settings")
        [ ("dbPort","DB_PORT","The environment variable's name of the database's port")
        , ("dbAddr","DB_ADDR","The environment variable's name of the database's address")
        , ("dbPsk","DB_PSK","The environment variable's name of the password of database")
        , ("dbName","DB_NAME","The environment variable's name of the name of the database")
        , ("dbUsr","DB_USR","The environment variable's name of the database's user name")
        , ("dbPS","DB_POOLSTRIPES","The environment variable's name of MongoDB's poolstripes ")
        , ("dbSC","DB_STRIPECONNECTION","The environment variable's name of MongoDB's stripeconnections ")
        ]
      dbStmt :: [Stmt]
      dbStmt = map mkBindS
          [ "dbAddr"
          , "dbPort"
          , "dbName"
          , "dbPsk"
          , "dbUsr"
          , "dbPS"
          , "dbSC"
          ]
        ++ [LetS [FunD (mkName "dbS") [Clause [] (NormalB (AppE {-7-}
          {-6-}(AppE
            {-5-}(AppE
              {-4-}(AppE
                {-3-}(AppE
                  {-2-}(AppE
                    {-1-}(AppE (mkVarE "DbConfig")
                               (mkVarE "dbAddr'")){-1-}
                         (mkVarE "dbPort'")){-2-}
                       (mkVarE "dbUsr'")){-3-}
                     (mkVarE "dbPsk'")){-4-}
                   (mkVarE "dbName'")){-5-}
                  (AppE (mkVarE "read") (mkVarE "dbPS'")){-6-})
                 (AppE (mkVarE "read") (mkVarE "dbSC'")){-7-}) ) [] ]]
        ]
