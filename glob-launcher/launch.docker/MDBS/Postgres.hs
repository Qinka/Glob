




-- src/MDBS/Postgres.hs

module MDBS.Postgres
    ( dbCon
    , dbField
    , dbStmt
    , mkDbConfig
    ) where

      import Language.Haskell.TH
      import Language.Haskell.TH.Syntax
      import Main.CmdArgs.TH
      import Glob.MDBS


      dbCon :: [VarStrictType]
      dbCon = map (mkVarNotStrictT "String")
        [ "dbPort"
        , "dbAddr"
        , "dbName"
        , "dbPsk"
        , "dbUsr"
        , "dbConThd"
        ]

      dbField :: [FieldExp]
      dbField = map (mkFieldExp "PostgreSQL Connection Settings")
        [ ("dbPort","DB_PORT","The environment variable's name of the database's port")
        , ("dbAddr","DB_ADDR","The environment variable's name of the database's address")
        , ("dbPsk","DB_PSK","The environment variable's name of the password of database")
        , ("dbName","DB_NAME","The environment variable's name of the name of the database")
        , ("dbUsr","DB_USR","The environment variable's name of the database's user name")
        , ("dbConThd","DB_CONTHD","The environment variable's name of the limitation of the connection with db")
        ]
      dbStmt :: [Stmt]
      dbStmt = map mkBindS
          [ "dbAddr"
          , "dbPort"
          , "dbName"
          , "dbPsk"
          , "dbUsr"
          , "dbConThd"
          ]
        ++ [LetS [FunD (mkName "dbS") [Clause [] (NormalB (AppE
          {-5-}(AppE
            {-4-}(AppE
              {-3-}(AppE
                {-2-}(AppE
                  {-1-}(AppE (mkVarE "mkDbConfig")
                             (mkVarE "dbAddr'")){-1-}
                       (mkVarE "dbPort'")){-2-}
                     (mkVarE "dbUsr'")){-3-}
                   (mkVarE "dbPsk'")){-4-}
                 (mkVarE "dbName'")){-5-}
                (AppE (mkVarE "read") (mkVarE "dbConThd'")) )   ) [] ]]
        ]
