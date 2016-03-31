




-- src/MDBS/Postgres.hs

{-# LANGUAGE TemplateHaskell
           #-}

module MDBS.Postgres
    ( dbCon
    , dbField
    , dbStmt
    ) where

      import Language.Haskell.TH
      import Language.Haskell.TH.Syntax


      dbCon :: [VarStrictType]
      dbCon =
        [ (mkName "dbPort",NotStrict, ConT (mkName "String"))
        , (mkName "dbAddr",NotStrict, ConT (mkName "String"))
        , (mkName "dbName",NotStrict, ConT (mkName "String"))
        , (mkName "dbPsk",NotStrict, ConT (mkName "String"))
        , (mkName "dbUsr",NotStrict, ConT (mkName "String"))
        , (mkName "dbConThd",NotStrict, ConT (mkName "String"))
        ]

      dbField :: [FieldExp]
      dbField =
        [ ((mkName "dbPort"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "DB_PORT")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The environment variable's name of the database's port"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "dbport"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Database Settings")))))
        , ((mkName "dbAddr"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "DB_ADDR")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The environment variable's name of the database's address"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "dbaddr"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Database Settings")))))
        , ((mkName "dbPsk"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "DB_PSK")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The environment variable's name of the password of database"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "dbpsk"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Database Settings")))))
        , ((mkName "dbName"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "DB_NAME")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The environment variable's name of the name of the database"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "dbname"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Database Settings")))))
        , ((mkName "dbUsr"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "DB_USR")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The environment variable's name of the database's user name"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "dbusr"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Database Settings")))))
        , ((mkName "dbConThd"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "DB_CONTHD")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The environment variable's name of the limitation of the connection with db"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "dbdbconthd"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Database Settings")))))
        ]
      dbStmt :: [Stmt]
      dbStmt =
        [ BindS (VarP (mkName "dbAddr'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "dbAddr")) (VarE (mkName "x")))))
        , BindS (VarP (mkName "dbPort'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "dbPort")) (VarE (mkName "x")))))
        , BindS (VarP (mkName "dbName'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "dbName")) (VarE (mkName "x")))))
        , BindS (VarP (mkName "dbPsk'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "dbPsk")) (VarE (mkName "x")))))
        , BindS (VarP (mkName "dbUsr'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "dbUsr")) (VarE (mkName "x")))))
        , BindS (VarP (mkName "dbConThd'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "dbConThd")) (VarE (mkName "x")))))
        , LetS [FunD (mkName "dbS") [Clause [] (NormalB (AppE
          {-5-}(AppE
            {-4-}(AppE
              {-3-}(AppE
                {-2-}(AppE
                  {-1-}(AppE (ConE (mkName "DbConfig"))
                             (VarE (mkName "dbAddr'"))){-1-}
                       (VarE (mkName "dbPort'"))){-2-}
                     (VarE (mkName "dbUsr'"))){-3-}
                   (VarE (mkName "dbPsk'"))){-4-}
                 (VarE (mkName "dbName'"))){-5-}
                (AppE (VarE (mkName "read")) (VarE (mkName "dbConThd'"))) )   ) [] ]]
        ]
