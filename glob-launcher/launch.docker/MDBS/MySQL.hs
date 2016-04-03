




-- src/MDBS/MySQL.hs

{-# LANGUAGE TemplateHaskell
           , CPP
           #-}

module MDBS.MySQL
    ( dbCon
    , dbField
    , dbStmt
    , mkDbConfig
    , mkSSLI
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
        , "dsKey"
        , "dsCert"
        , "dsCA"
        , "dsCAPath"
        , "dsCiphers"
        ]

      dbField :: [FieldExp]
      dbField = map (mkFieldExp "MySQL Connection Settings")
        [ ("dbPort","DB_PORT","The environment variable's name of the database's port")
        , ("dbAddr","DB_ADDR","The environment variable's name of the database's address")
        , ("dbPsk","DB_PSK","The environment variable's name of the password of database")
        , ("dbName","DB_NAME","The environment variable's name of the name of the database")
        , ("dbUsr","DB_USR","The environment variable's name of the database's user name")
        , ("dbConThd","DB_CONTHD","The environment variable's name of the limitation of the connection with db")
        , ("dbPath","DB_PATH","The environment variable's name of the connection's path")
        , ("dsKey","DS_KEY","The environment variable's name of the connection's ssl's key's path")
        , ("dsCert","DS_CERT","The environment variable's name of the connection's ssl's cert-file's path")
        , ("dsCA","DS_CA","The environment variable's name of the connection's ssl's ca")
        , ("dsCAPath","DS_CAPATH","The environment variable's name of the connection's ssl's ca's path")
        , ("dsCiphers","DS_CIPHERS","The environment variable's name of the connection's ssl's ciphers")
        ]
      dbStmt :: [Stmt]
      dbStmt = map mkBindS
          [ "dbAddr"
          , "dbPort"
          , "dbName"
          , "dbPsk"
          , "dbUsr"
          , "dbConThd"
          , "dsKey"
          , "dsCert"
          , "dsCA"
          , "dsCAPath"
          , "dsCiphers"
          ]
        ++ [ BindS (VarP (mkName ("dbPath'"))) (InfixE (Just (mkVarE "lookupEnv")) (mkVarE "$") (Just (AppE (mkVarE "dbPath") (mkVarE "x"))))
           , LetS [FunD (mkName "ds") [Clause [] (NormalB  (AppE (ConE (mkName "Just")) (AppE {-4-}
              (AppE {-3-}(AppE {-2-}(AppE {-1-}
                (AppE (mkVarE "mkSSLI") (mkVarE "dsKey'") ){-1-}
              (mkVarE "dsCert'")){-2-}(mkVarE "dsCA'")){-3-} (mkVarE "dsCAPath'")){-4-}
             (mkVarE "dsCiphers'"))) )   []   ] ]
           , LetS [FunD (mkName "dbS") [Clause [] (NormalB (AppE
           {-7-}(AppE
             {-6-}(AppE
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
                    (AppE (mkVarE "read") (mkVarE "dbConThd'")) ){-6-}
                  (mkVarE "dbPath'")){-7-}
                 (mkVarE "ds")  )) [] ]]
        ]
