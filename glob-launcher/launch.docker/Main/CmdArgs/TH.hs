




-- src/Main/CmdArgs/TH.hs

{-# LANGUAGE TemplateHaskell #-}

module Main.CmdArgs.TH
    ( baseCmdD
    , tlsCon
    , baseCmd
    , tlsField
    , tocfg
    , tocfgTls
    , tocfgTlsAppE
    , tocfgNoTlsAppE
    , mkVarE
    , mkFieldExp
    , mkLitStrE
    , mkVarNotStrictT
    , mkBindS
    ) where

      import Language.Haskell.TH
      import Language.Haskell.TH.Syntax
      import Glob.Foundation.Common
      import System.Console.CmdArgs.Default

      mkVarE :: String -> Exp
      mkVarE x = VarE $ mkName x

      mkLitStrE :: String -> Exp
      mkLitStrE x = LitE $ StringL x

      mkVarNotStrictT :: String -- Type: mkName typ
                      -> String -- name of variable
                      -> VarStrictType
      mkVarNotStrictT typ name = (mkName name,NotStrict,ConT (mkName typ))

      mkFieldExp :: String -- groupname
                 -> ( String -- var
                    , String -- help
                    , String -- def
                    )
                 -> FieldExp
      mkFieldExp gT (vT,dT,hT) =
        (mkName vT, InfixE
            (Just (InfixE
              (Just (InfixE
                  (Just (mkLitStrE dT))
                  (mkVarE "&=")
                  (Just (AppE (mkVarE "help")
                              (mkLitStrE hT)))))
              (mkVarE "&=")
              (Just (AppE (mkVarE "name")
                          (mkLitStrE vT)))))
            (mkVarE "&=")
            (Just (AppE (mkVarE "groupname")
                        (mkLitStrE gT))))

      mkBindS :: String
              -> Stmt
      mkBindS vT = BindS (VarP (mkName (vT++"'"))) (InfixE (Just (mkVarE "getEnv")) (mkVarE "$") (Just (AppE (mkVarE vT) (mkVarE "x"))))

      mkBindSMaybe :: String
                   -> Stmt
      mkBindSMaybe vT = BindS (VarP (mkName (vT++"'"))) (InfixE (Just (mkVarE "lookupEnv")) (mkVarE "$") (Just (AppE (mkVarE vT) (mkVarE "x"))))


      baseCmdD :: [VarStrictType] -> [Dec]
      baseCmdD con =
        [ DataD
            []
            (mkName "DockerLaunch")
            []
            [ RecC (mkName "DockerLaunch")
                   (map (mkVarNotStrictT "String") ["port","staticPath","siteTitle","tokenEnv","logfile"] ++ con)
            ]
            (map mkName ["Show","Typeable","Data"])
        ]

      tlsCon :: [VarStrictType]
      tlsCon = map (mkVarNotStrictT "String") ["certPath","keyPath"]

      baseCmd :: [FieldExp] -> [Dec]
      baseCmd fe =
        [ ValD
            (VarP (mkName "dockerLaunch"))
            (NormalB
              (InfixE (Just --c
                  (InfixE (Just --b
                      (InfixE (Just --a
                          (InfixE (Just (RecConE (mkName "DockerLaunch") fes))
                                  (mkVarE "&=")
                                  (Just (AppE (mkVarE "program")
                                              (mkLitStrE "launch.docker")))))
                        (mkVarE "&=")
                        (Just (AppE (mkVarE "details")
                                    (ListE [mkLitStrE "This is the launcher of glob, when docker used"])))))
                      (mkVarE "&=")
                      (Just (AppE
                        (mkVarE "summary")
                        (InfixE
                          (Just (mkLitStrE "Glob's launcher for docker"))
                          (mkVarE "++")
                          (Just (InfixE
                            (Just (AppE (mkVarE "showVersion")
                                        (mkVarE "version")))
                            (mkVarE "++")
                            (Just (InfixE
                              (Just (mkLitStrE ",(C) Qinka 2016"))
                              (mkVarE "++")
                              (Just (InfixE (Just (mkLitStrE "\n Used "))
                                            (mkVarE "++")
                                            (Just (mkLitStrE $(globVersion))))))))))))))
                (mkVarE "&=")
                (Just (mkVarE "verbosity"))))
            []
        ]
        where
          fes =  map (mkFieldExp "Glob Settings")
            [ ("port","GLOB_PORT","The enviroment variable's name of this website's port")
            , ("staticPath","STATIC_PATH","The enviroment variable's name of this website's static files' path")
            , ("siteTitle","SITE_TITLE","The enviroment variable's name of this website's title")
            , ("tokenEnv","GLOB_BACKGROUND_TOKEN","The enviroment variable's name of this site's token")
            , ("logfile","GLOB_LOG_PATH","The enviroment variable's name of website's log file path, or stdin stdout")
            ] ++ fe



      tlsField :: [FieldExp]
      tlsField = map (mkFieldExp "Glob Settings of Tls")
        [ ("certPath","CERTIFICATE_PATH","The enviroment variable's name of website's certificate file's path")
        , ("keyPath","KEY_PATH","The enviroment variable's name of website's key file's path")
        ]

      tocfg :: (Exp -> Exp) -> [Stmt] -> [Dec]
      tocfg rtE stmt =
        [ SigD (mkName "toConfig") (AppT (AppT ArrowT (ConT (mkName "DockerLaunch"))) (AppT (ConT (mkName "IO")) (ConT (mkName "GlobConfig"))))
        , FunD (mkName "toConfig") [Clause [VarP (mkName "x")] (NormalB (DoE ( map mkBindS
            [ "port"
            , "staticPath"
            , "siteTitle"
            , "logfile"
            ]
          ++ [LetS [FunD (mkName "token") [Clause [] (NormalB (AppE (mkVarE "tokenEnv") (mkVarE "x"))) [] ]]]
          ++ stmt
          ++ [ NoBindS (AppE (mkVarE "return") (rtE mkGC) )]
          ))) []]
        ]
        where
          mkGC = AppE {-5-}(AppE {-4-}(AppE {-3-}(AppE {-2-} (AppE {- 1 -}
            (AppE (ConE (mkName "GlobConfig")) (AppE (mkVarE "read") (mkVarE "port'") ) ) {- 1 -} (mkVarE "dbS") ) {-2-} (mkVarE "siteTitle'") ){-3-} (mkVarE "token")){-4-} (mkVarE  "staticPath'")){-5-} (mkVarE "logfile'")

      tocfgTls :: [Stmt]
      tocfgTls = map mkBindS ["certPath","keyPath"]

      tocfgTlsAppE :: Exp -> Exp
      tocfgTlsAppE expr = AppE (AppE expr (mkVarE "certPath'")) (mkVarE "keyPath'")

      tocfgNoTlsAppE :: Exp -> Exp
      tocfgNoTlsAppE expr = AppE (AppE expr (mkLitStrE "")) (mkLitStrE "")
