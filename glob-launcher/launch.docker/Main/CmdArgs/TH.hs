




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
    ) where

      import Language.Haskell.TH
      import Language.Haskell.TH.Syntax
      import Glob.Foundation.Common
      import System.Console.CmdArgs.Default

      mkVarNotStrictT :: String -- Type: mkName typ
                      -> String -- name of variable
                      -> VarStrictType
      mkVarNotStrictT typ name = (mkName name,NotStrict,ConT (mkName typ))

      mkFieldExp :: String -- groupname
                 -> String -- var
                 -> String -- help
                 -> String -- def
                 -> FieldExp
      mkFieldExp gT vT hT dT =
        (mkName vT, InfixE
            (Just (InfixE
              (Just (InfixE
                  (Just (LitE (StringL dT)))
                  (VarE (mkName "&="))
                  (Just (AppE (VarE (mkName "help"))
                              (LitE (StringL hT))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "name"))
                          (LitE (StringL vT))))))
            (VarE (mkName "&="))
            (Just (AppE (VarE (mkName "groupname"))
                        (LitE (StringL gT)))))
      mkBindS ::

      baseCmdD :: [VarStrictType] -> [Dec]
      baseCmdD con =
        [ DataD
            []
            (mkName "DockerLaunch")
            []
            [ RecC (mkName "DockerLaunch")
                ([ (mkName "port",NotStrict, ConT (mkName "String"))
                , (mkName "staticPath",NotStrict, ConT (mkName "String"))
                , (mkName "siteTitle",NotStrict, ConT (mkName "String"))
                , (mkName "tokenEnv",NotStrict, ConT (mkName "String"))
                , (mkName "logfile",NotStrict, ConT (mkName "String"))
              --, (mkName "",NotStrict, ConT (mkName ""))
                ] ++ con)
            ]
            [ mkName "Show"
            , mkName "Typeable"
            , mkName "Data"
            ]
        ]

      tlsCon :: [VarStrictType]
      tlsCon =
        [ (mkName "certPath",NotStrict, ConT (mkName "String"))
        , (mkName "keyPath",NotStrict, ConT (mkName "String"))
        ]

      baseCmd :: [FieldExp] -> [Dec]
      baseCmd fe =
        [ ValD
            (VarP (mkName "dockerLaunch"))
            (NormalB
              (InfixE (Just --c
                  (InfixE (Just --b
                      (InfixE (Just --a
                          (InfixE (Just (RecConE (mkName "DockerLaunch") fes))
                                  (VarE (mkName "&="))
                                  (Just (AppE (VarE (mkName "program"))
                                              (LitE (StringL "launch.docker"))))))
                        (VarE (mkName "&="))
                        (Just (AppE (VarE (mkName "details"))
                                    (ListE [LitE (StringL "This is the launcher of glob, when docker used")])))))
                      (VarE (mkName "&="))
                      (Just (AppE
                        (VarE (mkName "summary"))
                        (InfixE
                          (Just (LitE (StringL "Glob's launcher for docker")))
                          (VarE (mkName "++"))
                          (Just (InfixE
                            (Just (AppE (VarE (mkName "showVersion"))
                                        (VarE (mkName "version"))))
                            (VarE (mkName "++"))
                            (Just (InfixE
                              (Just (LitE (StringL ",(C) Qinka 2016")))
                              (VarE (mkName "++"))
                              (Just (InfixE (Just (LitE (StringL "\n Used ")))
                                            (VarE (mkName "++"))
                                            (Just (LitE (StringL $(globVersion)))))))))))))))
                (VarE (mkName "&="))
                (Just (VarE (mkName "verbosity")))))
            []
        ]
        where
          fes =
              [ ((mkName "port"), InfixE
                    (Just (InfixE
                      (Just (InfixE
                          (Just (LitE (StringL "GLOB_PORT")))
                          (VarE (mkName "&="))
                          (Just (AppE (VarE (mkName "help"))
                                      (LitE (StringL "The enviroment variable's name of this website's port"))))))
                      (VarE (mkName "&="))
                      (Just (AppE (VarE (mkName "name"))
                                  (LitE (StringL "port"))))))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "groupname"))
                                (LitE (StringL "Glob Settings")))))
              , ((mkName "staticPath"), InfixE
                    (Just (InfixE
                      (Just (InfixE
                          (Just (LitE (StringL "STATIC_PATH")))
                          (VarE (mkName "&="))
                          (Just (AppE (VarE (mkName "help"))
                                      (LitE (StringL "The enviroment variable's name of this website's static files' path"))))))
                      (VarE (mkName "&="))
                      (Just (AppE (VarE (mkName "name"))
                                  (LitE (StringL "staticpath"))))))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "groupname"))
                                (LitE (StringL "Glob Settings")))))
              , ((mkName "siteTitle"), InfixE
                    (Just (InfixE
                      (Just (InfixE
                          (Just (LitE (StringL "SITE_TITLE")))
                          (VarE (mkName "&="))
                          (Just (AppE (VarE (mkName "help"))
                                      (LitE (StringL "The enviroment variable's name of this website's title"))))))
                      (VarE (mkName "&="))
                      (Just (AppE (VarE (mkName "name"))
                                  (LitE (StringL "sitetitle"))))))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "groupname"))
                                (LitE (StringL "Glob Settings")))))
              , ((mkName "tokenEnv"), InfixE
                    (Just (InfixE
                      (Just (InfixE
                          (Just (LitE (StringL "GLOB_BACKGROUND_TOKEN")))
                          (VarE (mkName "&="))
                          (Just (AppE (VarE (mkName "help"))
                                      (LitE (StringL "The enviroment variable's name of this site's token"))))))
                      (VarE (mkName "&="))
                      (Just (AppE (VarE (mkName "name"))
                                  (LitE (StringL "tokenenv"))))))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "groupname"))
                                (LitE (StringL "Glob Settings")))))
              , ((mkName "logfile"), InfixE
                    (Just (InfixE
                      (Just (InfixE
                          (Just (LitE (StringL "GLOB_LOG_PATH")))
                          (VarE (mkName "&="))
                          (Just (AppE (VarE (mkName "help"))
                                      (LitE (StringL "The enviroment variable's name of website's log file path, or stdin stdout"))))))
                      (VarE (mkName "&="))
                      (Just (AppE (VarE (mkName "name"))
                                  (LitE (StringL "logfile"))))))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "groupname"))
                                (LitE (StringL "Glob Settings")))))
              {-, ((mkName "port"), InfixE
                    (Just (InfixE
                      (Just (InfixE
                          (Just (LitE (StringL "def")))
                          (VarE (mkName "&="))
                          (Just (AppE (VarE (mkName "help"))
                                      (LitE (StringL "The"))))))
                      (VarE (mkName "&="))
                      (Just (AppE (VarE (mkName "name"))
                                  (LitE (StringL "port"))))))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "groupname"))
                                (LitE (StringL "Glob..")))))-}
              ] ++ fe



      tlsField :: [FieldExp]
      tlsField =
        [ ((mkName "certPath"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "CERTIFICATE_PATH")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The enviroment variable's name of website's certificate file's path"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "certpath"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Glob Settings of Tls")))))
        , ((mkName "keyPath"), InfixE
              (Just (InfixE
                (Just (InfixE
                    (Just (LitE (StringL "KEY_PATH")))
                    (VarE (mkName "&="))
                    (Just (AppE (VarE (mkName "help"))
                                (LitE (StringL "The enviroment variable's name of website's key file's path"))))))
                (VarE (mkName "&="))
                (Just (AppE (VarE (mkName "name"))
                            (LitE (StringL "keypath"))))))
              (VarE (mkName "&="))
              (Just (AppE (VarE (mkName "groupname"))
                          (LitE (StringL "Glob Settings of Tls")))))
        ]

      tocfg :: (Exp -> Exp) -> [Stmt] -> [Dec]
      tocfg rtE stmt =
        [ SigD (mkName "toConfig") (AppT (AppT ArrowT (ConT (mkName "DockerLaunch"))) (AppT (ConT (mkName "IO")) (ConT (mkName "GlobConfig"))))
        , FunD (mkName "toConfig") [Clause [VarP (mkName "x")] (NormalB (DoE (
          [ BindS (VarP (mkName "port'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "port")) (VarE (mkName "x")))))
          , BindS (VarP (mkName "staticPath'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "staticPath")) (VarE (mkName "x")))))
          , BindS (VarP (mkName "siteTitle'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "siteTitle")) (VarE (mkName "x")))))
          , BindS (VarP (mkName "logfile'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "logfile")) (VarE (mkName "x")))))
          , LetS [FunD (mkName "token") [Clause [] (NormalB (AppE (VarE (mkName "tokenEnv")) (VarE (mkName "x")))) [] ]]
        --, BindS (VarP (mkName "port'") (InfixE (Just (VarE (mkName "getEnv"))) (Var (mkName "$")) (Just (AppE (VarE (mkName "port")) (VarE (mkName "x"))))))
          ]++stmt++
          [ NoBindS (AppE (VarE (mkName "return")) (rtE mkGC) )
          ])
          )) []]]
        where
          mkGC = AppE {-5-}(AppE {-4-}(AppE {-3-}(AppE {-2-} (AppE {- 1 -}
            (AppE (ConE (mkName "GlobConfig")) (AppE (VarE (mkName "read")) (VarE (mkName "port'")) ) ) {- 1 -} (VarE (mkName "dbS")) ) {-2-} (VarE (mkName "siteTitle'")) ){-3-} (VarE (mkName "token"))){-4-} (VarE (mkName "staticPath'"))){-5-} (VarE (mkName "logfile'"))

      tocfgTls :: [Stmt]
      tocfgTls =
        [ BindS (VarP (mkName "certPath'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "certPath")) (VarE (mkName "x")))))
        , BindS (VarP (mkName "keyPath'")) (InfixE (Just (VarE (mkName "getEnv"))) (VarE (mkName "$")) (Just (AppE (VarE (mkName "keyPath")) (VarE (mkName "x")))))
        ]

      tocfgTlsAppE :: Exp -> Exp
      tocfgTlsAppE expr = AppE (AppE expr (VarE (mkName "certPath'"))) (VarE (mkName "keyPath'"))

      tocfgNoTlsAppE :: Exp -> Exp
      tocfgNoTlsAppE expr = AppE (AppE expr (LitE (StringL ""))) (LitE (StringL ""))
