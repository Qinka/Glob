




-- src/Main/CmdArgs.hs

{-# LANGUAGE DeriveDataTypeable
           , TemplateHaskell
           , CPP
           #-}

module Main.CmdArgs
    ( dl
    , dlD
    , tocfgi
    ) where

      import System.Console.CmdArgs
      import Main.CmdArgs.TH
      import Language.Haskell.TH
      import Paths_glob_launcher
      import Data.Version(showVersion)
      import Glob.Foundation.Config(GlobConfig)
      import Glob.Foundation.Common(globVersion)
      import MDBS

      dlD :: Q [Dec]
      dlD = return $ baseCmdD ext
        where
#ifdef WithTls
          ext = dbCon ++ tlsCon
#else
          ext = dbCon
#endif
      dl :: Q [Dec]
      dl = return $ baseCmd ext
        where
#ifdef WithTls
          ext = dbField ++ tlsField
#else
          ext = dbField
#endif
      tocfgi :: Q [Dec]
      tocfgi = return $ tocfg fc stmts
        where
#ifdef WithTls
          fc = tocfgTlsAppE
          stmts = tocfgTls ++ dbStmt
#else
          fc = tocfgNoTlsAppE
          stmts = dbStmt
#endif
