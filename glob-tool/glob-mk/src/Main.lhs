% src/Main.lhs

\begin{codeinfo}
  \CodePath{src/Main.lhs}
  \CodeProject{glob-auth}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-08-09}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
\end{codeinfo}

\begin{code}
module Main
    ( main
    ) where

      import Data.Char
      import Data.Time
      import System.Console.CmdArgs
      import System.Console.CmdArgs.Verbosity
      import System.IO

      import Data.Version
      import Paths_glob_mk
\end{code}

for cmd args
\begin{code}
      data Maker = MkBase
        | MkHtmlFramePut
          { hfpTagerName  :: String
          , hfpTagerUrl   :: String
          , hfpDps        :: [String]
          , hfpHtmlFile   :: String
          , hfpUpdateTime :: Maybe String
          }
        {-| MkHtmlFrameDel
          { hfdTagerName  :: String
          , hfdTargeUrl   :: String
          , htdDps        :: [String]
          -}
        deriving (Show, Data, Typeable)
\end{code}

Maker
\begin{code}
      mk = modes [mkBase,mkHtmlFramePut]
        &= summary ("Glob Makefiler "++showVersion version)
        &= verbosity
        &= versionArg [summary (showVersion version)]
      mkBase = MkBase &= explicit &= name "base"
      mkHtmlFramePut = MkHtmlFramePut
        { hfpTagerName = def
            &= typ "TAG_NAME"
            &= argPos 0
        , hfpTagerUrl = def
            &= typ "URL"
            &= argPos 1
        , hfpHtmlFile = def
            &= typFile
            &= argPos 2
        , hfpDps = def
            &= typ "TAG_NAMEs"
            &= help "the target which is needed"
            &= explicit &= name "dps"
            &= name "d"
        , hfpUpdateTime = def
            &= typ "TIME|$(NOW)"
            &= help "Update time"
            &= opt (Just "$(NOW)")
            &= explicit &= name "update-time"
            &= name "u"
        } &= explicit &= name "putframe"
\end{code}

short
\begin{code}
      loud = whenLoud.hPutStrLn stderr
      normal = whenNormal.hPutStrLn stderr
      quite = hPutStrLn stderr
\end{code}

the main
\begin{code}
      main :: IO ()
      main = do
        rt <- cmdArgs mk
        case rt of
          MkBase -> do
            loud "Begin Create Makefile"
            loud "Create Makefile to stdout"
            loud "the head of Makefile"
            putStrLn $ unlines
              [ replicate 66 '#'
              , "# The Makefile to update or upload the files of Glob"
              , "# Create by Glob Makefiler: glob-mk"
              , replicate 66 '#'
              , ""
              , "# The settings"
              , ""
              , "# the curl's path of name"
              , "CURL_TOOL=\"curl \""
              , "# the shell to do the cmds"
              , "SHELL=bash"
              , "# let curl show the detail"
              , "DETAIL=\' -i \'"
              , "# URL of site"
              , "URL=http://localhost:3000"
              , "# The password"
              , "PSK=1024"
              , "# the backgroud's url"
              , "BG=bg"
              , "# the fix-time"
              , "DTIME=0"
              , "# the set of ih tool"
              , "IH=glob-ih -m -v -f$(DTIME) # use new glob-ih >= 0.0.9.25"
              , "# hoe to get now"
              , "NOW=$$(glob-ih -t -f$(DTIME))"
              , ""
              ]
            loud "out put end"
          MkHtmlFramePut{..} -> do
            loud "begin to create part of updating"
            loud $ "target:" ++ hfpTagerName
            loud $ "dps:" ++ unwords hfpDps
            putStrLn $ hfpTagerName ++ ": " ++ unwords hfpDps
            loud "precompiler => null"
            loud "update time"
            now <- case hfpUpdateTime of
              Nothing -> do
                loud "getCurrtentTime"
                show <$> getCurrentTime
              Just x  -> do
                loud "use input"
                return x
            loud "update (echo)"
            putStrLn.unlines $ map ('\t':)
              [ "@echo $(CURL_TOOL) $(DETAIL) \' -X PUT -F \"type=frame\" \' \\"
              , "\t\' -F \"update-time="++now++"\" -F \"html=@"++hfpHtmlFile++"\" \' \\"
              , "\t$(URL)/$(BG)/frame"++hfpTagerUrl++" \' \' | $(IH) $(PSK) | $(SHELL)"
              ]
          _ -> quite "What ?"
\end{code}
