% src/Glob/Common/Infos.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Common/Infos.lhs}
  \CodeInfo{The infos use in Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-10-28}
\end{codeinfo}

Haskell Extension
\begin{code}
{-# LANGUAGE CPP #-}
\end{code}

\begin{code}
module Glob.Common.Infos
       ( globCoreVersion
       , globCoreVersionQuote
       , globCoreBuildInfo
       , isDebug
       , globGitBranchQuote
       , globGitCommitQuote
       ) where


import Import
import Import.TH

import Data.Version
import Paths_glob_core

import Data.Char
import Development.GitRev
import System.Info
\end{code}


the build time of Glob
\begin{code}
globCoreBuildInfo :: Q Exp
globCoreBuildInfo = do
  timeStr <- formatTime defaultTimeLocale "-%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  (LitE (StringL globCm)) <- globGitCommitQuote 
  stringE $ os ++ "-" ++ arch ++ "-"  ++ map toUpper compilerName
    ++ "-" ++ showVersion compilerVersion ++ timeStr
    ++ "-git:" ++ $gitBranch ++ ":" ++ globCm
    ++ debugInfo
  where
    debugInfo = if isDebug then "-debug" else ""  
\end{code}


the version of glob-core
\begin{code}
globCoreVersion :: Version
globCoreVersion = version
globCoreVersionQuote :: Q Exp
globCoreVersionQuote = stringE $ showVersion version
\end{code}

the commit hash of current
\begin{code}
globGitCommitQuote :: Q Exp
globGitCommitQuote =
  if not $gitDirty
    then gitHash
    else do
      now <- runIO $ getCurrentTime
      stringE $ show now
    
\end{code}

the branch name of current

\begin{code}
globGitBranchQuote :: Q Exp
globGitBranchQuote = gitBranch
\end{code}


Debug? Or not
\begin{code}
isDebug :: Bool
isDebug =
#ifdef DEBUG
  True
#else
  False
#endif
\end{code}
