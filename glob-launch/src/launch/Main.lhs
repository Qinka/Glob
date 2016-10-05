% src/launch/Main.lhs

\begin{codeinfo}
  \CodePath{src/launch/Main.lhs}
  \CodeInfo{The main module of the launcher of Glob}
  \CodeProject{glob-launch}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-25}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.1}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

Main
\begin{code}
module Main
       ( main
       ) where

import System.Console.CmdArgs
import Glob.Common
import Glob.Core
import Glob.Launch

import System.IO
import qualified GHC.IO.Encoding as E
\end{code}

cmdargs'
\begin{code}
data Launch = Launch {from :: String}
            deriving (Eq,Show,Data,Typeable)
launch = Launch
  { from = "stdin"
    &= typ "[stdin|FILEPATH]"
    &= help "The configure file glob loaded."
  }
  &= verbosity
  &= help "You can turn on '+RTS -N' to let glob run faster."
  &= summary ( " glob-core-"
               ++ $(globCoreVersionQuote)
               ++ ";glob-launch-"
               ++ $(globLaunchVersionQuote)
               ++ $(globCoreBuildInfo)
             )
fromLaunch :: Launch -> String
fromLaunch (Launch fp) = fp
\end{code}

\begin{code}
main :: IO ()
main = do
#ifdef WithUTF8
  E.setLocaleEncoding E.utf8
  hSetEncoding stdout utf8
#endif
  cfg' <- readFromFile =<< fromLaunch <$> cmdArgs launch :: IO (Maybe GlobCfg)
  case cfg' of
    Just cfg -> runToConfig cfg
    _ -> putStrLn "fail to parse config file from JSON or YAML."
  return ()
\end{code}
