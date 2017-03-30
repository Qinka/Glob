

\begin{code}
module Glob.Import.Version
       ( module X
       , compile_time
       , compile_os
       , compile_compiler
       ) where

import Data.Version       as X
import Development.GitRev as X
import Glob.Import        as X
import Glob.Import.TH     as X
import System.Info        as X

import Data.Char
\end{code}

to get the compiler time
\begin{code}
compile_time :: Q Exp
compile_time = do
  now <- formatTime defaultTimeLocale "-%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  stringE now
\end{code}

to get the compiler os
\begin{code}
compile_os :: Q Exp
compile_os = stringE $ os ++ "-" ++ arch
\end{code}

to get compiler system
\begin{code}
compile_compiler :: Q Exp
compile_compiler = stringE $ toUpper <$> compilerName ++ "-" ++ showVersion compilerVersion
\end{code}
