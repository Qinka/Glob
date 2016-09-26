% import/Import/Logger.lhs

\begin{codeinfo}
  \CodePath{import/Import/Logger.lhs}
  \CodeInfo{The import of loggers}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Import.Logger
       ( module X
       , Logger(..)
       ) where

import System.Log.FastLogger as X
import Network.Wai.Logger as X
import Yesod.Core.Types
\end{code}
