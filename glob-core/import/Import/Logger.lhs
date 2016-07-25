% import/Import/Logger.lhs

\begin{codeinfo}
  \CodePath{import/Import/Logger.lhs}
  \CodeInfo{The import of loggers}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
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
