% import/Import/Wai.lhs

\begin{codeinfo}
  \CodePath{import/Import/Wai.lhs}
  \CodeInfo{The import of wai, warp, and others}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Import.Wai
       ( module X
       ) where

import Network.Wai as X
import Network.Wai.Handler.Warp as X
import Network.HTTP.Types as X
\end{code}
