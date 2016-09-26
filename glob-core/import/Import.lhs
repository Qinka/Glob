％ import/Import.lhs

\begin{codeinfo}
  \CodePath{import/Import.lhs}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Import
       ( module X
       ) where

import Data.Aeson as X
import Data.Maybe as X
import Data.Time as X
\end{code}
