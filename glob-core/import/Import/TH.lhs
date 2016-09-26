% import/Import/TH.lhs

\begin{codeinfo}
  \CodePath{import/Import/TH.lhs}
  \CodeInfo{Import of Template Haskell}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Import.TH
       ( module X
       ) where

import Language.Haskell.TH as X
import Language.Haskell.TH.Quote as X
import Language.Haskell.TH.Syntax as X
\end{code}
