% src/Glob/Core.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Core.lhs}
  \CodeInfo{Export of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Core
       ( module X
       ) where


       
import Glob.Config     as X
import Glob.Foundation as X
import Glob.Handler    as X

import Yesod.Core.Dispatch
       
mkYesodDispatch "Glob" resourcesGlob
\end{code}

