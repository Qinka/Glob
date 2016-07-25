% import/Import/Wai.lhs

\begin{codeinfo}
  \CodePath{import/Import/Wai.lhs}
  \CodeInfo{The import of wai, warp, and others}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
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
