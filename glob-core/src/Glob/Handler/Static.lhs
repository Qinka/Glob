% src/Glob/Handler/Static.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Static.lhs}
  \CodeInfo{The handler of GET method of static url}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-22}
  %\CodeChangeLog{date}{text}
\end{codeinfo}


\begin{code}
module Glob.Handler.Static
    ( getStaticR
    ) where

      import Glob.Foundation
      import Yesod.Core

      import Import.Wai
      import qualified Import.Text as T
\end{code}

static used 301
\begin{code}
      getStaticR :: [T.Text] -> Handler ()
      getStaticR idx = do
        Glob{..} <- getYesod
        redirectWith status301.T.concat $ globStaticUrl:(T.cons '/' <$> idx)
\end{code}
