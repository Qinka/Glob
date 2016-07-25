% import/Import/Text.lhs

\begin{codeinfo}
  \CodePath{import/Import/Text.lhs}
  \CodeInfo{Import of Data.Text and Data.Encoding}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Import.Text
    ( module X
    , fromStrictT, toStrictT
    , readT, showT
    ) where

      import Data.Text as X
      import Data.Text.Lazy
      import Data.Text.Encoding as X
\end{code}

read and show for Text
\begin{code}
      readT :: Read a => X.Text -> a
      readT = read.X.unpack
      showT :: Show a => a -> X.Text
      showT = X.pack.show
\end{code}

transform between lazy Text and strict Text
\begin{code}
      fromStrictT = fromStrict
      toStrictT = toStrict
\end{code}
