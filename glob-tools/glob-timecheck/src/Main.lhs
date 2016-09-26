% src/timecheck/Main.lhs

\begin{codeinfo}
  \CodePath{src/timecheck/Main.lhs}
  \CodeProject{glob-tool}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-08-26}
\end{codeinfo}

\begin{code}
module Main
       ( main
       ) where
import Data.Time
\end{code}

check the time fetched from server and get the differ.
\begin{code}
main :: IO ()
main = do
  time <- read <$> getContents
  now <- getCurrentTime
  print $ diffUTCTime time now
\end{code}
