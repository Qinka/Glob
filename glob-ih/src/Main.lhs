% src/Main.lhs

\begin{codeinfo}
  \CodePath{src/Main.lhs}
  \CodeInfo{Main of Glob-IH}
  \CodeProject{glob-ih}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Main
    ( main
    ) where

      import Data.Time
      import Data.Time.Format
      import Glob.Auth.Token
      import System.Environment
      import System.IO

      import qualified GHC.IO.Encoding as E
\end{code}

\begin{code}
      main :: IO ()
      main = do
#ifdef WithUTF8
        E.setLocaleEncoding E.utf8
        hSetEncoding stdout utf8
#endif
        r:tokens <- getArgs
        now <- getCurrentTime
        let dt       = addUTCTime (fromIntegral $ read r) now
        let d        = formatTime defaultTimeLocale "%a, %d %b %G %T GMT" dt
        let ut       = readTime   defaultTimeLocale "%a, %d %b %G %T GMT" d :: UTCTime
        let s        = transPsk2Token (show ut) tokens
        cmds <- getContents
        put.concat.lines $ cmds
                           ++ "-H \"Token:"
                           ++ s
                           ++ "\" -H \"Date:"
                           ++ d
                           ++ "\" "
        where
          put f = putStr f >> hPutStrLn stderr f
\end{code}
