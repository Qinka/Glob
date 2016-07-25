% src/Glob/Model.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Model.lhs}
  \CodeInfo{The Model of Glob, here I used MongoDB}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-17}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Model
    ( module X
    , Mongoble(..)
    , getDBCPoolM
    , runDB, runDB'
    , ConnectionPool(..)
    ) where

      import Control.Exception
      import Control.Monad.IO.Class
      import Data.Pool
      import Yesod.Core

      import Database.MongoDB as X
\end{code}

to operate DB
\begin{code}
      class Mongoble a where
        getDBCPool :: a -> ConnectionPool
        getDefaultAM :: a -> AccessMode
        getDefaultAM = defaultAM
        getDefaultDB :: a -> Database
        getDefaultDB = defaultDB

      defaultAM _ = master
      defaultDB _ = "master"
\end{code}

for IO
\begin{code}
      getDBCPoolM :: (Yesod site,Mongoble site)
                  => HandlerT site IO (Pool Pipe)
      getDBCPoolM = getDBCPool <$> getYesod
\end{code}

runDB function
\begin{code}
      runDB' :: (Yesod site,Mongoble site)
             => Action (HandlerT site IO) a
             -> HandlerT site IO a
      runDB' f = getYesod >>= \site ->
        runDB (getDefaultAM site)
              (getDefaultDB site)
              f
\end{code}

runDB function
\begin{code}
      runDB :: (Yesod site,Mongoble site)
            => AccessMode -> Database
            -> Action (HandlerT site IO) a
            -> HandlerT site IO a
      runDB am db f = getDBCPoolM >>= \pool ->
        withResource pool $ \p -> access p am db f
\end{code}


connection pool
\begin{code}
      type ConnectionPool = Pool Pipe
\end{code}
