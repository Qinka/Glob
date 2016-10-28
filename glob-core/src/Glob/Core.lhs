% src/Glob/Core.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Core.lhs}
  \CodeInfo{Export of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Core
       ( module X
       , globCore
       ) where


       
import Glob.Config     as X
import Glob.Foundation as X
import Glob.Handler    as X

import Glob.Common.Infos(isDebug)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Yesod.Core
import Yesod.Core.Dispatch
       
mkYesodDispatch "Glob" resourcesGlob

globCore :: ( ToConfig a
            , CfgD a ~ b
            , Yesod b
            , YesodDispatch b
            )
         => a -> IO Application
globCore cfg =
  toCfgD cfg >>= toWaiApp >>=  withMiddleware
  where
    withMiddleware = return
      . (if isDebug then logStdoutDev else id)
      . gzip def
\end{code}

