

\sourcename{Types.lhs}
\author{Qinka}
\email{qinka@live.com}


\begin{code}
module Glob.Auth.Types
       (
       ) where

import Yesod.Core

\end{code}

The type class Authly

\begin{code}
class Yesod a => Authly a where
  clientPublicKeyDir :: a -> String
  
\end{code}
