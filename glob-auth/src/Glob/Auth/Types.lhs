

\sourcename{Types.lhs}
\author{Qinka}
\email{qinka@live.com}


\begin{code}
module Glob.Auth.Types
       ( Authly(..)
       ) where

import Yesod.Core (Yesod(..))

\end{code}

The type class Authly

\begin{code}
class Authly a where
  clientPublicKeyDir  :: a -> String
\end{code}