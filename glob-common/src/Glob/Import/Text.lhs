

\begin{code}
module Glob.Import.Text
       ( module X
       , Glob.Import.Text.read
       , Glob.Import.Text.show
       ) where

import Prelude            as P
import Data.Text          as X
import Data.Text.Encoding as X
\end{code}

\begin{code}
read :: P.Read a => X.Text -> a
read = P.read . X.unpack
show :: P.Show a => a -> X.Text
show = X.pack . P.show
\end{code}
