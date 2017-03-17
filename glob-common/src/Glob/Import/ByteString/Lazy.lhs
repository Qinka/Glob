
\begin{code}.Lazy
module Glob.Import.ByteString
       ( module X
       , Glob.Import.ByteString.Lazy.read
       , Glob.Import.ByteString.Lazy.show
       , BC8.pack
       , BC8.unpack
       ) where

import Prelude as P
import Data.ByteString.Lazy as X hiding(pack,unpack)
import Data.ByteString.Lazy.Char8 as BC8
\end{code}

\begin{code}
read :: Read a => X.ByteString -> a
read = P.read . BC8.unpack
show :: Show a => a -> X.ByteString
show = BC8.pack . P.show
\end{code}