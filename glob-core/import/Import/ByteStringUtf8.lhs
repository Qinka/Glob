% import/Import/ByteStringUtf8.lhs

\begin{codeinfo}
  \CodePath{import/Import/ByteStringUtf8.lhs}
  \CodeInfo{Import of ByteString}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Import.ByteStringUtf8
       ( module X
       , fromStrictBS, toStrictBS
       , readBSUtf8, showBSUtf8
       ) where

import Data.ByteString as X
import Data.ByteString.Lazy as BL
import Data.Text as T
import Data.Text.Encoding as TE
\end{code}

read and show for ByteString
\begin{code}
readBSUtf8 :: Read a => X.ByteString -> a
readBSUtf8 = read.T.unpack.TE.decodeUtf8
showBSUtf8 :: Show a => a -> X.ByteString
showBSUtf8 = TE.encodeUtf8.T.pack.show
\end{code}

transform between strict BS and lazy BS
\begin{code}
fromStrictBS = BL.fromStrict
toStrictBS = BL.toStrict
\end{code}
