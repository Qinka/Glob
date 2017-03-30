
For infos

\begin{code}
module Glob.Common.Info
       ( glob_common_version
       , glob_common_version_quote
       , glob_common_git_branch_quote
       , glob_common_git_commit_quote
       ) where

import Glob.Import.Version
import Paths_glob_common
\end{code}


the version of the glob-auth
\begin{code}
glob_common_version :: Version
glob_common_version = version
\end{code}

the version string quote
\begin{code}
glob_common_version_quote :: Q Exp
glob_common_version_quote = stringE $ showVersion version
\end{code}

\begin{code}
glob_common_git_branch_quote :: Q Exp
glob_common_git_branch_quote = gitBranch
glob_common_git_commit_quote :: Q Exp
glob_common_git_commit_quote = gitHash
\end{code}
