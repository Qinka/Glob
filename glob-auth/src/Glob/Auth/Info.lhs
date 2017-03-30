
For infos

\begin{code}
module Glob.Auth.Info
       ( glob_auth_version
       , glob_auth_version_quote
       , glob_auth_git_branch_quote
       , glob_auth_git_commit_quote
       ) where

import Glob.Import.Version
import Paths_glob_auth
\end{code}


the version of the glob-auth
\begin{code}
glob_auth_version :: Version
glob_auth_version = version
\end{code}

the version string quote
\begin{code}
glob_auth_version_quote :: Q Exp
glob_auth_version_quote = stringE $ showVersion version
\end{code}

\begin{code}
glob_auth_git_branch_quote :: Q Exp
glob_auth_git_branch_quote = gitBranch
glob_auth_git_commit_quote :: Q Exp
glob_auth_git_commit_quote = gitHash
\end{code}
