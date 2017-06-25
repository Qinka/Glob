{-|
Module:      Glob.Utils.Info
Description: The information of the package
Copyright:   (C) Qinka 2017
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The information and version of the package.
-}

module Glob.Utils.Info
       ( glob_utils_version
       , -- * quotes
         glob_utils_version_quote
       , glob_utils_git_branch_quote
       , glob_utils_git_commit_quote
       ) where

import           Glob.Import.TH
import           Glob.Import.Version
import           Paths_glob_utils


-- | Version of glob-common
glob_utils_version :: Version
glob_utils_version = version

-- | Version of glob-common (quote)
glob_utils_version_quote :: Q Exp
glob_utils_version_quote = stringE $ showVersion version

-- | Git Branch (quote)
glob_utils_git_branch_quote :: Q Exp
glob_utils_git_branch_quote = gitBranch

-- | Git Branch (quote)
glob_utils_git_commit_quote :: Q Exp
glob_utils_git_commit_quote = gitHash
