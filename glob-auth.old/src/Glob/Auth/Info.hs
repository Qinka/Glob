{-# OPTIONS_HADDOCK hide #-}

{-|
Module        : Glob.Auth.Info
Description   : THe info of this package
Copyright     : Qinka 2017
License       : GPL-3
Maintainer    : qinka@live.com
                me@qinka.pro
Stability     : experimental
Portability   : x86/64

The information of thos package, such as version, git commit-id.
-}

module Glob.Auth.Info
       ( -- * TemplateHaskell's variables
         glob_auth_version
       , glob_auth_version_quote
       , glob_auth_git_branch_quote
       , glob_auth_git_commit_quote
       ) where

import           Glob.Import.Version
import           Paths_glob_auth

-- | The version of this package, in Data.Version.Version
glob_auth_version :: Version -- ^ The version
glob_auth_version = version
-- | The version of the package, in a Q Exp
glob_auth_version_quote :: Q Exp -- ^ String
glob_auth_version_quote = stringE $ showVersion version



-- | The commit of the git
glob_auth_git_commit_quote :: Q Exp -- ^ String
glob_auth_git_commit_quote = gitHash
-- | The commit of the branch
glob_auth_git_branch_quote :: Q Exp -- ^ String
glob_auth_git_branch_quote = gitBranch


-- | Use these as a string
-- @
--   putStrLn $glob_auth_version_quote ++ $glob_auth_git_commit_quote
-- @
