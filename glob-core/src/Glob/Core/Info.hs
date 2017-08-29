{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE TemplateHaskell #-}

{-|
Module        : Glob.Core.Info
Description   : THe info of this package
Copyright     : Qinka 2017
License       : GPL-3
Maintainer    : qinka@live.com
                me@qinka.pro
Stability     : experimental
Portability   : x86/64

The information of thos package, such as version, git commit-id.
-}





module Glob.Core.Info
       ( -- * TemplateHaskell's variables
         glob_core_version
       , glob_core_version_quote
       , glob_core_git_branch_quote
       , glob_core_git_commit_quote
       , glob_build_info_quote
       ) where

import           Data.Char
import           Data.Time
import           Glob.Import.TH
import           Glob.Import.Version
import           Paths_glob_core
import           System.Info

-- | The version of this package, in Data.Version.Version
glob_core_version :: Version -- ^ The version
glob_core_version = version
-- | The version of the package, in a Q Exp
glob_core_version_quote :: Q Exp -- ^ String
glob_core_version_quote = stringE $ showVersion version



-- | The commit of the git
glob_core_git_commit_quote :: Q Exp -- ^ String
glob_core_git_commit_quote = gitHash
-- | The commit of the branch
glob_core_git_branch_quote :: Q Exp -- ^ String
glob_core_git_branch_quote = gitBranch

-- | build information
glob_build_info_quote :: Q Exp -- ^ String
glob_build_info_quote = do
  timeStr <- formatTime defaultTimeLocale "-%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  stringE $ os ++ "-" ++ arch ++ "-"  ++ map toUpper compilerName
    ++ "-" ++ showVersion compilerVersion ++ timeStr
    ++ "-git:" ++ $gitBranch ++ ":" ++ $gitHash



-- | Use these as a string
-- @
--   putStrLn $glob_core_version_quote ++ $glob_core_git_commit_quote
-- @
