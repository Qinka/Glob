{-|
Module:      Glob.Import.Version
Description: The reexport of the ``version''
Copyright:   (C) Qinka 2017
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The reexport of many
-}


module Glob.Import.Version
       ( -- * the reexported module
         module Data.Version
       , module Development.GitRev
       , module System.Info
       , -- * the methods
         compile_time
       , compile_os
       , compile_compiler
       ) where

import           Data.Char
import           Data.Version
import           Development.GitRev
import           Glob.Import
import           Glob.Import.TH
import           System.Info


-- | Get the time of compiling
compile_time :: Q Exp
compile_time = do
  now <- formatTime defaultTimeLocale "%Y-%m-%d-%H-%M-%S" <$> runIO getCurrentTime
  stringE now

-- | Get the os of compiler-host
compile_os :: Q Exp
compile_os = stringE $ os ++ "-" ++ arch

-- | Get the compiler system
compile_compiler :: Q Exp
compile_compiler = stringE $ toUpper <$> compilerName ++ "-" ++ showVersion compilerVersion
