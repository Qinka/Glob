{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The test module for model for glob
module ModelSpec where

import Test.Hspec
import Glob.Core.Model

-- | test database name
test_db :: Database
test_db = "test-glob-db"

-- | instance
instance Mongodic () IO where
  get_default_access_mode
  

-- | clean the database
withCleanDatabase :: ActionWith () -> IO ()
withCleanDatabase action = 




spec :: Spec
spec = around withCleanDatabase $ undefined
