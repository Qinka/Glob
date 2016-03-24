




-- src/Glob/MDBS.hs

{-# LANGUAGE CPP #-}

module Glob.MDBS
    ( module X
    ) where

#ifdef WithPostgres
#ifndef HasDB
      import Glob.MDBS.Postgres as X
#else
#warning You had used other db.
#endif
#define HasDB
#endif

#ifdef WithMongoDB
#ifndef HasDB
      import Glob.MDBS.MongoDB as X
#else
  #warning You had used other db.
#endif
#define HasDB
#endif

#ifndef HasDB
#warning "You need one and only one db."
      import Prelude as X 
#endif
