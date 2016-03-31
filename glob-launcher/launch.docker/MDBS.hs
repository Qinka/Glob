




-- src/MDBS.hs
{-# LANGUAGE CPP #-}

module MDBS
    ( module X
    ) where

#ifdef WithPostgres
#ifndef HADDB
      import MDBS.Postgres as X
#else
#warning "Used two kinds of database"
#endif
#define HADDB
#endif

#ifdef WithMongoDB
#ifndef HADDB
      import MDBS.Mongo as X
#else
#warning "Used two kinds of database"
#endif
#define HADDB
#endif

#ifndef HADDB
#warning "Need db"
      import Prelude as X
#endif
