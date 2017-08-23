{-|
Module:      Glob.Auth.Core
Desription:  Core method of the authentication
Copyright:   (C) Qinka 2017
License:     GPL-3
Maintainer:  me@qinka.pro
Stability:   experimental
Portability: unknown

The collection of core method for authentication.
-}

module Glob.Auth.Core
       ( -- * method for hash
         --
         -- $hashMethod
         generateHash
       , verifyHash
       , hash
         -- * hash algorithm
       , SHA512(..)
       , SHA384(..)
       , SHA3_512(..)
       , SHA3_384(..)
       , SHA3_256(..)
       , SHA3_224(..)
       , SHA256(..)
       , SHA224(..)
       , SHA1(..)
       , SHA512t_256(..)
       , SHA512t_224(..)
       , HashAlgorithm(..)
       , ByteArrayAccess(..)
       ) where

import           Crypto.Hash
import           Data.ByteArray
import           Glob.Import
import           Glob.Import.ByteString (ByteString)
import qualified Glob.Import.ByteString as B

-- $hashMethod
--
-- These methods are used to generate the token to the hash
-- and verify the token and hash.
--
-- If you want the generate the hash for "12345qwert",
-- you should:
-- > let hashStr = generateHash SHA512 "12345qwert"
-- When you verify the hash, you need to
-- > let isOk = verifyHash SHA512 "xxxxx" "12345qwert"


-- | generate hash for the key
generateHash :: HashAlgorithm a
             => a            -- ^ Hash algorithm
             -> ByteString   -- ^ The hash of key
             -> ByteString   -- ^ Hash string
generateHash a = B.show . hashWith a


-- | verify the hash and key
verifyHash :: HashAlgorithm a
           => a          -- ^ Hash algorithm
           -> ByteString -- ^ Hash for key
           -> ByteString -- ^ Hash string
           -> Bool
verifyHash a hash token = token == generateHash a hash

