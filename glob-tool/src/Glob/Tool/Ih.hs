{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.Ih
  ( ihHandler
  ) where

import           Glob.Auth.Core hiding (generateHash)
import qualified Glob.Auth.Core as A
import           Glob.Tool.Opt
import           System.IO


ihHandler :: Glob -> IO ()
ihHandler Ih{..} =
  case ihToken of
    Just token -> do
      cmds <- getContents
      let put t = hPutStrLn stdout t
               >> hPutStrLn stderr t
      put . concat . lines $ cmds
        ++ " -F \"token="
        ++ generateHash token
        ++"\" "
    _ ->  hPutStrLn stderr "token required"
  where generateHash token = pack $ case ihHash of
          Just "sha512"      -> A.generateHash SHA512
          Just "sha384"      -> A.generateHash SHA384
          Just "sha3-512"    -> A.generateHash SHA3_512
          Just "sha3-384"    -> A.generateHash SHA3_384
          Just "sha3-256"    -> A.generateHash SHA3_256
          Just "sha3-224"    -> A.generateHash SHA3_224
          Just "sha256"      -> A.generateHash SHA256
          Just "sha224"      -> A.generateHash SHA224
          Just "sha512t-256" -> A.generateHash SHA512t_256
          Just "sha512t-224" -> A.generateHash SHA512t_224
          _                  -> A.generateHash SHA1
        pack f = B.unpack . f . B.pack
