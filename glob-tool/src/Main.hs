{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import qualified Glob.Import.ByteString as B
import           System.Console.CmdArgs
import Glob.Auth.Core
import System.IO


data Glob = Ih -- ^ identification helper
            { ihToken :: String
            }
          | New -- ^ create new one
            { newId      :: String        -- ^ id
            , newTyp     :: String        -- ^ type
            , newPath    :: String        -- ^ path (url)
            , newTitle   :: String        -- ^ title
            , newTags    :: [String]      -- ^ tags
            , newSum     :: Maybe String  -- ^ summary for item
            , newContent :: String        -- ^ path for item
            , newWhose   :: Maybe String  -- ^ owner
            }
          | Del -- ^ delete one
            { delId :: String -- ^ id
            }
          | Make -- ^ build make file
            { mkOpt  :: String -- ^ make option(name)
            , mkKind :: Maybe String -- ^ make kind
            , mkOut  :: Maybe String -- ^ output file
            }
          | Nav  -- ^ about nav
            { navOpt   :: String
            , navLabel :: String
            , navOrder :: String
            , navName  :: String
            }
          | Script -- ^ about script
            { sptKind :: Maybe String
            }
            deriving (Show,Data)

ih :: Glob
ih = Ih { ihToken = def
                 &= help "token of identifying"
                 &= typ "TOKEN"
                 &= explicit &= name "token"
                 &= explicit &= name "t"
        , ihHash  = def
                 &= help "hash algorithm"
                 &= typ "shaXX"
                 &= explicit &= name "hash"
                 &= explicit &= name "h"
        }

new :: Glob
new = New { newId = def
            &= help "the id of item"
            &= typ "ID"
            &= explicit &= name "id"
            &= explicit &= name "i"
          , newTyp = def
            &= help "the type of item"
            &= typ "TYPE"
            &= explicit &= name "type"
            &= explicit &= name "k"
          , newPath = def
            &= help "the url of item"
            &= typ "URL"
            &= explicit &= name "url"
            &= explicit &= name "u"
          , newTitle = def
            &= help "the title of item"
            &= typ "TEXT"
            &= explicit &= name "title"
            &= explicit &= name "h"
          , newTags = def
            &= help "the tag(s) of item"
            &= typ "TAG"
            &= explicit &= name "tag"
            &= explicit &= name "t"
          , newSum = def
            &= help "the summory of item"
            &= typ "TEXT|FILE"
            &= explicit &= name "summary"
            &= explicit &= name "s"
          , newContent = def
            &= help "the context's path"
            &= typFile
            &= explicit &= name "content"
            &= explicit &= name "c"
          , newWhose = def
            &= help"the owner"
            &= typ "OWNER"
            &= explicit &= name "whose"
            &= explicit &= name "w"
          }

del :: Glob
del = Del { delId = def
            &= help "id"
            &= typ "ID"
            &= explicit &= name "id"
            &= explicit &= name "i"
          }

make :: Glob
make = Make { mkOpt = "all"
              &= help "create a hold make file"
              &= typ "all|item"
              &= explicit &= name "opt"
              &= explicit &= name "o"
            , mkKind = def
              &= help "label"
              &= typ "ID"
              &= explicit &= name "label"
              &= explicit &= name "l"
            , mkOut = def
              &= help "output"
              &= typFile
              &= explicit &= name "out"
              &= explicit &= name "f"
            }

nav :: Glob
nav = Nav { navOpt = def
            &= help "opt"
            &= typ "add|del"
            &= explicit &= name "opt"
            &= explicit &= name "o"
          , navLabel = def
            &= help "label"
            &= typ "LABEL"
            &= explicit &= name "label"
            &= explicit &= name "l"
          , navName = def
            &= help "name"
            &= typ "NAME"
            &= explicit &= name "name"
            &= explicit &= name "n"
          , navOrder = def
            &= help "order"
            &= typ "INT"
            &= explicit &= name "order"
            &= explicit &= name "r"
          }

script :: Glob
script = Script { sptKind = def
                  &= help "scirpt"
                  &= typ "TEXT"
                  &= explicit &= name "kind"
                  &= explicit &= name "k"
                }



glob :: Glob
glob = modes [ ih
             , new
             , del
             , make
             , nav
             , script
             ]
  &= program "glob"
  &= summary "summary"

main :: IO ()
main = do
  it <- cmdArgs glob
  case it of
    ih_@Ih{..} -> do
      cmds <- getContents
      let put t = hPutStrLn stdout t
               >> hPutStrLn stderr t 
      put . concat . lines $ cmds
        ++ " -F \"token="
        ++ (B.unpack $ generateHash SHA3_512 $ B.pack ihToken)
        ++"\" "
    new_@New{..} -> do
