{-# LANGUAGE TemplateHaskell #-}

module Glob.Tool.Repo
  ( RepoCfg(..)
  , Summary(..)
  , Item(..)
  , Nav(..)
  , findRepo
  , makeRepoAbsolute
  , globRepoName
  ) where

import           Data.List
import           Glob.Import
import           Glob.Import.Aeson
import           System.Directory

data RepoCfg = RepoCfg { tokenFile :: FilePath
                       , siteUrl   :: String
                       }
             deriving Show
deriveJSON defaultOptions ''RepoCfg

newtype Summary a = Summary (Either FilePath a)
                  deriving Show
deriveJSON defaultOptions ''Summary

data Item a = Item { iSummary :: Summary a
                   , iMIME    :: Maybe a
                   , iPath    :: a
                   , iWhose   :: Maybe a
                   , iCreTime :: UTCTime
                   , iId      :: a
                   , iContent :: a
                   , iTitle   :: Maybe a
                   , iType    :: a
                   , iTags    :: [a]
                   }
          deriving Show
deriveJSON defaultOptions ''Item

data Nav = Nav { order :: Int
               , label :: String -- url
               , name  :: String -- name
               }
           deriving Show
deriveJSON defaultOptions ''Nav

-- | find out repo's dir
findRepo :: FilePath -> IO (Maybe FilePath)
findRepo fp = getCurrentDirectory >>= findRepoStep
  where findRepoStep "" = return Nothing
        findRepoStep dir = do
          list <- filter (== fp) <$>  listDirectory dir
          if null list
            then findRepoStep (uplevel dir)
            else return (Just dir)
        uplevel "/" = ""
        uplevel p = reverse
                  . dropWhile (\x -> x /= '/' && x /= '\\')
                  . dropWhile (\x -> x == '/' || x == '\\')
                  . reverse
                  $ p

makeRepoAbsolute :: FilePath -> FilePath -> IO (Maybe FilePath)
makeRepoAbsolute r p = findRepo r >>= \repo' -> case repo' of
  Just repo -> do
    p' <- makeAbsolute p
    return $ stripPrefix repo p'
  _ -> return Nothing

globRepoName :: String
globRepoName = ".glob"



updateRealPathT :: Item T.Text -> Item T.Text
