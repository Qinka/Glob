{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Glob.Tool.Repo
  ( RepoCfg(..)
  , Summary(..)
  , Item(..)
  , Nav(..)
  , findRepo
  , makeAbsoluteRepoT
  , globRepoName
  , makePathRelateRepo
  , globRepoName
  , globRepoNameT
  ) where

import           Data.List
import           Glob.Import
import           Glob.Import.Aeson
import qualified Glob.Import.Text      as T
import           System.Directory
import           System.FilePath.Posix (makeRelative)

data RepoCfg = RepoCfg { siteUrl   :: String
                       }
             deriving Show
deriveJSON defaultOptions ''RepoCfg

newtype Summary a = Summary (Either FilePath a)
                  deriving Show
deriveJSON defaultOptions ''Summary
instance Functor Summary where
  fmap f (Summary summary) = Summary $ f <$> summary



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
instance Functor Item where
  fmap f Item{..} = Item
                    (f <$> iSummary)
                    (f <$> iMIME)
                    (f     iPath)
                    (f <$> iWhose)
                    (      iCreTime)
                    (f     iId)
                    (f     iContent)
                    (f <$> iTitle)
                    (f     iType)
                    (f <$> iTags)


data Nav a = Nav { order :: Int
                 , url   :: a -- url
                 , label :: a -- name, or say label
                 }
           deriving Show
instance Functor Nav where
  fmap f Nav{..} = Nav order (f url) (f label)

deriveJSON defaultOptions ''Nav

-- | find out repo's dir
--   If the path of the .git direcory is @/path/to/repo/.git@
--   @the findRepo ".git"@ will return @/path/to/repo@
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

makeAbsoluteRepoT :: FilePath -- ^ repo
                  -> Item T.Text
                  -> Item T.Text
makeAbsoluteRepoT repo item =
  let newCon = T.pack repo `T.append` iContent item
      newSum = case iSummary item of
        Summary (Left p) -> Summary $ Left $ repo ++ p
        _                -> iSummary item
  in item { iSummary = newSum, iContent = newCon}

globRepoName :: String
globRepoName = ".glob"
globRepoNameT :: T.Text
globRepoNameT = ".glob"


makePathRelateRepo :: FilePath -- ^ repo path
                   -> Item String
                   -> IO (Item String)
makePathRelateRepo repo item = do
  newSum <- case iSummary item of
        Summary (Left path) -> makeAbsolute path >>= (return . Summary . Left . makeRelative repo)
        _ -> return (iSummary item)
  newCon <- makeRelative repo <$> makeAbsolute (iContent item)
  return item{iSummary = newSum, iContent = newCon}

