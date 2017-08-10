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
import System.FilePath.Posix (makeRelative)

data RepoCfg = RepoCfg { tokenFile :: FilePath
                       , siteUrl   :: String
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

data Nav a = Nav { order :: Int
                 , label :: a -- url
                 , name  :: a -- name
                 }
           deriving Show
instance Functor Nav where
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


makePathRelateRepo :: FilePath -> Item String -> IO (Item String)
makePathRelateRepo repo item = do
  newSum <- case iSummary of
        Summary (Left path) -> makeAbsolute path >>= (return . Summary . Left . makeRelative repo)
        _ -> return iSummary
  newCon -> makeRelative repo <$> makeAbsolute iContent
  return item{iSummary = newSumm, iContent = newCon}
  

updateRealPathT :: Item T.Text -> Item T.Text

