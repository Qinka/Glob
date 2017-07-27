{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.New
  ( newHandler
  ) where

import Glob.Tool.Opt
import Glob.Tool.Repo
import System.Directory
import Glob.Import
import qualified Glob.Import.ByteString.Lazy as BL
import Glob.Import.Aeson

newHandler :: Glob -> IO ()
newHandler New{..} = findRepo globRepoName >>= \repo' case repo' of
  Just repo -> do
    case (newId,newTyp,newPath,newContent) of
      (Just nid, Just ntyp, Just npath, Just ncontent) -> do
        sum <- toSummary newSum
        cur <- getCurrentTime
        let item = Item { summary = sum
                        , mime    = newMIME
                        , path    = npath
                        , whose   = newWhose
                        , cTime   = cur
                        , id      = nid
                        , content = ncontent
                        , title   = newTitle
                        , typ     = ntyp
                        , tags    = newTags
                        }
         BL.writeFile (repo ++ "/" ++ globRepoName ++ nid ++ ".item.json") $ encode item
      _ -> hPutStrLn stderr "error: one(some) of id, typ, path, or content is(are) empty"
  where toSummary (Just s) = do
          is <- makeAbsolute s >>= doesPathExist
          ra <- makeRepoAbsolute
          case (is,ra) of
            (True, Just raPath) -> return $ Left raPath
            _ -> return $ Right s
        toSummary Nothing = return $ Right ""
