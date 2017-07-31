{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.New
  ( newHandler
  ) where

import           Glob.Import
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString.Lazy as BL
import           Glob.Tool.Opt
import           Glob.Tool.Repo
import           System.Directory
import           System.IO

newHandler :: Glob -> IO ()
newHandler New{..} = findRepo globRepoName >>= \repo' -> case repo' of
  Just repo -> do
    case (newId,newTyp,newPath,newContent) of
      (Just nid, Just ntyp, Just npath, Just ncontent) -> do
        sum <- toSummary newSum repo
        cur <- getCurrentTime
        let item = Item { summary = Summary sum
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
  where toSummary (Just s) repo = do
          is <- makeAbsolute s >>= doesPathExist
          ra <- makeRepoAbsolute repo s
          case (is,ra) of
            (True, Just raPath) -> return $ Left raPath
            _                   -> return $ Right s
        toSummary Nothing _ = return $ Right ""
