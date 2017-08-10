--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Glob.Tool.Make
  ( makeHandler
  ) where

import           Control.Monad
import           Data.Functor
import           Data.String
import           Data.Text.Internal.Builder  hiding (fromByteString)
import qualified Data.Text.Internal.Builder  as TB
import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy              as TL
import           Glob.Import.Aeson
import qualified Glob.Import.ByteString      as B
import qualified Glob.Import.ByteString.Lazy as BL
import qualified Glob.Import.Text            as T
import           Glob.Tool.Opt               (Glob (Make, mkItem, mkOut))
import           Glob.Tool.Repo
import           System.Directory
import           System.IO


-- | MakeM
data MakeM a = MakeM { mkBuilder  :: Builder
                     , mkConstant :: a
                     }

instance Functor MakeM where
  fmap f (MakeM mf c) = MakeM mf (f c)

instance Applicative MakeM where
  pure c = MakeM mempty c
  (<*>) (MakeM a f) (MakeM b c) = MakeM (a `mappend` b) (f c)

instance Monad MakeM where
  (>>=) (MakeM a c) f = let MakeM b t = f c
                        in MakeM (a `mappend` b) t

instance (() ~ a) => IsString (MakeM a) where
  fromString str = MakeM (TB.fromString str) undefined

instance Show a => Show (MakeM a) where
  show (MakeM mf _) = TL.unpack (toLazyText mf)

endLine :: MakeM ()
endLine = MakeM "\n" ()

-- | add prefix for each line
--   o(n)
linesPrefix :: T.Text -- ^ prefix
            -> MakeM a -- ^ items
            -> MakeM a
linesPrefix prefix (MakeM builder c) =
  let (t:ts) = map (\x -> fromText prefix `mappend` fromText x) $
               T.lines $ TL.toStrict $ toLazyText builder
      new    = foldl (\a b -> a `mappend` singleton '\n' `mappend` b) t ts
  in MakeM new c

string :: String -> MakeM ()
string str = MakeM (TB.fromString str) ()
stringT :: T.Text -> MakeM ()
stringT str = MakeM (fromText str) ()
stringLnT :: T.Text -> MakeM ()
stringLnT str = MakeM (fromText str `mappend` "\n") ()
charT :: Char -> MakeM ()
charT c = MakeM (singleton c) ()

target :: T.Text -- ^ target
       -> [T.Text] -- ^ dependences
       -> MakeM () -- ^ body
       -> MakeM ()
target tar deps body = do
  "\n"
  stringT tar
  charT ':'
  mapM_ (\t -> charT ' ' >> stringT t) deps >> endLine
  linesPrefix "\t" body

comment :: T.Text
        -> MakeM ()
comment c = linesPrefix "# " (stringT c) >> "\n"

(\=\) :: T.Text -> T.Text -> MakeM ()
var \=\ value = MakeM (fromText var `mappend` " = " `mappend` fromText value `mappend` singleton '\n') ()

macro :: T.Text -> T.Text
macro str = "${" `T.append` str `T.append` "}"
macroM :: T.Text -> MakeM ()
macroM = stringT . macro

-- | echo
echo :: T.Text -> MakeM ()
echo t = MakeM ("echo" `mappend` fromText t `mappend` singleton '\n') ()

-- | curl
curl :: T.Text -- ^ flags
     -> T.Text -- ^ http method
     -> T.Text -- ^ url
     -> [(T.Text,T.Text)] -- ^ settings
     -> MakeM ()
curl flags method url settings = do
  macroM curlPath >> " " >> macroM curlDetail >> " " >> stringT flags >> " \\\n"
  linesPrefix "\t" $ do
    "-X " >> stringT method >> " \\\n"
    let putSetting (label,value) = stringT $ "-F \"" `T.append` label `T.append` "="
          `T.append` value `T.append` "\" \\\n"
    mapM_ putSetting settings
    "-H \"token=" >> macroM siteToken >> " \\\n"
    stringT url

curlF :: T.Text -- param
      -> T.Text -- value
      -> (T.Text,T.Text) -- pair
curlF = (,)

----------------------
-- setttings
----------------------
-- | curl's program name(path) macro
curlPath :: T.Text
curlPath = "CURL_PATH"
-- | curl's option to show details
curlDetail :: T.Text
curlDetail = "CURL_DETAIL"
-- | site url
siteURL :: T.Text
siteURL = "SITE_URL"
-- | now
updateTime :: T.Text
updateTime = "NOW_TIME"
-- | token
siteToken :: T.Text
siteToken = "SITE_TOKEN"

mkClean :: MakeM ()
mkClean = do
  comment "clean"
  target "clean" [] $ do
    echo "Clean .ignore/tmp.*"
    stringT "@rm -rf .ignore/tmp.*" >> endLine
    echo "Down"

mkSettings :: RepoCfg -> MakeM ()
mkSettings rc = do
  comment "site url"
  siteURL \=\ T.pack (siteUrl rc)
  comment $   "curl settings\n"
   `T.append` "detail: show details or not\n"
  curlDetail \=\ "\'\'"
  curlPath   \=\ "curl"
  comment "update time"
  updateTime \=\ "`date -u \"+%Y-%m-%d %H:%M:%S UTC\"`"
  comment "token for site"
  siteToken \=\ "`cat /dev/null`"
  return ()

mkComment :: MakeM ()
mkComment = do
  "### Glob update Makefile\n" :: MakeM ()
  "### Copyright (C) 2017\n"

makeItem :: Item T.Text -> MakeM ()
makeItem item =  mkItemUpdate item >> mkItemDel item

mkItemUpdate :: Item T.Text -> MakeM ()
mkItemUpdate item@Item{..} = do
  let sumDepends = case iSummary of
        (Summary (Left p)) -> [T.pack p]
        _                  -> []
  target iId (iContent:sumDepends) $ do
    when (not $ null sumDepends) $ do
      "pandoc -t html -o " >> stringT (head sumDepends `T.append` ".htmlout")
        >> " " >> stringT (head sumDepends) >> "\n"
    when (iType `elem` ["post","frame"]) $ do
      "pandoc -t html -o " >> stringT (iContent `T.append` ".htmlout")
        >> " " >> stringT iContent >> "\n"
    let url = macro siteURL `T.append` iPath
    "@"
    let commonF =
          [ curlF "type"        iType
          , curlF "create-time" (T.show iCreTime)
          , curlF "update-time" (macro updateTime)
          ]
        titleF = case iTitle of
          Just t -> [curlF "title" t]
          _      -> []
        summaryF = if null sumDepends
          then let (Summary (Right s)) = iSummary in [curlF "summary" s]
          else [curlF "summary" ('@' `T.cons` head sumDepends `T.append` ".htmlout")]
        whoseF = case iWhose of
          Just w -> [curlF "whose" w]
          _      -> []
        mimeF = case iMIME of
          Just m -> [curlF "mime" m]
          _      -> []
        tagsF = map (\t -> curlF "tag" t) iTags
        contentF = return $  case T.toLower iType of
          "post"   -> curlF "html"   ('@' `T.cons` iContent `T.append` ".htmlout")
          "frame"  -> curlF "html"   ('@' `T.cons` iContent `T.append` ".htmlout")
          "text"   -> curlF "text"   ('@' `T.cons` iContent)
          "binary" -> curlF "binary" ('@' `T.cons` iContent)
          "static" -> curlF "url"                  iContent
          "query"  -> curlF "var"                  iContent
    curl "" "PUT" url $ commonF ++ contentF ++ summaryF ++ titleF ++ whoseF ++ mimeF ++ tagsF


mkItemDel :: Item T.Text -> MakeM ()
mkItemDel item@Item{..} = target (iId `T.append` ".del") [] $ do
  let url = macro siteURL `T.append` iPath
  curl "" "DELETE" url [curlF "type" iType]


renewExtensionT :: T.Text -> T.Text -> T.Text
renewExtensionT path new =
  let (name,ext) = T.breakOnEnd "." path
  in if T.null name || name == "." || T.null ext
     then path
     else name `T.append` new

makeNavList :: [Nav T.Text] -> MakeM ()
makeNavList navs = target "navs" [] $ do
  "@" >> curl "" "DELETE" "/@/@nav" []
  flip mapM_ navs $ \Nav{..} -> do
    "@" >> curl "" "PUT" url [ curlF "order" (T.show order)
                             , curlF "url"           url
                             , curlF "label"         label
                             ]



makeHandler :: Glob -> IO ()
makeHandler Make{..} = do
  repo' <- findRepo globRepoName
  case repo' of
    Nothing -> hPutStrLn stderr "Can not find out the repo"
    Just repo ->
      case mkItem of
        Nothing -> do
          let cfgPath = repo ++ "/.glob/global.json"
          is <- doesFileExist cfgPath
          if is then do
            rcfg' <- decode <$> BL.readFile cfgPath
            case rcfg' of
              Nothing -> hPutStrLn stderr "Can not parse the global config file"
              Just rcfg ->
                let text = show $ mkSettings rcfg
                in case mkOut of
                  Nothing   -> putStrLn text
                  Just file -> appendFile (repo ++ "/" ++ file) text
            else hPutStrLn stderr $ "Can not find file: " ++ cfgPath
        Just "navlist" -> do
          let navsPath = repo ++ "/.glob/navlist.json"
          is <- doesFileExist navsPath
          if is
            then do
            navs' <- decode <$> BL.readFile (repo ++ "/.glob/navlist.json")
            case navs' of
              Nothing   -> hPutStrLn stderr "Can not parse the json file for nav list"
              Just navs ->
                let text = show $ makeNavList navs
                in case mkOut of
                  Nothing   -> putStrLn text
                  Just file -> appendFile (repo ++ file) text
            else hPutStrLn stderr "Can not find the json file for nav-list"
        Just item' -> do
          let itemPath = repo ++ "/.glob/" ++ item' ++ ".item.json"
          is <- doesFileExist itemPath
          if is then do
            itemMaybe <- decode <$> BL.readFile itemPath
            case itemMaybe of
              Nothing -> hPutStrLn stderr "Can not find the json file of item"
              Just item'' -> do
                let item = makeAbsoluteRepoT repo item''
                    text = show $ makeItem item''
                case mkOut of
                  Nothing   -> putStrLn text
                  Just file -> appendFile (repo ++ file) text
            else hPutStrLn stderr $ "Can not find the json file for item file: " ++ item' ++ ".item.json"


