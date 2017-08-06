{-# LANGUAGE RecordWildCards #-}

module Glob.Tool.Make
  (
  ) where

import           Control.Monad
import           Data.Functor
import           Data.String
import           Data.Text.Internal.Builder
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
import qualified Glob.Import.Text           as T
import           Glob.Tool.Opt
import           Glob.Tool.Repo

-- | MakeM
data MakeM a = MakeM { mkBuilder  :: Builder
                     , mkConstant :: a
                     }

instance Functor MakeM where
  fmap f (MakeM mf c) = MakeM mf (f c)

instance Applicative MakeM where
  pure c = MakeM (Makefile mempty) c
  (<*>) (MakeM a f) (MakeM b c) = MakeM (a `mappend` b) (f c)

instance Monad MakeM where
  (>>=) (MakeM a c) f = let MakeM b t = f c
                        in MakeM (a `mappend` b) t

instance IsString (MakeM ()) where
  fromString str = MakeM (fromString str) ()


endLine :: MakeM ()
endLine = MakeM "\n" ()

-- | add prefix for each line
--   o(n)
linesPrefix :: Text -- ^ prefix
            -> MakeM a -- ^ items
            -> MakeM a
linesPrefix prefix (Make builder c) =
  let (t:ts) = map (\x -> fromText prefix `mappend` fromText x) $
               T.lines $ TL.toStrict $ toLazyText builder
      new    = foldl (\a b -> a `mappend` singleton '\n' `mappend` b) t ts
  in Make new c

string :: String -> MakeM ()
string str = MakaM (fromString str) ()
stringT :: T.Text -> MakeM ()
stringT str = MakeM (fromText str) ()
stringLnT :: T.Text -> MakeM ()
stringLnT str = MakeM (fromText str `mappend` "\n") ()
charT :: Chat -> MakeM ()
charT c = MakeM (singleton c) ()

target :: T.Text -- ^ target
       -> [T.Text] -- ^ dependences
       -> MakeM () -- ^ body
       -> MakeM ()
target tar deps body = do
  stringT tar
  charT ':'
  forM_ (\t -> charT ' ' >> stringT t) >> endLine
  linesPrefix "\t" body

comment :: T.Text
        -> MakeM ()
comment c = linesPrefix "# " $ stringT c

(\=\) :: T.Text -> T.Text -> MakeM ()
var \=\ value = MakeM (fromText var `mappend` " = " `mappend` fromText value) ()

macro :: Text -> Text
macro str = "${" `T.append` str `T.mappend` "}"
macroM :: Text -> MakeM ()
macroM = stringT . macro

-- | echo
echo :: Text -> MakeM ()
echo t = MakeM ("echo" `mappend` fromText t `mappend` singleton '\n') ()

-- | curl
curl :: Text -- ^ flags
     -> Text -- ^ http method
     -> Text -- ^ url
     -> [(Text,Text)] -- ^ settings
     -> MakeM ()
curl flags method url settings = do
  macroM curlPATH >> " " >> macroM curlDetail >> " " >> stringT flags >> " \\n"
  linesPrefix "\t" $ do
    "-X " >> stringT method >> " \\n"
    let putSetting (label,value) = "\'-F \"" `T.append` label `T.append` "="
          `T.append` value `T.append` "\"\' \\n"
    mapM_ putSetting settings


----------------------
-- setttings
----------------------
-- | curl's program name(path) macro
curlPath :: Text
curlPath = "CURL_PATH"
-- | curl's option to show details
curlDetail :: Text
curlDetail = "CURL_DETAIL"
-- | site url
siteURL :: Text
siteURL = "SITE_URL"




mkClean :: MakeM ()
mkClean = do
  comment "clean"
  target "clean" [] $ do
    echo "Clean .ignore/tmp.*"
    stringT "@rm -rf .ignore/tmp.*" >> endLine
    echo "Down"

mkSettings :: Repo -> MakeM ()
mkSettings = do
  comment "site url"
  siteURL \=\ T.pack siteUrl
  comment $ do
    "curl settings\n"
    "detail: show details or not\n"
  curlPath   \=\ "curl"
  curlDetail \=\ "\'\'"
  return ()

mkComment :: MakeM ()
mkComment = do
  "### Glob update Makefile\n"
  "### Copyright (C) 2017\n"

mkItem :: Item -> MakeM ()
mkItem item =  mkItemUpdate item >> mkItemDel item

mkItemUpdate :: Item -> MakeM ()
mkItemUpdate item@Item{..} = do
  let sumDepends = case summary of
        Left path -> [path]
        _         -> []
  target (T.pack id) (T.pack content:map T.pack summaryDepends) $ do
    when (typ `elem` ["post","frame"]) $ do
      when (not $ null summaryDepends) $ do
        "pandoc -o" >> string (withoutExtension $ head sumDepends) >> ".html " >> string path
      "pandoc -o" >> string (withoutExtension content) >> ".html " >> string content
    "echo "
    curl "" "PUT"



withoutExtension :: String -> String
withoutExtension path = if '.' `elem` path
  then reverse . dropWhile (== '.') . dropWhile (/='.') $ reverse path
  else path
