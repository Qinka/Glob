--{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Glob.Tool.Make where
 -- ( MakeM(..)
 -- ,
 -- ) where

import           Control.Monad
import           Data.Functor
import           Data.String
import           Data.Text.Internal.Builder hiding (fromByteString)
import qualified Data.Text.Internal.Builder as TB
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
  pure c = MakeM mempty c
  (<*>) (MakeM a f) (MakeM b c) = MakeM (a `mappend` b) (f c)

instance Monad MakeM where
  (>>=) (MakeM a c) f = let MakeM b t = f c
                        in MakeM (a `mappend` b) t

instance (() ~ a) => IsString (MakeM a) where
  fromString str = MakeM (TB.fromString str) undefined

instance Show a => Show (MakeM a) where
  show (MakeM mf c) = show c ++ "\n" ++ TL.unpack (toLazyText mf)

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
  stringT tar
  charT ':'
  mapM_ (\t -> charT ' ' >> stringT t) deps >> endLine
  linesPrefix "\t" body

comment :: T.Text
        -> MakeM ()
comment c = linesPrefix "# " $ stringT c

(\=\) :: T.Text -> T.Text -> MakeM ()
var \=\ value = MakeM (fromText var `mappend` " = " `mappend` fromText value) ()

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
    "-X " >> stringT method >> " \\n"
    let putSetting (label,value) = stringT $ "\'-F \"" `T.append` label `T.append` "="
          `T.append` value `T.append` "\"\' \\\n"
    mapM_ putSetting settings


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
  curlPath   \=\ "curl"
  curlDetail \=\ "\'\'"
  return ()

mkComment :: MakeM ()
mkComment = do
  "### Glob update Makefile\n" :: MakeM ()
  "### Copyright (C) 2017\n"

mkItem :: Item -> MakeM ()
mkItem item =  mkItemUpdate item >> mkItemDel item

mkItemUpdate :: Item -> MakeM ()
mkItemUpdate item@Item{..} = do
  let sumDepends = case summary of
        (Summary (Left p)) -> [p]
        _                  -> []
  target (T.pack id) (T.pack content:map T.pack sumDepends) $ do
    when (typ `elem` ["post","frame"]) $ do
      when (not $ null sumDepends) $ do
        "pandoc -o " >> string (withoutExtension $ head sumDepends) >> ".html " >> string path
      "pandoc -o " >> string (withoutExtension content) >> ".html " >> string content
    "echo "
    let url = macro siteURL `T.append` T.pack path
    curl "" "PUT" url
      [ ("a","b")
      ]



withoutExtension :: String -> String
withoutExtension path = if '.' `elem` path
  then reverse . dropWhile (== '.') . dropWhile (/='.') $ reverse path
  else path

mkItemDel = undefined
