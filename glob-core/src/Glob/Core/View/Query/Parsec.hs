{-|
Module      : Glob.Core.View.Query.Parsec
Description : The parsec for query command
Copyright   : (C) Qinka 2017
License     : GPL v3+
Maintainer  : me@qinka.pro
Stability   : experimental
Portability : unknown

This module is for the query command, to parsec the query command
-}

{-# LANGUAGE RecordWildCards #-}

module Glob.Core.View.Query.Parsec
       ( run_qp
       ) where

import           Data.Time
import           Glob.Core.Model  (ResT (..))
import qualified Glob.Import.Text as T
import           Text.Parsec


-- | the ADT for parser
data QueryParser = QPTake  Int                 -- ^ like @take@
                 | QPDrop  Int                 -- ^ like @drop@
                 | QPBefor UTCTime      Bool   -- ^ select those whose dates are earlier than given date
                 | QPAfter UTCTime      Bool   -- ^ select those whose dates are later than given date
                 | QPTag   Bool         String -- ^ select those whose tags include or not include given tag
                 | QPType  Bool         String -- ^ select those whose content's type include or not include
                                               --   given type
                 | QPOr   [QueryParser]        -- ^ like @or@
                 | QPAnd  [QueryParser]        -- ^ like @and@
                 deriving (Show)


-- | get an empty command
qpEmpty :: Parsec String () [QueryParser]
qpEmpty = do
  string ";"
  return []
-- | get the command type
qpType :: Parsec String () [QueryParser]
qpType = do
  t <- string "type=" *> oneOf ['t','f'] <* char '='
  typ <- many letter <* char ';'
  return [QPType (t=='t') typ]
-- | get command take
qpTake :: Parsec String () [QueryParser]
qpTake = do
  len <- string "take=" *> many (oneOf ['0'..'9']) <* char ';'
  return [QPTake (read len)]
-- | get command drop
qpDrop :: Parsec String () [QueryParser]
qpDrop = do
  len <- string "drop=" *> many (oneOf ['0'..'9']) <* char ';'
  return [QPDrop (read len)]
-- | get command befor
qpBefor :: Parsec String () [QueryParser]
qpBefor = do
  date <- parseTimeM True defaultTimeLocale "%F+%T" =<<
    (string "befor=" *> many (noneOf "@") <* char '@')
  b <- true <|> false <* char ';'
  return [QPBefor date b]
-- | get true
true :: Parsec String () Bool
true = string "true" >> return True
-- | get false
false :: Parsec String () Bool
false = string "false" >> return False
-- | get command after
qpAfter :: Parsec String () [QueryParser]
qpAfter = do
  date <- parseTimeM True defaultTimeLocale "%F+%T" =<<
    (string "after=" *> many (noneOf "@") <* char '@')
  b <- true <|> false <* char ';'
  return [QPAfter date b]
-- | get command tag
qpTag :: Parsec String () [QueryParser]
qpTag = do
  t <- string "tag=" *> oneOf ['t','f'] <* char '='
  tag <- many (noneOf ";") <* char ';'
  return [QPTag (t=='t') tag]
-- | get command and
qpAnd :: Parsec String () [QueryParser]
qpAnd = do
  sub <- char '[' *> many qps <* char ']'
  return [QPAnd $ concat sub]
-- | get command or
qpOr :: Parsec String () [QueryParser]
qpOr = do
  sub <- char '{' *> many qps <* char '}'
  return [QPOr $ concat sub]
-- | parser for query command
qp :: Parsec String () [QueryParser]
qp =  concat <$> many qps

qps :: Parsec String () [QueryParser]
qps = foldl (<|>) qpEmpty $ try <$>
  [qpTake,qpDrop,qpBefor,qpAfter,qpTag,qpOr,qpAnd
  , qpType
  ]

-- | transform to filter
to_filter :: [QueryParser] -> ([ResT] -> [ResT])
to_filter [] = id
to_filter (QPTake i:xs) = take i . to_filter xs
to_filter (QPDrop i:xs) = drop i . to_filter xs
to_filter (QPBefor i b:xs) = filter (time_filter i (>) b) . to_filter xs
to_filter (QPAfter i b:xs) = filter (time_filter i (<) b) . to_filter xs
to_filter (QPTag  t i:xs) = filter ((==t) . tag_filter i) . to_filter xs
to_filter (QPType t i:xs) = filter ((==t) . typ_filter i) . to_filter xs
to_filter (QPOr s:xs) = concat . map sg . to_filter xs
  where funcs = (\y -> to_filter [y]) <$> s
        sg y = take 1 $ concatMap (\f -> f [y]) funcs
to_filter (QPAnd s:xs) = to_filter s . to_filter xs

typ_filter :: String -> ResT -> Bool
typ_filter t ResT{..} = rType == T.pack t
tag_filter :: String -> ResT -> Bool
tag_filter t ResT{..} = T.pack t `elem` rTags
time_filter :: UTCTime -> (UTCTime -> UTCTime -> Bool) -> Bool -> ResT -> Bool
time_filter t o b ResT{..} = t `o` res_time
  where res_time = if b then rCTime else rUTime

-- | transform the command to function
run_qp :: String -> Either ParseError ([ResT]->[ResT])
run_qp str = to_filter <$> runP qp () "QueryPaserError" str
