% src/Glob/Handler/Query/Parser.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Query/Parser.lhs}
  \CodeInfo{Foundation of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-08-19}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

Parser of query

\begin{code}
module Glob.Handler.Query.Parser
    ( qpTake
    , qpDrop
    , qpBefor
    , qpAfter
    , qpTag
    , qpOr
    , qp
    , toFunc
    , runQP
    ) where

      import Text.Parsec
      import Data.Time
      import Glob.Types

      import qualified Import.Text as T
\end{code}

data of parse
\begin{code}
      data QueryParser = QPTake Int
                       | QPDrop Int
                       | QPBefor UTCTime Bool
                       | QPAfter UTCTime Bool
                       | QPTag String
                       | QPType String
                       | QPOr  [QueryParser]
                       | QPAnd [QueryParser]
                       deriving (Show)
      qpEmpty :: Parsec String () [QueryParser]
      qpEmpty = do
        string ";"
        return []
      qpType :: Parsec String () [QueryParser]
      qpType = do
        typ <- string "type＝" *> many letter <* char ';'
        return [QPType typ]
      qpTake :: Parsec String () [QueryParser]
      qpTake = do
        len <- string "take=" *> many (oneOf ['0'..'9']) <* char ';'
        return [QPTake (read len)]
      qpDrop :: Parsec String () [QueryParser]
      qpDrop = do
        len <- string "drop=" *> many (oneOf ['0'..'9']) <* char ';'
        return [QPDrop (read len)]
      qpBefor :: Parsec String () [QueryParser]
      qpBefor = do
        date <- parseTimeM True defaultTimeLocale "%F+%T" =<<
          (string "befor=" *> many (noneOf "@") <* char '@')
        b <- true <|> false <* char ';'
        return [QPBefor date b]
      true = string "true" >> return True
      false = string "false" >> return False
      qpAfter :: Parsec String () [QueryParser]
      qpAfter = do
        date <- parseTimeM True defaultTimeLocale "%F+%T" =<<
          (string "after=" *> many (noneOf "@") <* char '@')
        b <- true <|> false <* char ';'
        return [QPAfter date b]
      qpTag :: Parsec String () [QueryParser]
      qpTag = do
        tag <- string "tag=" *> many (noneOf ";") <* char ';'
        return [QPTag tag]
      qpAnd :: Parsec String () [QueryParser]
      qpAnd = do
        sub <- char '[' *> many qps <* char ']'
        return [QPAnd $ concat sub]
      qpOr :: Parsec String () [QueryParser]
      qpOr = do
        sub <- char '{' *> many qps <* char '}'
        return [QPOr $ concat sub]
      qp :: Parsec String () [QueryParser]
      qp =  concat <$> many qps
      qps = foldl (<|>) qpEmpty
        [qpTake,qpDrop,qpBefor,qpAfter,qpTag,qpOr,qpAnd
        , qpType
        ]
\end{code}
:
\begin{code}
      toFunc :: [QueryParser] -> ([Rest] -> [Rest])
      toFunc (QPTake i:xs) = take i.toFunc xs
      toFunc (QPDrop i:xs) = drop i.toFunc xs
      toFunc (QPBefor i b:xs) = filter (timeFilter i (>) b).toFunc xs
      toFunc (QPAfter i b:xs) = filter (timeFilter i (<) b).toFunc xs
      toFunc (QPTag i:xs) = filter (tagFilter i).toFunc xs
      toFunc (QPType i:xs) = filter (typFilter i).toFunc xs
      toFunc (QPOr s:xs) =  concat.map sg.toFunc xs
        where
          funcs = (\y -> toFunc [y]) <$> s
          sg y = take 1 $ concatMap (\f -> f [y]) funcs
      toFunc (QPAnd s:xs) = funcs.toFunc xs
        where funcs = toFunc s
      toFunc [] = id
      typFilter :: String -> Rest -> Bool
      typFilter t Rest{..} = rType == T.pack t
      tagFilter :: String -> Rest -> Bool
      tagFilter t' Rest{..} = t `elem` rTags
        where t = T.pack t'
      timeFilter :: UTCTime -> (UTCTime -> UTCTime -> Bool) -> Bool -> Rest -> Bool
      timeFilter t o b Rest{..} = t `o` restTime
        where restTime = if b then rCtime else rUtime
\end{code}

\begin{code}
      runQP :: String -> Either ParseError ([Rest]->[Rest])
      runQP str = toFunc <$> runP qp () "QueryPaserError" str
\end{code}
