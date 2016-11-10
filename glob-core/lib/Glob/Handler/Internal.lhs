% src/Glob/Handler/Internal.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Internal.lhs}
  \CodeInfo{some common functions}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-09-25}
  %\CodeChangeLog{}{}{}
\end{codeinfo}
\begin{code}
module Glob.Handler.Internal
       ( getFilesBS
       , getFile
       , getField
       , lookupPostUnRest
       , restItem
       ) where


--import Control.Exception (throw)
--import Control.Monad(mapM)
import Data.Conduit
import Glob.Common
import Glob.Foundation
import Glob.Model
import Glob.Types
--import Text.Blaze.Html (preEscapedToHtml)
import Yesod.Core
import Yesod.Core.Types
       
import Import
import Import.ByteStringUtf8 (toStrictBS)
--import Import.Wai(status301)
import qualified Import.Text as T
import qualified Import.ByteStringUtf8 as B
\end{code}

\begin{code}
lookupPostUnRest :: [T.Text] -> Handler (Maybe Rest)
lookupPostUnRest idx = do
  $logDebugS "Un Rest" (T.concat idx)
  ty <- lookupPostParam "type"
  ct <- lookupPostParam "create-time"
  ut <- lookupPostParam "update-time"
  ti <- lookupPostParam "title"
  su <- getField        "summary"
  wh <- lookupPostParam "whose"
  mi <- lookupPostParam "mime"
  tg <- lookupPostParams "tag"
  ts <- T.words <#> lookupPostParams "tags"
  return $ case (ty,ct,ut,ti) of
    (Just t,Just c,Just u,Just i) -> Just .Rest
      idx undefined t (T.readT c) (T.readT u) i su wh mi .concat $ tg:ts
    _ -> Nothing
\end{code}

\begin{code}
restItem :: Val a => Maybe Rest -> Maybe a
         -> ( a -> Rest -> Action Handler ())
         -> Handler TypedContent
restItem unR item f = do
  case (unR,item) of
    (Just r,Just i) -> do
      rt <- tryH.runDB' $ f i r
      returnItem rt
    _ ->  invalidArgs [" args failed"]
  where
    returnItem (Left e) = returnER e
    returnItem (Right _) = respondSource "" $ sendChunkText "success"
\end{code}




Get file's text
\begin{code}
getFilesBS :: (MonadResource a,MonadHandler a)
           => [FileInfo] -> a (Maybe B.ByteString)
getFilesBS [] = return Nothing
getFilesBS xs = Just. B.concat.concat <$>
  mapM (sourceToList.fileSource) xs

getFile :: (MonadResource a,MonadHandler a)
        => T.Text -> a (Maybe B.ByteString)
getFile file = getFilesBS =<< lookupFiles file

getField :: (MonadResource a,MonadHandler a)
         => T.Text -> a (Maybe T.Text)
getField fieled = do
  su <- b2tUtf8 <#> getFile fieled
  case su of
    Just s -> return su
    _ -> lookupPostParam fieled
\end{code}
