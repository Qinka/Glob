% src/Glob/Model.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Model.lhs}
  \CodeInfo{The Model of Glob, here I used MongoDB}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-17}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Model
    ( module X
    , Mongoble(..)
    , getDBCPoolM
    , runDB, runDB'
    , ConnectionPool(..)
    , fetchFrame, fetchRest, fetchFrame'
    , fetchPost, fetchContext, fetchStatic
    , fetchResourceT, fetchResourceB
    , updateRest, updateContext, updateItem
    , updateFrame, updatePost, updateStatic
    , updateResourceT,updateResourceB
    , deleteRest, deleteContext, deleteContextM
    , deleteItem
    ) where

      import Control.Exception hiding (Handler)
      import Control.Monad.IO.Class
      import Data.Pool
      import Glob.Common
      import Glob.Types
      import Yesod.Core

      import Database.MongoDB as X
      import qualified Import.Text as T
      import qualified Import.ByteStringUtf8 as B
      import qualified Text.Blaze.Html as TBH (preEscapedToHtml)
\end{code}

for preEscapedToHtml
\begin{code}
      preEscapedToHtml :: T.Text -> Html
      preEscapedToHtml = TBH.preEscapedToHtml
\end{code}

to operate DB
\begin{code}
      class Mongoble a where
        getDBCPool :: a -> ConnectionPool
        getDefaultAM :: a -> AccessMode
        getDefaultAM = defaultAM
        getDefaultDB :: a -> Database
        getDefaultDB = defaultDB
        getDBUP :: a -> (T.Text,T.Text)

      defaultAM _ = master
      defaultDB _ = "master"
\end{code}

handler
\begin{code}
      type Handler site = HandlerT site IO
\end{code}

for IO
\begin{code}
      getDBCPoolM :: (Yesod site,Mongoble site)
                  => Handler site (Pool Pipe)
      getDBCPoolM = getDBCPool <$> getYesod
\end{code}

runDB function
\begin{code}
      runDB' :: (Yesod site,Mongoble site)
             => Action (Handler site) a
             -> HandlerT site IO a
      runDB' f = getYesod >>= \site ->
        runDB (getDefaultAM site)
              (getDefaultDB site)
              f
\end{code}

runDB function
\begin{code}
      runDB :: (Yesod site,Mongoble site)
            => AccessMode -> Database
            -> Action (Handler site) a
            -> HandlerT site IO a
      runDB am db f = getDBCPoolM >>= \pool ->
        withResource pool $ \p -> getYesod >>= (\site ->
        access p am db (toAuth site >> f))
        where
          toAuth site = uncurry auth $ getDBUP site
\end{code}


connection pool
\begin{code}
      type ConnectionPool = Pool Pipe
\end{code}

fetch
\begin{code}
      fetchRest :: [T.Text] -> Action (Handler site) (Maybe Rest)
      fetchRest idx = (doc2Rest <%>).findOne $ select ["index" =: idx] "index"
      fetchContext :: Val a => T.Text -> Rest -> T.Text -> Action (Handler site) (Maybe a)
      fetchContext field rest = ((!? field) <%>).findOne.select ["_id" =: rRest rest]

      fetchFrame :: Rest -> Action (Handler site) (Maybe Html)
      fetchFrame rest = preEscapedToHtml <#> fetchContext "html" rest "frame"
      fetchFrame' :: [T.Text] -> Action (Handler site) (Maybe Html)
      fetchFrame' idx = do
        rest <- fetchRest idx
        case rest of
          Just r -> fetchFrame r
          _ -> return Nothing
      fetchPost :: Rest -> Action (Handler site) (Maybe Html)
      fetchPost rest = preEscapedToHtml <#> fetchContext "html" rest "post"
      fetchResourceT :: Rest -> Action (Handler site) (Maybe T.Text)
      fetchResourceT rest = fetchContext "text" rest "resource"
      fetchResourceB :: Rest -> Action (Handler site) (Maybe B.ByteString)
      fetchResourceB rest = fromBinary <#> fetchContext "binary" rest "resource"
      fetchStatic :: Rest -> Action (Handler site) (Maybe T.Text)
      fetchStatic rest = fetchContext "url" rest "static"
\end{code}

\begin{code}
      updateRest :: Rest -> Action (Handler site) ()
      updateRest rest@Rest{..} = upsert
        (select ["index"=:rIndex] "index") $ rest2Doc rest
      updateContext :: Val a => T.Text -> Maybe ObjectId
                             -> T.Text -> a -> Action (Handler site) ObjectId
      updateContext db oid field v = case oid of
          Just o -> upsert (select ["_id"=:o] db) [field =: v] >> return o
          _ -> (\(ObjId i) -> i) <$> insert db [field =: v]
      updateItem :: Val a => T.Text -> T.Text
                          -> a -> Rest -> Action (Handler site) ()
      updateItem typ field d unR = do
        rest <- fetchRest idx
        rr <- if (rType <$> rest) /= Just typ
          then deleteContextM rest typ >> return Nothing
          else return $ rRest <$> rest
        rO <- updateContext typ rr field d
        updateRest (unR {rRest = rO})
        where idx = rIndex unR

      updateFrame ::  T.Text -> Rest -> Action (Handler site) ()
      updateFrame = updateItem "frame" "html"
      updatePost :: T.Text -> Rest -> Action (Handler site) ()
      updatePost = updateItem "post" "html"
      updateResourceT :: T.Text -> Rest -> Action (Handler site) ()
      updateResourceT = updateItem "resource" "text"
      updateResourceB :: Binary -> Rest -> Action (Handler site) ()
      updateResourceB = updateItem "resource" "binary"
      updateStatic :: T.Text -> Rest -> Action (Handler site) ()
      updateStatic = updateItem "static" "url"
\end{code}

\begin{code}
      deleteRest :: Rest -> Action (Handler site) ()
      deleteRest Rest{..} =
        delete $ select ["index" =: rIndex] "index"
      deleteContext :: Rest -> T.Text -> Action (Handler site) ()
      deleteContext Rest{..} db =
        delete $ select ["_id" =: rRest] db
      deleteContextM :: Maybe Rest -> T.Text -> Action (Handler site) ()
      deleteContextM (Just rest) = deleteContext rest
      deleteContextM _           = \_ -> return ()
      deleteItem :: [T.Text] -> T.Text -> Action (Handler site) ()
      deleteItem idx db = fetchRest idx >>=
        (\rest -> case rest of
          Just r -> deleteContext r db >> deleteRest r
          _ -> return ())
\end{code}
