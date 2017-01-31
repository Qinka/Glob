% src/Glob/Handler/Fetch.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Handler/Fetch.lhs}
  \CodeInfo{The handler of GET method}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{0.0.9.25}{2016.08.08}{Add the author to blogs and pages.}
  \CodeChangeLog{0.0.9.26}{2016.08.09}{add  the version of auth}
  \CodeChangeLog{0.0.9.30}{2016.08.12}{made bloglist more powerful}
  \CodeChangeLog{0.0.9.31}{2016.08.13}{ change to use  HTTP date}
  \CodeChangeLog{0.0.10.0}{2016.08.13}
    { change version to 0.0.10.0 and rewrote some things.}
  \CodeChangeLog{0.0.10.10}{2016.08.23}{using tryH to catch exception}
  \CodeChangeLog{0.0.10.10}{2016.08.23}{change to use stream}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  \CodeChangeLog{0.0.10.31}{2016.11.09}{add tag to the post}}
\end{codeinfo}

\begin{code}
module Glob.Handler.Fetch
       ( getPostR
       , getStaticR
       , getResourceR
       , restPostR
       , restStaticR
       , restResourceR
       , restFrame
       ) where

import Control.Exception (throw)
import Control.Monad(mapM)
import Data.Conduit
import Glob.Common
import Glob.Foundation
import Glob.Model
import Glob.Types
import Glob.Handler.Internal
import Text.Blaze.Html (preEscapedToHtml)
import Yesod.Core
import Yesod.Core.Types
       
import Import
import Import.ByteStringUtf8 (toStrictBS)
import Import.Wai(status301)
import qualified Import.Text as T
import qualified Import.ByteStringUtf8 as B
\end{code}


pages
\begin{code}
getPostR :: Maybe Rest -> Handler TypedContent
getPostR (Just rest@Rest{..}) = do
  html <- runDB' $ fetchPost rest
  isRaw <- null <$> lookupHeaders "RAW123RAW"
  case html of
    Just pH -> respondHTML isRaw pH
    _ -> notFound
  where
    withTags xs = toWidget [julius|tags=#{xs};|]
    withSummary (Just sH) = [whamlet|<summary id=sum>#{sH}|]
    withSummary _ = return ()
    withWhose (Just au) = do
      let whose = showJS au
      toWidget [julius|author=#{whose};|]
    withWhose _  = toWidget [julius|author=null;|]
    withHTML pH sH = defaultLayout $ do
      setTitle $ toHtml $ rTitle
      withSummary sH
      [whamlet|#{pH}|]
      withWhose rWhose
      withTags $ toJSON rTags
    respondHTML False pH = respondSource "text/html" $ do
      sendChunkHtml pH
      sendFlush
    respondHTML True pH = do
      let sH = preEscapedToHtml <$> rSummary
      html <- withHTML pH sH
      respondSource "text/html" $ do
        sendChunkHtml html
        sendFlush
    getPostR _ = notFound
\end{code}

\begin{code}
restPostR :: [T.Text] -> Handler TypedContent
restPostR idx = do
  unR <- lookupPostUnRest idx
  html <- b2tUtf8 <#> getFile "html"
  restItem unR html updatePost
\end{code}

resource
\begin{code}
getResourceR :: Bool -> Maybe Rest -> Handler TypedContent
getResourceR t (Just rest@Rest{..}) = do
  ct <- runDB' $ fetchItem rest
  case ct of
    Just (Left  text) -> respondSource (gmime rMIME) $ do
      sendChunkText text
      sendFlush
    Just (Right binary) -> respondSource (gmime rMIME) $ do
      sendChunkBS binary
      sendFlush
    _ -> notFound
  where
    gmime = fromMaybe "".(t2bUtf8 <$>)
    fetchItem :: Rest-> Action Handler (Maybe (Either T.Text B.ByteString))
    fetchItem = if t
                then (Left  <#>) <$> fetchResourceT
                else (Right <#>) <$> fetchResourceB
getResourceR _ _ = notFound
\end{code}

\begin{code}
restResourceR :: Bool -> [T.Text] -> Handler TypedContent
restResourceR t idx = do
  unR <- lookupPostUnRest idx
  text <- b2tUtf8 <#> getFile "text"
  bin <- getFile "binary"
  if t
    then restItem unR text updateResourceT
    else restItem unR (Binary <$> bin) updateResourceB
\end{code}

\begin{code}
getStaticR :: Maybe Rest -> Handler TypedContent
getStaticR (Just rest@Rest{..}) = do
  url <- runDB' $ fetchStatic rest
  case url of
    Just u -> redirectWith status301 u
    _ -> notFound
getStaticR _ = notFound
\end{code}

\begin{code}
restStaticR :: [T.Text] -> Handler TypedContent
restStaticR idx = do
  unR <- lookupPostUnRest idx
  url <- lookupPostParam "url"
  restItem unR url updateStatic
\end{code}

\begin{code}
restFrame :: [T.Text] -> Handler TypedContent
restFrame idx = do
  unR <- lookupPostUnRest idx
  html <- b2tUtf8 <#> getFile "html"
  restItem unR html updateFrame
\end{code}
