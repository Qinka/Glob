% src/Glob/Launch.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Launch.lhs}
  \CodeInfo{The launcher of Glob}
  \CodeProject{glob-launch}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-25}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.1}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Launch
       ( Path(..)
       , parseCfgFile
       , parseCfgFileJSON
       , parseCfgFileYAML
       , readFromFile
       , readFromPath
       , runToConfig
       , globLaunchVersionQuote
       , globLaunchVersion
       , globLaunchGitBranchQuote
       , globLaunchGitCommitQuote
       , glob_middleware
       ) where

--import Glob.Common
import Glob.Common.Infos as CoreInfos
import Glob.Core
import Glob.Model
import Paths_glob_launch
       
import Control.Applicative ((<|>))
import Data.Char
import Data.String
import Development.GitRev
import System.IO

import Yesod.Core
import Network.Wai.Handler.Warp

import Network.Wai
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Gzip
import Data.Version
import Language.Haskell.TH
       
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Yaml as Y
\end{code}

the data of files' path
\begin{code}
data Path = PathFile    FilePath
          | PathStdout
          | PathStderr
          | PathStdin
          deriving (Show,Eq)
                   
instance IsString Path where
  fromString str = case lowerStr of
    "stdin"  -> PathStdin
    "stdout" -> PathStdout
    "stderr" -> PathStderr
    _        -> PathFile str
    where
      lowerStr = toLower <$> str
\end{code}

read Config from file
\begin{code}
parseCfgFileJSON :: ToConfig a => String -> Maybe a
parseCfgFileJSON str = A.decode blStr
  where blStr = BL.fromStrict $ TE.encodeUtf8 $ T.pack str
parseCfgFileYAML :: ToConfig a => String -> Maybe a
parseCfgFileYAML str = Y.decode blStr
  where  blStr = TE.encodeUtf8 $ T.pack str
parseCfgFile :: ToConfig a => String -> Maybe a
parseCfgFile str = parseCfgFileYAML str <|> parseCfgFileJSON str
readFromPath :: Path -> IO String
readFromPath (PathFile x) = readFile x
readFromPath PathStdin    = getContents
readFromPath _            = error "can not read from write-only handles"
readFromFile :: ToConfig a => FilePath -> IO (Maybe a)
readFromFile = (parseCfgFile <$>)
  . readFromPath
  . fromString
\end{code}

run from ToConfig

\begin{code}
runToConfig :: ( ToConfig a
               , CfgD a ~ b
               , Yesod b
               , YesodDispatch b
               )
            => a -> IO ()
runToConfig cfg = toCfgD cfg >>= toWaiApp >>= run
  where
    settings = setSettings cfg
    run = runSettings (settings defaultSettings).glob_middleware
\end{code}

version
\begin{code}
globLaunchVersion :: Version
globLaunchVersion = version
globLaunchVersionQuote :: Q Exp
globLaunchVersionQuote = stringE $ showVersion version
globLaunchGitBranchQuote :: Q Exp
globLaunchGitBranchQuote = gitBranch
globLaunchGitCommitQuote :: Q Exp
globLaunchGitCommitQuote = gitHash
\end{code}


\begin{code}
glob_middleware :: Middleware
glob_middleware = debugItem.gzipItem
  where
    debugItem = if CoreInfos.isDebug then logStdoutDev else id
    gzipItem =
#ifdef WithMiddlewareGzip
       gzip def
#else
       id
#endif
\end{code}
