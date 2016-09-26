% src/newpost/Main.lhs

\begin{codeinfo}
  \CodePath{src/newpost/Main.lhs}
  \CodeProject{glob-tool}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-08-29}
\end{codeinfo}

\begin{code}
module Main
       ( main
       ) where

import Control.Monad
import Data.Aeson
import Data.Maybe
import Data.String (fromString)
import Data.Time
import Glob.Auth.Token
import Network.HTTP.Client
import Network.HTTP.Client.MultipartFormData
import System.Console.CmdArgs
import System.Console.CmdArgs.Verbosity
import System.Directory
import System.Exit
import System.IO
import System.Process

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Data.Version(showVersion)
import Paths_glob_tool(version)
\end{code}

main function
\begin{code}
main :: IO ()
main = do
\end{code}
get options
\begin{code}
  p@Post{..} <- cmdArgs newpost
  whenLoud $ print p
\end{code}
read configures from  somewhere.
get path
\begin{code}
  cfgpath <- getCfgPath
  whenLoud.putStrLn $ "config path:" ++ cfgpath
\end{code}
if file do not exist
\begin{code}
  (&& isNothing sitePsk).(&& isNothing siteUrl).not <$> doesFileExist cfgpath
     >>= whenNotExist cfgpath
\end{code}

fetch settings
\begin{code}
  settings <- decode <$> BL.readFile cfgpath
  whenLoud $ print settings
  case settings of
    Nothing -> do
      putStrLn "can not parse json file!"
      exitFailure
    Just s@Settings{..} -> do
      htmlPath <- fetchHtml postH defEditor
      sumPath <- fetchSum sumH defEditor
      fetchContents htmlPath sumPath $ uploadPost s p
\end{code}

fetchContents
\begin{code}
fetchContents :: FilePath -> Maybe FilePath
              -> (Handle -> Maybe Handle -> IO ())
              -> IO ()
fetchContents fp mfp f = do
  h <- openFile fp ReadMode
  s <- case mfp of
         Just mfp' -> Just <$> openFile mfp' ReadMode
         _ -> return Nothing
  f h s
  case s of
    Just s' -> hClose s'
    _ -> return ()
  hClose h
\end{code}

fetch summary
\begin{code}
fetchSum :: Maybe String -> String -> IO (Maybe FilePath)
fetchSum s editor = do
  case s of
    Just s' -> do
      is <- doesFileExist s'
      if is
        then return $ Just s'
        else fetchOne
    _ -> fetchOne
        where
          fetchOne = do
            name <- ()
            sPath <- (++"/newpost.new.sum.html") <$> getAppUserDataDirectory "glob"
            (_,_,_,ph) <- createProcess.shell $ unwords [editor,sPath]
            x <- waitForProcess ph
            whenLoud $ print x
            is <- doesFileExist sPath
            if x == ExitSuccess && is
              then return $ Just sPath
              else return Nothing
\end{code}

fetch post
\begin{code}
      fetchHtml :: Maybe FilePath -> String -> IO FilePath
      fetchHtml h editor = do
        case h of
          Just h' -> do
            is <- doesFileExist h'
            if is
              then return h'
              else fetchOne
          _ -> fetchOne
        where
          fetchOne = do
            hPath <- (++"/newpost.new.html") <$> getAppUserDataDirectory "glob"
            (_,_,_,ph) <- createProcess.shell $ unwords [editor,hPath]
            x <- waitForProcess ph
            whenLoud $ print x
            is <- doesFileExist hPath
            if x == ExitSuccess && is
              then return hPath
              else do
                putStrLn "edit exit failed"
                exitFailure
\end{code}


fetch content

\begin{code}
      uploadPost :: Settings -> Post
                 -> Handle -> Maybe Handle
                 -> IO ()
      uploadPost Settings{..} Post{..} hH sH = do
        hsrc <- B.hGetContents hH
        ssrc <- case sH of
          Just h -> Just <$> B.hGetContents h
          _ -> return Nothing
        let u = fromMaybe url siteUrl
        let p = fromMaybe psk sitePsk
        let d = fromMaybe dtime  $ fromIntegral <$> dTime
        whenLoud $ print hsrc
        whenLoud $ print ssrc
        now' <- addUTCTime d <$> getCurrentTime
        let httpNow = formatTime defaultTimeLocale "%a, %d %b %G %T GMT"  now'
        let ut = readTime defaultTimeLocale "%a, %d %b %G %T GMT" httpNow :: UTCTime
        let now = show ut
        let token = transPsk2Token now $ words p
        sendRequest now httpNow token (url++location) title tags hsrc ssrc
\end{code}


get cfgpath
\begin{code}
      getCfgPath :: IO FilePath
      getCfgPath =
#ifdef WIN32
        fmap (++"/newpost.json") (getAppUserDataDirectory "glob")
#else
        return "/etc/glob/newpost.json"
#endif
\end{code}

when file do not exist
\begin{code}
      whenNotExist :: FilePath -> Bool -> IO ()
      whenNotExist cfgpath is = when is $ do
        let dir = reverse.dropWhile (`notElem` ("/\\"::String)) $ reverse cfgpath
        whenLoud $ print dir
        createDirectoryIfMissing True dir
        putStrLn "you need to set settings"
        putStrLn "type in your site's url"
        u <- getLine
        whenLoud $ print u
        putStrLn "type in your password"
        putStrLn "***WARNING*** saved without hash"
        p <- getLine
        putStrLn "type in the diff of your computer and site"
        d <- fromIntegral.read <$> getLine
        putStrLn "type in your best editor"
        e <- getLine
        BL.writeFile cfgpath.encode  $ Settings u p d e
\end{code}


send request
\begin{code}
      sendRequest :: String -> String   -- time
                  -> String -> String   -- token url
                  -> String -> [String] -- title tags
                  -> B.ByteString -> Maybe B.ByteString
                  -> IO ()
\end{code}

\begin{code}
      sendRequest now httpNow token url title tags html summary = do
        manager <- newManager defaultManagerSettings
        req' <- parseRequest url
        whenLoud $ print req'
        req <- upHeader token httpNow <$> formDataBody (
            [ partLBS "type" "post"
            , partLBS "title" $ fromString title
            , partLBS "create-time" $ fromString now
            , partLBS "update-time" $ fromString now
            , partLBS "tags".fromString $ unwords ("blog":tags)
            , partFileRequestBody "html" "html" (RequestBodyBS html)
            ] ++ sumPart) req'
        whenLoud $ print req
        res <- httpLbs req manager
        whenNormal $ print res
        where
          sumPart = case summary of
            Just s -> [partFileRequestBody "summary" "summary" (RequestBodyBS s)]
            _ -> []
          upHeader t d r = r
            { method ="PUT"
            , requestHeaders = requestHeaders r ++
              [ ("Token",fromString t)
              , ("Date",fromString d)
              ]
            }
\end{code}
\begin{code}
      data Post = Post
        { title :: String
        , tags :: [String]
        , location :: String
        , siteUrl :: Maybe String
        , sitePsk :: Maybe String
        , dTime :: Maybe Int
        , postH :: Maybe FilePath
        , sumH :: Maybe FilePath
        } deriving (Show, Data, Typeable)
\end{code}
\begin{code}
      post = Post
        { title = def
            &= typ "TITLE"
            &= argPos 0
        , tags = def
            &= typ "TAGs"
            &= help "the tags"
        , location = "/"
            &= typ "PATH"
            &= argPos 1
        , siteUrl = Nothing
            &= typ "URL"
            &= help "where you post uploaded"
            &= explicit &= name "url"
            &= name "u"
        , sitePsk = Nothing
            &= typ "PASSWORD"
            &= help "the passwords"
            &= explicit &= name "passwords"
            &= name "d"
        , dTime = Nothing
            &= typ "TIME-DIFF"
            &= help "the diff of time between site and local"
            &= explicit &= name "diff"
        , postH = Nothing
            &= typ "FILE"
            &= help "main part of your file"
            &= explicit &= name "html"
        , sumH = Nothing
            &= typ "FILE"
            &= help "the summary of your post"
            &= explicit &= name "summary"
        }
      newpost = post
        &= summary ("Glob Newpost" ++ showVersion version )
        &= verbosity
        &= versionArg [summary (showVersion version)]
\end{code}

settings of auth
\begin{code}
      data Settings = Settings
        { url :: String
        , psk :: String
        , dtime :: NominalDiffTime
        , defEditor :: String
        } deriving (Show)
      instance ToJSON Settings where
        toJSON Settings{..} = object
          [ "url" .= url
          , "passwords" .= psk
          , "d-time" .= dtime
          , "default-editor" .= defEditor
          ]
      instance FromJSON Settings where
        parseJSON (Object v) = Settings
          <$> v .: "url"
          <*> v .: "passwords"
          <*> v .: "d-time"
          <*> v .: "default-editor"
\end{code}