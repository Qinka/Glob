
\begin{code}
module Main
       ( main
       ) where


import Control.Monad
import Data.Data

import Data.IORef
import System.IO.Unsafe

import Import
import qualified Import.ByteString.Lazy as BL
import Types
\end{code}


\begin{code}
data Update = New
              { newId :: Maybe Id
              , newTyp :: Maybe String
              , newPath :: Maybe Path
              , newTitle :: Maybe Title
              , newTags :: Maybe Tags
              , newSum :: Maybe String
              , newContent :: Maybe String
              , newWhose :: Maybe Whose
              }
            | Del
            | Nav
            | Make
              { mkOpt :: String
              , mkTyp :: MakeKind
              }
            deriving (Show,Eq,Data,Typeable)
\end{code}

\begin{code}
data MakeKind = Item | Basic 
              deriving (Show,Eq,Typeable,Data)
\end{code}
for make
\begin{code}
make :: Update
make = Make
  { mkTyp = Item 
         &= typ "TYPE"
         &= argPos 0
  , mkOpt = def
         &= argPos 1
  } &=name "make"
\end{code}


for new
\begin{code}
new :: Update
new = New
  { newId      = def
              &= help "The id of the updation."
              &= typ "ID"
              &= explicit &= name "id"
              &= explicit &= name "i"
              &= groupname "new"
  , newTyp     = def
              &= help "The type of the updation."
              &= typ "TYPE"
              &= explicit &= name "type"
              &= explicit &= name "t"
              &= groupname "new"
  , newPath    = def
              &= help "The path of the url in your site."
              &= typ "URL"
              &= explicit &= name "path"
              &= name "p"
              &= groupname "new"
  , newTitle   = Just "null"
              &= help "The title of the updation."
              &= typ "TEXT"
              &= explicit &= name "title"
              &= explicit &= name "c"
              &= groupname "new"
  , newTags    = def
              &= help "The tags of the updation."
              &= typ "{TEXT}"
              &= explicit &= name "tag"
              &= explicit &= name "tags"
              &= explicit &= name "l"
              &= groupname "new"
  , newSum     = def
              &= help "The summary or the file of the updation."
              &= typ "TEXT|FILE"
              &= explicit &= name "summary"
              &= explicit &= name "sum"
              &= explicit &= name "s"
              &= groupname "new"
  , newContent = def
              &= help "The content of these kinds of resources."
              &= explicit &= name "content"
              &= groupname "new"
  , newWhose   = def
              &= help "The owner of this updation."
              &= typ "OWNER"
              &= explicit &= name "whose"
              &= explicit &= name "w"
              &= groupname "new"
  }
\end{code}

\begin{code}
makeHandle :: Update -> IO ()
makeHandle = makeHandleS
\end{code}


the handle to make
\begin{code}
makeHandleS :: Update -> IO ()
makeHandleS (Make id Item) = do
  item <- decode <$> BL.readFile (".infos/"++id++".json")
  case item of
    Nothing -> do putStrLn "Sorry, the id is error"
                  whenLoud $ putStrLn $ ".infos/"++id++".json does not exist"
    Just (Res i p ti ts s co ct w) -> do
      let dpS = case s of
            Left i -> ' ':i
            Right _ -> ""
          (typ,cn,dpF) = case co of
            Post p' -> ("post","html",' ':p')
            Frame f' -> ("frame","html",' ':f')
            Binary b' -> ("binary","binary",' ':b')
            TextFile t' -> ("text","text",' ':t')
            Static u' -> ("static","url","")
            Query v' -> ("query","var","")
      putStrLn $ id++":"++dpF++dpS
      let isPandocF = length dpF > 6 && take 5 (reverse dpF) /= ".lmth"
          isPandocS = length dpS > 6 && take 5 (reverse dpS) /= ".lmth"
      when isPandocF $ putStrLn $ "\t@pandoc -o .ignore/"++i++".html"++dpF
      when isPandocS $ putStrLn $ "\t@pandoc -o .ignore/"++i++".sum.html"++dpS
      putStrLn $ "\t@$(ECHO) $(CURL_PATH) $(CURL_DETAIL) \' -X PUT -F \"type=" ++ typ ++ "\" \' \\"
      putStrLn $ "\t\t\' -F \"create-time=" ++ (show ct) ++ "\" ' \\"
      putStrLn $ "\t\t\' -F \"update-time=$(IH_NOW)\" \' \\"
      putStrLn $ "\t\t\' -F \"title=" ++ ti ++ "\" \' \\"
      case s of
        Left f' -> putStrLn $ "\t\t\' -F \"summary=@.ignore/"++i++".sum.html\" \' \\ "
        Right "" -> return ()
        Right t' -> putStrLn $ "\t\t\' -F \"summary="++t'++"\" \' \\"
      putStrLn $ "\t\t\' -F \""++cn++"="++fetchContent co++"\" \' \\"
      case w of
        Just w' -> putStrLn $ "\t\t\' -F \"whose="++w'++"\" \' \\"
        Nothing -> return ()
      let putTag = \tag -> putStrLn $ "\t\t\' -F \"tag="++tag++"\" \' \\"
      mapM_ putTag ts
      putStrLn $ "\t\t$(SITE_URL)"++p++" \' \' | $(IH_PATH) -m -f$(IH_DELAY) -v $(PSK) | $(SHELL)"
      putStrLn $ id++".del:"
      putStrLn $ "\t@$(ECHO) $(CURL_PATH) $(CURL_DETAIL) ' -X DELETE -F \"type="++typ++"\" \' \\"
      putStrLn $ "\t\t$(SITE_URL)"++p++" \' \' | $(IH_PATH) -m -f$(IH_DELAY) -v $(PSK) | $(SHELL)\n"
      
makeHandleS (Make id Basic) = case id of
  "timecheck" -> pTimeCheck
  "cleantmp" -> pCleanTmp
  "site" -> pSite
  "shell" -> pShell
  "curl" -> pCURL
  "comment" -> pComment
  "all" -> do pComment
              pCURL
              pShell
              pSite
              pTimeCheck
              pCleanTmp
  where
    pTimeCheck = do
      putStrLn $ unlines [ "# time check #"
                         , (unlines.map ('\t':))
                           [ "check-delay:"
                           , "@$(ECHO)"
                           , "@$(ECHO) check time"
                           , "@$(ECHO) $(CURL_PATH) \' -X GET ' $(SITE_URL)/@/~servertime | $(SHELL) | $(TIMECHECK_PATH)"
                           , "@$(ECHO)"
                           ]
                         ]
    pCleanTmp = do
      putStrLn $ unlines [ "# clean #"
                         , "clean-tmp:"
                         , (unlines.map ('\t':))
                           [ "@$(ECHO) Clean .ignore/tmp.*"
                           , "@rm -f .ignore/tmp.*"
                           , "@$(ECHO) DONE"
                           ]
                         ]
    pSite = do
      putStrLn "# SITE #"
      putStrLn "## URL of site"
      site_url <- siteUrl <$> getGlobal
      putStrLn $ "SITE_URL=" ++ site_url
      putStrLn "## Password"
      psk' <- psk <$> getGlobal
      putStrLn $ "PSK=" ++ psk'
      putStrLn "## The path of glob-ih"
      ih_path <- ihPath <$> getGlobal
      putStrLn $ "IH_PATH=" ++ ih_path
      putStrLn "## The delay between server and glob-ih"
      ih_delay <- ihDelay <$> getGlobal
      putStrLn $ "IH_DELAY" ++ ih_delay
      putStrLn "## Get the time of now via glob-ih"
      ih_now <- ihNow <$> getGlobal
      putStrLn $ "IH_NOW="++ih_now
      putStrLn "## Check the delay"
      tc_path <- timeCheckPath <$> getGlobal
      putStrLn $ "TIMECHECK_PATH=" ++ tc_path ++ "\n"
    pShell = do
      putStrLn "# SHELL #"
      putStrLn "## The shell we used"
      shell_kind <- shellKind <$> getGlobal
      putStrLn $ "SHELL=" ++ shell_kind
      putStrLn "## The echo or some things like that"
      echo_kind <- echoKind <$> getGlobal
      putStrLn $ "$ECHO=" ++ echo_kind ++ "\n"
    pCURL = do
      putStrLn "# CURL #"
      putStrLn "## Path of CURL"
      curl_path <- curlPath <$> getGlobal
      putStrLn $ "CURL_PATH=" ++ curl_path
      putStrLn $ "## show details or not"
      curl_detail <- curlDetail <$> getGlobal
      putStrLn $ "CURL_DETAIL=" ++ curl_detail ++ "\n"
    pComment = do
      putStrLn   "#\n# Makefile\n#"
      putStrLn $ replicate 60 '#'
      putStrLn   "##"
      putStrLn   "##\tGlob Updating"
      putStrLn   "##"
      putStrLn   "##\tCreated by glob-update"
      putStrLn   "##\tCopyright (C) 2016"
      putStrLn   "##"
      putStrLn $ replicate 60 '#'
      putStrLn   "#\n"
\end{code}


the handle to new
\begin{code}
newHandle :: Update -> IO ()
newHandle New{..} = do
\end{code}
  get the params 
\begin{code}
  id' <- newId `fetch` getId
  typ' <- newTyp `fetch` getTyp
  path' <- newPath `fetch` getPath
  title' <- newTitle `fetch` getTitle
  tags' <- newTags `fetch` getTags
  sum' <- newSum `fetchM` getSum >>= getSum'
  content' <- newContent `fetch` getContent
  whose' <- newContent `fetchM` getWhose
  ctime <- getCurrentTime
\end{code}
transform to Res
\begin{code}
  let res = Res id' path' title' tags' sum'
                (getContent' typ' content') ctime whose'
  createDirectoryIfMissing True ".infos"
  BL.writeFile (".infos/" ++ id' ++ ".json") $ encode res
  where
    getContent' k = case k of
      "post" -> Post
      "frame" -> Frame
      "binary" -> Binary
      "text" -> TextFile
      "static" -> Static
      "query" -> Query
    getSum' (Just s) = do
      is <- doesFileExist s
      if is then return $ Left s
            else return $ Right s
    getSum' Nothing = return $ Right ""
    getWhose = do
      putStrLn "Type in the owner of thr updation:"
      x <- getLine
      return $ if null x then Nothing else Just x
    getContent = do
      putStrLn "Type in the contents of the updation:"
      x <- getLine
      return x
    getId = do
      putStrLn "Type in this updation's id:"
      x <-  (head.words) <$> getLine
      whenLoud $ putStrLn $ "Your id is " ++ x 
      return x
    getTyp  = do
      putStrLn $ "Type in the type of the updation"
        ++ "(post/frame/binary/text/static/query):"
      x <- (head.words) <$> getLine
      if x `elem` ["post","frame","binary","text","static","query"]
        then do whenLoud $ putStrLn $ "Your type is: " ++ x
                return x
        else do putStrLn "Wrong input, try more!"
                getTyp
    getPath = do
      putStrLn "Type in the url path of the updation:"
      x <- getLine
      whenLoud $ putStrLn $ "Your url is " ++ x
      return x
    getTitle = do
      putStrLn "Type in the title of the updation:"
      x <- getLine
      whenLoud $ putStrLn $ "Your title is " ++ x
      return x
    getTags = do
      putStrLn "Type in the tags of the updatiion(splited by space):"
      x <- words <$> getLine
      return x
    getSum = do
      putStrLn "Type in the kind of summary(file, text, no):"
      (head.words) <$> getLine >>= (\x -> case x of
           "file" -> Just <$> getLine
           "text" -> Just <$> getLine
           _ -> return Nothing
           )
    fetch :: Maybe a -> IO a -> IO a
    fetch (Just x) _ = return x
    fetch Nothing f = f
    fetchM :: Maybe a -> IO (Maybe a) -> IO (Maybe a)
    fetchM j@(Just x) _ = return j
    fetchM Nothing f = f
\end{code}

\begin{code}
update :: Update
update = modes [new,make]
  &= program "glob-update"
  &= summary "summary"
  &= verbosity
  where
\end{code}


\begin{code}
main :: IO ()
main = do
  getGlobal
  rt <- cmdArgs update
  main' rt
  where
    main' :: Update -> IO ()
    main' n@New{..} = newHandle n
    main' m@Make{..} = makeHandle m
\end{code}


\begin{code}
global :: IORef (Maybe GlobalSettings)
global = unsafePerformIO $ do
  is_exist <- doesFileExist ".infos/global.json"
  if is_exist
    then do x <- decode <$> BL.readFile ".infos/global.json"
            newIORef x
    else newIORef Nothing
getGlobal :: IO GlobalSettings
getGlobal = do
  x <- readIORef global
  case x of
    Just x -> return x
    Nothing -> initGlobal
  where
    initGlobal = do
      gs <- mkGlobalSetting
      writeIORef global (Just gs)
      BL.writeFile ".infos/global.json" $ encode gs
      return gs

mkGlobalSetting :: IO GlobalSettings
mkGlobalSetting = do
      putStrLn "type in the path of cURL(1):"
      curl_path <- getLine
      putStrLn "type in the flag about curl's detail mode:"
      curl_detail <- getLine
      putStrLn "type in the shell's name:"
      shell_kind <- getLine
      putStrLn "type in the echo command name of the shell:"
      echo_kind <- getLine
      putStrLn "type in the site's url:"
      site_url <- getLine
      putStrLn "type in your password(WARRNING: THE PASSWORD WILL BE STORED IN PLAIN TEXT):"
      psk' <- getLine
      putStrLn "type in your glob-ih's path:"
      ih_path <- getLine
      putStrLn "type in your delay between the site:"
      ih_delay <- getLine
      putStrLn "type in your script about get the time of \"now\":"
      ih_now <- getLine
      putStrLn "type in the glob-timecheck's path:"
      timecheck_path <- getLine
      putStrLn "type in who are your:"
      whose <- getLine
      return $ GlobalSettings curl_path curl_detail shell_kind echo_kind site_url psk' ih_path ih_delay ih_now timecheck_path whose
\end{code}


    
