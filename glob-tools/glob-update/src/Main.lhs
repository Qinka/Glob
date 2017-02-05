
\begin{code}
module Main
       ( main
       ) where


import Control.Monad
import Data.Data
import Data.IORef
import System.IO.Unsafe

import Glob.Update

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
            | Script
              { scKind :: String
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


\begin{code}
script :: Update
script = Script
  { scKind = "all"
          &= typ "KIND"
          &= argPos 0
  } &=name "script"
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
scriptHandle :: Update -> IO ()
scriptHandle (Script kind) = do
  print $ case kind of
    "all" -> do
      "script for fix the bug in \"whose\"(fix-whose): "
      scriptFixWhose >> endLine
      "script for make all(make-all): "
      scriptMakeAll >> endLine
      "script for update all(update-all): "
      scriptUpdateAll >> endLine
      "script for delete all(delete-all): "
      scriptDeleteAll >> endLine
    "fix-whose" -> scriptFixWhose
    "make-all" -> scriptMakeAll
    "update-all" -> scriptUpdateAll
    "delete-all" -> scriptDeleteAll


scriptFixWhose :: UpdateM ()
scriptFixWhose = do
  "sed -i \'s/\\(,\"whose\":\"[^\"]*\"\\)//\' .infos/*.json"
scriptMakeAll :: UpdateM ()
scriptMakeAll = do
  "ls -1 .infos  | sed \'s/.json//\' | sed \'/global/d\' | awk \'{print \"glob-update make item  \"$0}\' "

scriptUpdateAll,scriptDeleteAll :: UpdateM ()
scriptUpdateAll = do
  "ls -1 .infos  | sed \'s/.json//\' | sed \'/global/d\' | "
  "sed \':a;N;$ s/\\n/ /g;ba\' | "
  "awk \'{print \"make \"$0}\'"
scriptDeleteAll = do
  "ls -1 .infos  | sed \'s/.json//\' | sed \'/global/d\' | awk \'{print $0\".del\"}\' | "
  "sed \':a;N;$ s/\\n/ /g;ba\' | "
  "awk \'{print \"make \"$0}\'"
  
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
    Just (Res i su ti ts s co ct w) -> do
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
      let isPandoc fn = (fn,length fn > 6 && take 5 (reverse fn) /= "lmth.")
      print $ mkUpdate id (isPandoc dpF,isPandoc dpS) ct ti i ts su typ (cn,fetchContent co) s w
 
      

      
makeHandleS (Make id Basic) = do
  su    <- siteUrl       <$> getGlobal
  pri   <- privateKey    <$> getGlobal
  ihp   <- ihPath        <$> getGlobal
  ihd   <- ihDelay       <$> getGlobal
  ihn   <- ihNow         <$> getGlobal
  tc    <- timeCheckPath <$> getGlobal
  shell <- shellKind     <$> getGlobal
  echo  <- echoKind      <$> getGlobal
  cp    <- curlPath      <$> getGlobal
  cd    <- curlDetail    <$> getGlobal
  utc   <-                   getCurrentTime
  let mkCHighlight = mkChange ("change-code-highlight","Change Site Code Highlight") "/@/~highlight" "CODE_STYLE" utc
      mkCTheme     = mkChange ("change-site-theme","Change Site Theme") "/@/~site-theme" "SITE_STYLE" utc
  print $ case id of
    "timecheck"    -> mkTimeCheck
    "cleantmp"     -> mkClean
    "site"         -> mkSettingSite su pri ihp ihd ihn tc
    "shell"        -> mkSettingShell shell echo
    "curl"         -> mkSettingcURL cp cd
    "comment"      -> mkComment
    "change-theme" -> mkCTheme
    "change-highlight" -> mkCHighlight
    "all"          -> do mkComment
                         mkSettingcURL cp cd
                         mkSettingShell shell echo
                         mkSettingSite su pri ihp ihd ihn tc
                         mkTimeCheck
                         mkClean
                         mkCTheme
                         mkCHighlight
     
--mkUpdate :: UpdateM ()
mkUpdate tag ((dpF,isF),(dpS,isS)) ct ti i ts su typ (cn,co) s w = do
  target tag [dpF,dpS] $ do
    let chg is ii dp = when (is && (typ == "post" || typ == "frame")) $ cmd $ do
          string $ "pandoc -o " ++ toHtml (tail dp) ++ dp
          endLine
    chg isF  i          dpF
    chg isS (i++".sum") dpS
    echoM $ do
      tCurlPath
      tCurlDetail
      tCurlMethod "PUT" >> nonEndLine
      tCurlF "type" typ
      tCurlF "create-time" $ show ct
      tCurlF "update-time" "$(IH_NOW)"
      tCurlF "title" ti
      tCurlF cn ('@':co)
      tCurlF "sha-file-name" "/`$(MD5) $(PRIVATE_KEY).pub`"
      case s of
        Left f'  -> tCurlF "summary" $ "@" ++ (reverse $ dropWhile(/='.') $ reverse co) ++ "sum.html"
        Right "" -> return ()
        Right t' -> tCurlF "summary" t'
      case w of
        Just w -> tCurlF "whose" w
        _ -> return ()
      mapM_ (\x -> tCurlF "tag" x) ts
      tSiteUrl su
      tShellPipe
      string " $(IH_PATH) -m "
      string " -f \'$(IH_DELAY)\'"
      string " -p \'$(PRIVATE_KEY)\'"
      string " -d \'$(SITE_DELTA)\' -v"
      tShellPipe >>  tShell >> endLine
  target (tag ++ ".del") [] $ echoM $ do
    tCurlPath
    tCurlDetail
    tCurlMethod "DELETE" >> nonEndLine
    tCurlF "type" typ
    tCurlF "sha-file-name" "/`$(MD5) $(PRIVATE_KEY).pub`"
    tSiteUrl su
    tShellPipe
    string " $(IH_PATH) -m "
    string " -f \'$(IH_DELAY)\'"
    string " -p \'$(PRIVATE_KEY)\'"
    string " -d \'$(SITE_DELTA)\' -v"
    tShellPipe >>  tShell >> endLine

mkChange :: (String,String) -> String -> String -> UTCTime ->  UpdateM ()
mkChange (t,tc) path var utc = do
  comP tc
  target t [] $ do
    aLine $ "OLD=$($(CURL_PATH) -X GET $(SITE_URL)"++path++")"
    cmd $ aLine "if [ \"$(OLD)\" = \"{\\\"error\\\":\\\"not found\\\"}\" ]; then OLD=\"\";fi;"
    cmd $ aLine "$(ECHO) The old theme is $(OLD_THEME)"
    cmd $ aLine $ "$(ECHO) The new theme is $("++var++")"
    cmd $ string $ "if [ \"$(OLD)\" = \"$("++var++")\" ]; then $(ECHO) The new one is eq2 old one. DO NOTHING; \\\n"
    string $ "\telse $(ECHO) $(CURL_PATH) $(CURL_DETAIL)  -X PUT  -F \\\"sha-file-name=/`$(MD5) $(PRIVATE_KEY).pub`\\\" -F \\\"var=$("++var++")\\\" -F \\\"type=query\\\" -F \\\"create-time=" ++ show utc ++ "\\\" -F \\\"update-time=$(IH_NOW)\\\" -F \\\"title=query\\\"  \\\n"
    string $ "\t$(SITE_URL)"++path++" ' '  | $(IH_PATH) -m -f$(IH_DELAY) -p$(PRIVATE_KEY) -d$(SITE_DELTA) -v  | $(SHELL) ; fi"
    
                   
mkClean :: UpdateM ()
mkClean = do
  comP "clean"
  target "clean-tmp" [] $ do
    echo "Clean .ignore/tmp.*"
    cmd $ string "rm -rf .ignore/tmp.*" >> endLine
    echo "DONE"
  
mkTimeCheck :: UpdateM ()
mkTimeCheck = do
  comP "TIME CHECK"
  target "check-delay" [] $ do
    echo ""
    echo "check time"
    echoM $ do
      tCurlPath
      tCurlMethod "GET"
      tSiteUrl "/@/~servertime"
      tShellPipe >> tShell >> tShellPipe
      string "$(TIMECHECK_PATH)"
    echo ""

mkSettingSite :: String -> String -> String
              -> String -> String -> String -> UpdateM ()
mkSettingSite su pri ihp ihd ihn tc = do
  comP "SITE"
  comments 2 "URL of site"
  "SITE_URL" \=\ su
  comments 2 "Private key file"
  "PRIVATE_KEY" \=\ pri
  comments 2 "The path of glob-ih"
  "IH_PATH" \=\ ihp
  comments 2 "The delay between server and glob-ih"
  "IH_DELAY" \=\ ihd
  comments 2 "Get the time of now via glob-ih or date"
  "IH_NOW" \=\ ihn
  comments 2 "Check the delay"
  "TIMECHECK_PATH" \=\ tc
  comments 2 "Delta of site's check"
  "SITE_DELTA" \=\ "6"
  comments 2 "MD5 cmd"
  "MD5" \=\ "md5"
  comments 2 "Site Theme"
  "SITE_THEME" \=\ "hack"
  "CODE_THEME" \=\ "default"

mkSettingShell :: String -> String -> UpdateM ()
mkSettingShell shell echo = do
  comP "SHELL"
  comments 2 "The shell will be used"
  "SHELL" \=\ shell
  comments 2 "The echo or some things like that"
  "ECHO" \=\ echo
      
mkSettingcURL :: String -> String -> UpdateM ()
mkSettingcURL cp cd = do
  comP "cURL"
  comments 2 "Path of cURL"
  "CURL_PATH" \=\ cp
  comments 2 "show details(flag, if don\'t want => spaces)"
  "CURL_DELTAIL" \=\ cd

mkComment :: UpdateM ()
mkComment = do
  string "#" >> endLine
  comP "Makefile"
  chars 60 '#' >> endLine
  comments 2 ""
  comS 2 "Glob Updating"
  comments 2 ""
  comS 2 "Created by glob-update"
  comS 2 "Copyright (C) 2016-2017"
  comments 2 ""
  chars 60 '#' >> endLine
  string "#" >> endLine  
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
update = modes [new,make,script]
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
    main' s@Script{..} = scriptHandle s
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
      putStrLn "type in your private key's path:"
      psk' <- getLine
      putStrLn "type in your delta settings:"
      delta <- getLine 
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
      return $ GlobalSettings curl_path curl_detail shell_kind echo_kind site_url psk' delta ih_path ih_delay ih_now timecheck_path whose
\end{code}


    
