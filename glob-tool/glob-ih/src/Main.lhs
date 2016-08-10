% src/Main.lhs

\begin{codeinfo}
  \CodePath{src/Main.lhs}
  \CodeInfo{Main of Glob-IH}
  \CodeProject{glob-ih}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-20}
  \CodeChangeLog{2016-08-08}{0.0.9.25}{Add the console's options}
\end{codeinfo}

\begin{code}
module Main
    ( main
    ) where

      import Control.Monad
      import Data.Char
      import Data.Maybe
      import Data.Time
      import Data.Time.Format
      import Glob.Auth.Token
      import System.Console.GetOpt
      import System.Environment
      import System.IO

      import Data.Version
      import Paths_glob_ih

      import qualified GHC.IO.Encoding as E
\end{code}

main
\begin{code}
      main :: IO ()
      main = do
#ifdef WithUTF8
        E.setLocaleEncoding E.utf8
        hSetEncoding stdout utf8
#endif
        (os,ns) <- getArgs >>= compilerOpts
        case os of
          OptHelp v    -> helpMain v
          OptTime tf f -> timeMain tf f
          OptMix   f v -> mixMain f ns v
\end{code}

the main of help
\begin{code}
      helpMain :: Bool -> IO ()
      helpMain v = if v
        then putStr $ showVersion version
        else do
          putStrLn $ usageInfo "Glob-IH helps\nUsage: glob-ih [OPTIONs] [TOKENs]"
            options
          getArgs >>= print
\end{code}

the main of time print
\begin{code}
      timeMain :: OptTimeFormat -> Int -> IO ()
      timeMain otf f = putStr =<< trans <$> getCurrentTime
        where
          trans' = case otf of
            OTFHaskell -> show
            OTFHttp    -> formatTime defaultTimeLocale "%a, %d %b %G %T GMT"
            OTFOther s -> formatTime defaultTimeLocale s
          trans = trans'.addUTCTime (fromIntegral f)
\end{code}

the main of mix
\begin{code}
      mixMain :: Int -> [String] -> Bool ->  IO ()
      mixMain f tokens v = do
        now <- getCurrentTime
        let dt       = addUTCTime (fromIntegral f) now
        let d        = formatTime defaultTimeLocale "%a, %d %b %G %T GMT" dt
        let ut       = readTime   defaultTimeLocale "%a, %d %b %G %T GMT" d :: UTCTime
        let s        = transPsk2Token (show ut) tokens
        cmds <- getContents
        put.concat.lines $ cmds
                           ++ "-H \"Token:"
                           ++ s
                           ++ "\" -H \"Date:"
                           ++ d
                           ++ "\" "
        where  put t = putStr t >> when v (hPutStrLn stderr t)
\end{code}

for Opts'

options
\begin{code}
      data Options = OptHelp
          { ohVersion    :: Bool
          }
        | OptTime
          { otTimeFormat :: OptTimeFormat
          , otTimeFix    :: Int
          }
        | OptMix
          { omTimeFix    :: Int
          , omVerbose    :: Bool
          }
\end{code}

opt descrs
\begin{code}
      options :: [OptDescr (Options -> Options)]
      options =
        [ Option ['h','?'] ["help"]   (NoArg (\_ -> defHelp)    ) "show this help"
        , Option ['t'] ["time-print"] (OptArg timeprint "format") "print the current time"
        , Option ['m'] ["mix"]        (NoArg  mix               ) "mix the things"
        , Option ['f'] ["fix"]        (ReqArg timefix "time fix") "fix the time-diff"
        , Option ['v'] ["verbose"]    (NoArg  verbose           ) "show details"
        , Option ['V'] ["version"]    (NoArg  ver               ) "show version"
        ]
\end{code}

the help
\begin{code}
      defHelp :: Options
      defHelp = OptHelp False
\end{code}

show version
\begin{code}
      ver :: Options -> Options
      ver opt@(OptHelp _) = opt {ohVersion = True}
      ver _               = defHelp {ohVersion = True}
\end{code}


time format
\begin{code}
      data OptTimeFormat = OTFHaskell
                         | OTFHttp
                         | OTFOther String
\end{code}

default time format
\begin{code}
      defOptTime :: Options
      defOptTime = OptTime OTFHaskell 0
\end{code}

time print
\begin{code}
      timeprint' :: Maybe String -> Options -> Options
      timeprint' mf opt = opt {otTimeFormat = tf}
        where
          tf = case (map toLower) <$> mf of
            Nothing        -> OTFHaskell
            Just "haskell" -> OTFHaskell
            Just "http"    -> OTFHttp
            Just _         -> OTFOther $ fromMaybe (error "how come?") mf
      timeprint mf     (OptHelp _ )  = timeprint' mf defOptTime
      timeprint mf opt@(OptTime _ _) = timeprint' mf opt
      timeprint _  opt               = opt
\end{code}

default of OptMix
\begin{code}
      defOptMix :: Options
      defOptMix = OptMix 0 False
\end{code}

for mix
\begin{code}
      mix :: Options -> Options
      mix (OptHelp _) = defOptMix
      mix opt     = opt
\end{code}

to show details
\begin{code}
      verbose :: Options -> Options
      verbose opt@(OptMix _ _) = opt {omVerbose = True}
\end{code}

time fix use -f or --fix
\begin{code}
      timefix ::String -> Options -> Options
      timefix f opt@(OptTime _ _) = opt {otTimeFix = read f}
      timefix f opt@(OptMix _ _)    = opt {omTimeFix = read f}
      timefix _ opt               = opt
\end{code}

to compiler opts
\begin{code}
      compilerOpts :: [String] -> IO (Options,[String])
      compilerOpts args =
        case getOpt Permute options args of
          (os,ns,[]) -> return (foldl (flip id) defHelp os,ns)
          (_,_,es)   -> ioError $ userError $ concat es ++ usageInfo header options
        where header = "Usage: glob-ih [OPTIONs] [TOKENs]"
\end{code}
