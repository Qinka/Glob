

for update monad

\begin{code}
module Glob.Update
       ( UpdateM(..)
       , chars, string, endLine, nonEndLine, tabs
       , comS, comment, comments, comPs, comP
       , target, (\=\), cmd, echo, echoM, tCurlPath
       , tCurlMethod, tCurlDetail, tCurlF, tSiteUrl
       , tShell, tShellPipe, tShellOutputPipe, aLine                    
       ) where
\end{code}


\begin{code}
import Control.Monad
import Data.Functor
import Data.String
\end{code}


\begin{code}


data UpdateM a = UpdateM
               { strUM :: [String]
               , conUM :: a
               } 
\end{code}

\begin{code}
instance Show (UpdateM a) where
  show (UpdateM strs _) = concat strs
\end{code}

\begin{code}
instance Functor UpdateM where
  fmap f (UpdateM str t) = UpdateM str $ f t
\end{code}

\begin{code}
instance Applicative UpdateM where
  pure a = UpdateM [""] a
  (<*>) (UpdateM str1 f) (UpdateM str2 t) = UpdateM (str1 ++ str2)  $ f t
\end{code}

\begin{code}
instance Monad UpdateM where
  (>>=) (UpdateM str t) f = let UpdateM str' t' = f t
                            in UpdateM (str ++ str') t'
\end{code}


\begin{code}
instance IsString (UpdateM a) where
  fromString str = UpdateM [str] undefined
\end{code}

  
\begin{code}
chars :: Int -> Char -> UpdateM ()
chars i c = UpdateM [replicate i c] ()
string :: String -> UpdateM ()
string str = UpdateM [str]()
endLine :: UpdateM ()
endLine = UpdateM ["\n"] ()
nonEndLine :: UpdateM ()
nonEndLine = UpdateM [" \\\n\t"] ()
tabs :: Int -> UpdateM ()
tabs i = UpdateM [replicate i '\t'] ()
aLine :: String -> UpdateM ()
aLine str = string str >> endLine
\end{code}

\begin{code}
comS :: Int -> String -> UpdateM()
comS i str = UpdateM [replicate i '#','\t':str,"\n"] ()
comment :: String -> UpdateM ()
comment = comments 1
comments :: Int -> String -> UpdateM ()
comments i str = UpdateM [replicate i '#',' ':str,"\n"] ()
comPs :: Int -> String -> UpdateM ()
comPs i str = UpdateM [replicate i '#',' ':str,' ':replicate i '#',"\n"] ()
comP :: String -> UpdateM ()
comP  = comPs 1

\end{code}

\begin{code}
target :: String -> [String] -> UpdateM () -> UpdateM ()
target tag deps (UpdateM sls ()) = UpdateM [tl,oths++"\n"] ()
  where tl   = tag ++ ":" ++ unwords deps ++ "\n"
        oths = unlines $ ('\t':) <$> (lines $ concat sls)
\end{code}

\begin{code}
(\=\) :: String -> String -> UpdateM ()
(\=\) var value = UpdateM [var,"=",value,"\n"] ()
\end{code}


\begin{code}
cmd :: UpdateM () -> UpdateM ()
cmd m = UpdateM ["@"] () >> m

echo :: String -> UpdateM()
echo str = cmd $ UpdateM ["$(ECHO) ",str,"\n"] ()
echoM :: UpdateM () -> UpdateM ()
echoM (UpdateM strs _) = cmd $ UpdateM ["$(ECHO) ",concat strs] ()

tCurlPath :: UpdateM ()
tCurlPath = UpdateM ["$(CURL_PATH)"] ()
tCurlMethod :: String -> UpdateM ()
tCurlMethod m = UpdateM [" -X ",m," "] ()
tCurlDetail :: UpdateM ()
tCurlDetail = UpdateM [" $(CURL_DETAIL) "] ()
tCurlF' :: String -> String -> UpdateM ()
tCurlF' var value = UpdateM ["\' -F \"",var,'=':value,"\" \'"] ()
tCurlF :: String -> String -> UpdateM ()
tCurlF var value = tCurlF' var value >> nonEndLine


tSiteUrl :: String -> UpdateM ()
tSiteUrl url = UpdateM [" $(SITE_URL)",url," "] ()
\end{code}

\begin{code}
tShell :: UpdateM ()
tShell = UpdateM ["$(SHELL)"] ()
tShellPipe :: UpdateM ()
tShellPipe = UpdateM [" | "] ()
tShellOutputPipe :: String -> UpdateM ()
tShellOutputPipe fp = UpdateM [" > ",fp] ()
\end{code}

