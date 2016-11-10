% src/Glob/Auth.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Auth.lhs}
  \CodeInfo{Authentication of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-19}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  \CodeChangeLog{2016-09-25}{0.0.10.16}{with lts-7.0}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Auth
       ( bgAuth
       ) where

import Control.Concurrent(threadDelay)
import Network.HTTP.Date
import System.Environment
import Yesod.Core
import Yesod.Core.Handler
       
import Glob.Auth.Token
import Glob.Model
import Glob.Common
       
import Import
import qualified Import.ByteStringUtf8 as B
import qualified Import.Text as T
\end{code}

用于后台验证的
\begin{code}
bgAuth :: Yesod site
       => String
       -> HandlerT site IO AuthResult
bgAuth pskKeyEnvVal = do 
  fetchToken checkToken
  where
    checkToken tokens time = do
      let strTime = show time
      (floorTime,upperTime) <- liftIO $ getTimeValidBound 6
      if floorTime <= time && upperTime >= time
        then fetchItem strTime >>= checkItem tokens
        else return $ Unauthorized "Who are you! The thing did not answer."
    fetchItem strTime = do
      psk <- liftIO $! transPsk2Token strTime.words <$> getEnv pskKeyEnvVal
      $logDebugS "Auth" (T.showT (strTime ++ " ;; " ++ psk))
      return psk
    checkItem tokens st = do
      $logDebugS "Auth Eq" (T.showT tokens)
      if s2bUtf8 st `elem` tokens
        then return Authorized
        else return $ Unauthorized ":( Who are you!"
    getTimeValidBound delta = getCurrentTime >>=
      (\utcNow -> return ( addUTCTime (-delta) utcNow
                         , addUTCTime   delta  utcNow
                         ))
    fetchToken f = do
      tokens <- lookupHeaders "Token"
      time <- parseHTTPDateMaybe <$> lookupHeader "Date"
      $logDebugS "Auth Time" (T.showT time)
      case time of
        Just ti -> f tokens ti
        _ -> invalidArgs ["Error Header","token","Date"]
    parseHTTPDateMaybe = (fromHttpDate2UTC.b2sUtf8 <$>)
\end{code}
