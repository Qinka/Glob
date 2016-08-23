% src/Glob/Config.lhs

\begin{codeinfo}
  \CodePath{src/Glob/Config.lhs}
  \CodeInfo{The configure of Glob}
  \CodeProject{glob-core}
  \CodeCreater{Qinka}
  \CodeCreatedDate{2016-07-16}
  \CodeChangeLog{2016-08-19}{0.0.10.0}{changed version}
  %\CodeChangeLog{date}{text}
\end{codeinfo}

\begin{code}
module Glob.Config
    ( LogPath(..)
    , ToConfig(..)
    , defGetServerName
    , defSetSettings
    , defSigINTHandle
    ) where

      import Data.String(fromString)
      import Glob.Common
      import Glob.Types
      import System.Signal

      import Import
      import Import.Logger
      import Import.Wai hiding (getPort)

      import qualified Import.ByteStringUtf8 as B
      import qualified Import.Text as T
\end{code}



Config class
\begin{code}
      class FromJSON a => ToConfig a where
        type CfgD a
        getPort :: a -> Port
        getLogPath :: a -> LogPath
        getTimeout :: a -> Int
        getServerName :: a -> String
        getServerName = defGetServerName
        toCfgD :: a -> IO (CfgD a)
        getListenType :: a -> String
        sigINTHandle :: a -> IO () -> IO ()
        sigINTHandle = defSigINTHandle
        setSettings :: a -> Settings -> Settings
        setSettings = defSetSettings
\end{code}

Default of getServerName
\begin{code}
      defGetServerName :: ToConfig a => a -> String
      defGetServerName _ = "Glob-" ++ globCoreVersionStr
\end{code}

Default of shutdown
\begin{code}
      defSigINTHandle :: ToConfig a => a -> IO () -> IO ()
      defSigINTHandle _ f =  installHandler sigINT $ \sig ->
          if sig ==sigINT
            then f >> putStrLn "\nGoing to turn down"
            else putStrLn $ "catch: " ++ show sig
\end{code}

Default of setSettings
\begin{code}
      defSetSettings :: ToConfig a => a -> Settings -> Settings
      defSetSettings l = setInstallShutdownHandler (sigINTHandle l)
                       . setPort (getPort l)
                       . setHost (fromString $ getListenType l)
                       . setTimeout (getTimeout l)
                       . setServerName (s2bUtf8 $ getServerName l)
\end{code}
