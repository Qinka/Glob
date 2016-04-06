




-- src/Import/TH.hs



module Import.TH
    ( module X
    , persistFileWithC
    , parseRoutesFileC
    , hamletFileT
    , juliusFileT
    , cassiusFileT
    , luciusFileT
    , whamletFileT
    ) where

      import Language.Haskell.TH as X
      import Database.Persist.Quasi as X
      import Text.Hamlet as X
      import Text.Julius as X
      import Text.Cassius as X
      import Text.Lucius as X
      import Text.Blaze.Html as X hiding (Tag)
      import Yesod as X hiding (Env)

      persistFileWithC :: PersistSettings
                       -> FilePath
                       -> Q Exp
      persistFileWithC s = persistFileWith s.("config/"++)

      parseRoutesFileC :: FilePath -> Q Exp
      parseRoutesFileC = parseRoutesFile.("config/"++)

      whamletFileT :: FilePath -> Q Exp
      whamletFileT = whamletFile.("template/"++)

      hamletFileT :: FilePath -> Q Exp
      hamletFileT = hamletFile.("template/"++)

      juliusFileT :: FilePath -> Q Exp
      juliusFileT = juliusFile.("template/"++)

      cassiusFileT :: FilePath -> Q Exp
      cassiusFileT = cassiusFile.("template/"++)

      luciusFileT :: FilePath -> Q Exp
      luciusFileT = luciusFile.("template/"++)
