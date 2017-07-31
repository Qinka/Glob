

module Glob.Tool.Make
  (
  ) where

import           Control.Monad
import           Data.Functor
import           Data.Text.Internal.Builder
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy             as TL
import qualified Glob.Import.Text           as T
import           Glob.Tool.Opt
import           Glob.Tool.Repo

-- | MakeM
data MakeM a = MakeM { mkBuilder  :: Builder
                     , mkConstant :: a
                     }

instance Functor MakeM where
  fmap f (MakeM mf c) = MakeM mf (f c)

instance Applicative MakeM where
  pure c = MakeM (Makefile mempty) c
  (<*>) (MakeM a f) (MakeM b c) = MakeM (a `mappend` b) (f c)

instance Monad MakeM where
  (>>=) (MakeM a c) f = let MakeM b t = f c
                        in MakeM (a `mappend` b) t

endLine :: MakeM ()
endLine = MakeM (fromText "\n") ()

-- | add prefix for each line
--   o(n)
linesPrefix :: Text -- ^ prefix
            -> MakeM a -- ^ items
            -> MakeM a
linesPrefix prefix (Make builder c) =
  let (t:ts) = map (\x -> fromText prefix `mappend` fromText x) $
               T.lines $ TL.toStrict $ toLazyText builder
      new    = foldl (\a b -> a `mappend` singleton '\n' `mappend` b) t ts
  in Make new c

stringT :: T.Text -> MakeM ()
stringT str = MakeM (fromText str) ()
charT :: Chat -> MakeM ()
charT c = MakeM (singleton c) ()

target :: T.Text -- ^ target
       -> [T.Text] -- ^ dependences
       -> MakeM () -- ^ body
       -> MakeM ()
target tar deps body = do
  stringT tar
  charT ':'
  forM_ (\t -> charT ' ' >> stringT t) >> endLine
  linesPrefix "\t" body

comment :: T.Text
        -> MakeM ()
comment c = linesPrefix "# " $ stringT c


(\=\) :: T.Text -> T.Text -> MakeM ()
var \=\ value = MakeM (fromText var `mappend` fromText "=" `mappend` fromText value) ()


-- | echo
echo :: Text -> MakeM ()
echo t = MakeM (fromText "echo" `mappend` fromText t `mappend` singleton '\n') ()


mkClean :: MakeM ()
mkClean = do
  comment "clean"
  target "clean" [] $ do
    echo "Clean .ignore/tmp.*"
    stringT "@rm -rf .ignore/tmp.*" >> endLine
    echo "Down"

mkSettings :: MakeM ()
mkSettings = do
  return ()




