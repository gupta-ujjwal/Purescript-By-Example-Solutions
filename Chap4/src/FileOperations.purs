module FileOperations where

import Prelude

import Data.Path (Path, ls, isDirectory, size, root)
import Data.Array (concatMap, (:), length, filter)
import Control.MonadZero (guard)
import Data.Array.Partial (head)
import Partial.Unsafe (unsafePartial)
import Data.Foldable(foldl)
import Data.Maybe

allFiles :: Path -> Array Path
allFiles file = file : do
  child <- ls file
  allFiles child

onlyFiles :: Path -> Array Path
onlyFiles file = filter (\d -> (isDirectory d)==false) (allFiles file)

maxFileSize :: Path -> Path
maxFileSize file = foldl (\acc f -> if (size f) > (size acc) then f else acc) (unsafePartial head (onlyFiles file)) (onlyFiles file)

whereIs :: Path -> Array (Array Path)
whereIs file = do 
  ch <- allFiles root
  guard $ ch == file
  [[ch]]
  

  