module Ex where

import Prelude

import Data.Array(null,unsafePartial,null)


checkEven :: Int -> Boolean
checkEven a = 
  if mod a 2 == 0
    then true
    else false

countEven :: Array Int -> Int
countEven a = 
  if null a
    then 0
    else if checkEven $ unsafePartial $ head a
      then 1 + countEven $ unsafePartial $ tail a
      else 0 + countEven $ unsafePartial $ tail a