module Ex where

import Prelude

import Data.Array(null, (..), cons)
import Data.Array.Partial (tail, head)
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)
import Prelude
import Data.Array.Partial (head, tail)

-- Excercise 2
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
      then 1 + countEven (unsafePartial tail a)
      else 0 + countEven (unsafePartial tail a)


-- Excercise 3

cartProd :: Array Int -> Array Int -> Array (Array Int)
cartProd arr1 arr2 = do 
  i <- arr1
  j <- arr2
  [[i,j]]

triples :: Int -> Array (Array Int)
triples n = do 
  i <- 1..n
  j <- 1..n
  let val = i*i + j*j
  guard $ val <= n
  [[i,j,val]]

factorizations :: Int -> Array Int
factorizations n = 
  if n == 1
    then [1]
    else do
      i <- 2..n
      guard $ mod n i == 0   
      cons i (factorizations (div n i))
      

factorizationsN :: Int -> Int -> Array Int -> Array Int
factorizationsN 1 i a = (cons 1 a)
factorizationsN n i a = 
  if mod n i == 0
    then factorizationsN (div n i) 2 (cons i a)
    else factorizationsN n (i+1) a


-- Excercise 4

    
count :: forall a. Int -> (a -> Boolean) -> Array a -> Int
count acc _ [] = acc
count acc p xs = if p (unsafePartial head xs)
                then count (acc+1) p (unsafePartial tail xs) 
                else count (acc) p (unsafePartial tail xs)