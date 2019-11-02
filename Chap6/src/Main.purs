module Main where

import Prelude
import Data.Picture
import Data.Array
-- EX 1 from chapter 6

-- instance showShape :: Show Shape where 
--   show s = showShape s

-- Ex2 from chapter 6

newtype Complex = Complex
  { 
    real :: Number
  , imaginary :: Number
  }

instance showComplex :: Show Complex where
  show (Complex r) = (show r.real) <> "  " <> (show r.imaginary) 

instance eqComp :: Eq Complex where
  eq (Complex a) (Complex b) = 
    if (a.real == b.real) && (a.imaginary == b.imaginary) 
      then true
      else false
    

-- Ex3 
data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty a ax) = show (a:ax) 