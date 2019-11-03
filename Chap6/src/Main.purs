module Main where

import Prelude
import Data.Picture
import Data.Array
-- EX 1 from chapter 6

instance showShape :: Show Shape where 
  show s = showShape s

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

instance eqNonEmpty :: (Eq a, Eq (Array a)) => Eq (NonEmpty a) where
  eq (NonEmpty a ax) (NonEmpty b bx) = (a==b)&&(ax==bx)

instance semiGroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty a ax) (NonEmpty b bx) = (NonEmpty a (ax <> (b:bx)))

instance functorNonEmpty :: Functor (NonEmpty) where
  map f (NonEmpty a ax) = (NonEmpty (f a) (map f ax))

data Extended a = Finite a | Infinite

equalsExtended :: forall a. (Eq a) => (Extended a) -> (Extended a) -> Boolean
equalsExtended (Finite x) (Finite y) = eq x y
equalsExtended (Finite x) Infinite = false
equalsExtended Infinite (Finite y) = false
equalsExtended Infinite Infinite = true

notEqualsExtended :: forall a. (Eq a) => (Extended a) -> (Extended a) -> Boolean
notEqualsExtended e e' = not $ equalsExtended e e'

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq e e' = equalsExtended e e'
  
orderExtended :: forall a. (Ord a) => Extended a-> Extended a-> Ordering
orderExtended (Finite x) (Finite y) = compare x y
orderExtended (Finite x) (Infinite) = LT
orderExtended Infinite (Finite y) = GT
orderExtended Infinite Infinite = EQ

instance ordExtended :: (Ord a) => Ord (Extended a) where
  compare e e' = orderExtended e e'


instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f a (NonEmpty x xs) = foldr f a (x:xs)
  foldl f a (NonEmpty x xs) = foldl f a (x:xs)
  foldMap f (NonEmpty x xs) = foldMap f (x:xs)
