module Chap8 where

import Prelude
import Data.Array
import Data.Maybe
import Data.List(List)
import Data.List.Lazy.Types

-- Excercise 1
third :: forall a. Array a -> Maybe a
third arr = do
  x <- tail arr
  x' <- tail x
  x'' <- head x'
  pure x''

sums :: Array Int -> Array Int
sums lst = nub $ sort $ (foldM (\a e -> [a, a+e]) 0 lst)

-- filterM' :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> List a -> m (List a)
-- filterM' p acc nil = return acc
-- filterM' p acc (Cons x xs) = do
--   keep <- p x
--   case keep of
--     true  -> filterM' p (Cons x acc) xs
--     false -> filterM' p acc xs

-- filterM :: forall m a. (Monad m) => (a -> m Boolean) -> List a -> m (List a)
-- filterM p lst = filterM' p nil lst