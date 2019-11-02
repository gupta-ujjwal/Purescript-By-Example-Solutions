module Main where

import Prelude
import Data.Maybe
import Control.Apply ((*>), lift2)

-- EX 1
liftedAdd :: forall a. (Semiring a) => Maybe a -> Maybe a -> Maybe a 
liftedAdd a b = lift2 (+) a b
-- > liftedAdd (Just 4) (Just 3)
-- (Just 7)

-- > liftedAdd (Just 4) Nothing
-- Nothing