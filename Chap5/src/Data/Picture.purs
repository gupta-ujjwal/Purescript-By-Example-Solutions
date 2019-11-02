module Data.Picture where

import Prelude

import Data.Foldable (foldl)
import Global as Global
import Math as Math
import Data.Eq
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.String as String
import Data.Monoid (class Monoid, mempty)


data Point = Point
  { x :: Number
  , y :: Number
  }

showPoint :: Point -> String
showPoint (Point { x, y }) =
  "(" <> show x <> ", " <> show y <> ")"

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

showShape :: Shape -> String
showShape (Circle c r) =
  "Circle [center: " <> showPoint c <> ", radius: " <> show r <> "]"
showShape (Rectangle c w h) =
  "Rectangle [center: " <> showPoint c <> ", width: " <> show w <> ", height: " <> show h <> "]"
showShape (Line start end) =
  "Line [start: " <> showPoint start <> ", end: " <> showPoint end <> "]"
showShape (Text loc text) =
  "Text [location: " <> showPoint loc <> ", text: " <> show text <> "]"


type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape 

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number
  }


-- bounds :: Picture -> Bounds
-- bounds = foldl combine emptyBounds
--   where
--     combine :: Bounds -> Shape -> Bounds
--     combine b shape = union (shapeBounds shape) b


-- data NonEmpty a = NonEmpty a (Array a)

-- instance eqNonEMpty :: (Eq a, Eq (Array a)) => Eq NonEmpty where 
--   eq (NonEmpty a) (NonEmpty b) = 
--     if 

