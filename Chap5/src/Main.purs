module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

fact :: Int -> Int
fact 0 = 1
fact 1 = 1
fact n = n*(fact (n-1))

type Person = {
  name :: String,
  age :: Int,
  city :: String
}



-- Excercise 2

sameCity :: Person -> Person -> Boolean 
sameCity p1 p2 | (p1.city == p2.city) = true
               | otherwise = false

-- Excercise 3
data Shape = Circle Point Number
           | Line Point Point
           | Rectangle Point Number Number
           | Text Point String

data Point = Point {x::Number, y:: Number}


exampleLine :: Shape
exampleLine = Line p1 p2
  where
    p1 :: Point
    p1 = Point { x: 0.0, y: 0.0 }

    p2 :: Point
    p2 = Point { x: 100.0, y: 50.0 }


exampleCircle :: Shape
exampleCircle = Circle p 10.0
  where 
    p :: Point
    p = Point { x:0.0, y:0.0}   

