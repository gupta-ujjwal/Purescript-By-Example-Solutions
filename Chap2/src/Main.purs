module Main where

import Prelude
import Math (pi)
import Effect (Effect)
import Effect.Console (logShow) 

areaCirc x p = p*x*x

main = do
    logShow (areaCirc 3.0 pi)
