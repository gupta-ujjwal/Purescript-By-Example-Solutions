module Example.Shapes where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (closePath, lineTo, moveTo, fillPath,
                        setFillStyle, arc, rect, getContext2D,
                        getCanvasElementById, setStrokeStyle, strokePath)
import Math as Math
import Partial.Unsafe (unsafePartial)

translate
  :: forall r
   . Number
  -> Number
  -> { x :: Number, y :: Number | r }
  -> { x :: Number, y :: Number | r }
translate dx dy shape = shape
  { x = shape.x + dx
  , y = shape.y + dy
  }

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  _ <- setFillStyle ctx "#0000FF"

  _ <- fillPath ctx $ rect ctx $ translate (-200.0) (-200.0)
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }

  -- _ <- setFillStyle ctx "#00FF00"

  -- _ <- fillPath ctx $ arc ctx $ translate 200.0 200.0
  --   { x: 300.0
  --   , y: 300.0
  --   , radius: 50.0
  --   , start: Math.pi * 5.0 / 8.0
  --   , end: Math.pi * 2.6
  --   }


  _ <- setStrokeStyle ctx "#0000FF"

  strokePath ctx $ do
    moveTo ctx 400.0 400.0
    lineTo ctx 500.0 500.0
    lineTo ctx 400.0 500.0
    closePath ctx




  _ <- setFillStyle ctx "#FF0000"

  fillPath ctx $ do
    _ <- moveTo ctx 300.0 260.0
    _ <- lineTo ctx 260.0 340.0
    _ <- lineTo ctx 340.0 340.0
    closePath ctx
