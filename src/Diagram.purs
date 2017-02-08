module App.Diagram where

import App.Element as E
import Data.Foldable
import Prelude (map, bind, ($), unit)
import Graphics.Canvas.Free (fillRect, setFillStyle)

type State = { paused :: Boolean
             , color :: { r :: Int, g :: Int, b :: Int }
             , statics :: Array E.Drawable
             , elements :: Array E.Drawable 
             }
                  
advanceFrame :: State -> State
advanceFrame st = st {elements = map updateOne st.elements } where
  updateOne (E.Drawable d) = d.updated unit


render st = 
  let drawOne (E.Drawable d) = d.drawn in
  do 
    setFillStyle $ E.colorToStr st.color
    fillRect {x: 0.0, y:0.0, w: 1000.0, h: 1000.0}
    traverse_ drawOne st.statics
    traverse_ drawOne st.elements