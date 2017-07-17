module App.Helpers.Mouse where

import App.Prelude
import App.Types.Point

import Data.Foreign (F, toForeign)
import DOM.Event.Types (MouseEvent)

import DOM.HTML (window)
import DOM.HTML.Window (scrollX, scrollY)
import DOM.HTML.HTMLElement (getBoundingClientRect)

foreign import pageX :: MouseEvent -> Int

foreign import pageY :: MouseEvent -> Int

getOffset p Nothing = do
  pure p
    
getOffset (Point {x, y}) (Just el) = do
  w    <- window
  rect <- getBoundingClientRect el
  scX  <- scrollX w
  scY  <- scrollY w
  pure $ Point {x: x - (round rect.left) - scX , y: y - (round rect.top) - scY }