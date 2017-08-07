module App.Helpers.Mouse where

import App.Prelude
import App.Types.Point

import Data.Foreign (F, toForeign)
import DOM.Event.Types (MouseEvent)
import DOM.HTML.Event.Types (DragEvent)

import DOM.HTML (window)
import DOM.HTML.Window (scrollX, scrollY)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import DOM.HTML.HTMLCanvasElement (width, height)

foreign import pageX :: MouseEvent -> Int
foreign import pageY :: MouseEvent -> Int

getOffset p Nothing = do
  pure p
    
getOffset (Point {x, y}) (Just el) = do
  w    <- window
  rect <- getBoundingClientRect el
  scX  <- scrollX w
  scY  <- scrollY w
  pure $ Point 
    { x: floor $ 600.0 * (toNumber $ x - (floor rect.left) - scX) / (rect.right-rect.left)
    , y: floor $ 600.0 * (toNumber $ y - (floor rect.top) - scY) / (rect.bottom-rect.top)
    }