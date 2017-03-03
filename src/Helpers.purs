module App.Helpers where

import Data.Foreign (F, toForeign)
import DOM.Event.Types (MouseEvent)

foreign import pageX :: MouseEvent -> Int

foreign import pageY :: MouseEvent -> Int