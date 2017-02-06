module App.Viewer where

import App.Element as E
import Data.Foldable
import Prelude (map, bind)
import Graphics.Canvas.Free (clearRect)

type State = { paused :: Boolean, elements :: Array E.Element }

advanceFrame :: State -> State
advanceFrame st = st {elements = map E.advanceFrame st.elements}


render st = do
  clearRect {x: 0.0, y:0.0, w: 1000.0, h: 1000.0}
  traverse_ E.render st.elements