module App.Static where

import Graphics.Canvas.Free
import App.Element (renderCanvas, Property)
import Prelude (Unit)

type Static = { props :: Array Property }

renderStatic :: Static -> Graphics Unit
renderStatic st = renderCanvas st.props