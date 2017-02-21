module App.Shape where

import Prelude

import Halogen (Action)
import Halogen.HTML.Indexed as H

type Point = { x :: Int, y :: Int }
type Color = { r :: Int, g :: Int, b :: Int }

data Property = Enabled 
              | Bordered
              | Color
              | Position
              | Opacity
              | Angle
              | Size

class Shape a where
  renderCanvas :: a -> Graphics Unit
  renderHTML   :: forall p b. a -> (Drawable -> Action b) -> Array (H.HTML p (b Unit))
  overlap      :: a -> Point -> Boolean
  props        :: [ Property ]
  reconcile    :: a -> a -> Int -> a
  
  
  
input :: Property -> (Drawable -> Action b)