module App.Static where

import Graphics.Canvas.Free
import App.Element
import Prelude (Unit)

import Halogen (Action)
import Halogen.HTML.Indexed as H

data Static a = Static { moment :: a
                       , render :: a -> Graphics Unit
                       , form :: forall p i b. Static a -> (Drawable -> Action b) -> H.HTML p i
                       }
                
boxStatic s@(Static {render,moment,form}) = 
  Drawable { drawn: render moment
           , updated: \_ -> boxStatic s
           , setTime: \t -> boxStatic s
           , insertKey:  \_ -> boxStatic s
           , layer: 0
           , formed: form s
           , overlap: \_ -> false
           }