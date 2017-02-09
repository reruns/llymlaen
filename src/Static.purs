module App.Static where

import Graphics.Canvas.Free
import App.Element
import Prelude (Unit)

data Static a = Static { moment :: a
                       , render :: a -> Graphics Unit
                       }
                
boxStatic (Static s) = Drawable { drawn: s.render s.moment
                                , updated: \_ -> boxStatic (Static s)
                                , setTime: \t -> boxStatic (Static s)
                                , insertKey:  \_ -> boxStatic (Static s)
                                , layer: 0
                                }