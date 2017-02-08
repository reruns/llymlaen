module App.Element where

import Prelude
import Data.Array ((!!))
import Data.String (joinWith)
import Data.Int (toNumber, round, toStringAs, hexadecimal)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Math(pi)

import Graphics.Canvas.Free

--this seems like it should work
data Element a = Element { layer :: Int
                         , keys :: Array a
                         , current :: a
                         , reconcile :: a -> a -> Int -> a
                         , render :: a -> Graphics Unit
                         }
  
data Drawable = Drawable { drawn :: Graphics Unit
                         , updated :: Unit -> Drawable
                         }
                         
unfoldDrawable (Element el) 
  = Drawable { drawn: el.render el.current
             , updated: \_ -> unfoldDrawable (advanceFrame el)}
             
 
advanceFrame el = setTime el (el.current.time + 1)

setTime el t =
  let ms = findMoment el.keys t
      l' = fromMaybe (el.current) ms.l
      r' = fromMaybe (el.current) ms.r in
  case ((==) t) <$> time <$> ms.r of
    Nothing    -> Element el
    Just false -> Element $ el {current = el.reconcile l' r' t}
    Just true  -> Element $ el {current = r'}    

time x = x.time --apparently ".foo" isn't a function, so...

findMoment keys t = go 0 where
  go x = case ((>=) t) <$> time <$> (keys !! x) of
           Just true   -> {l: (keys !! (x-1)), r: (keys !! x)}
           Just false  -> go (x+1)
           Nothing     -> {l: (keys !! (x-1)), r: Nothing}
          
setKeys :: forall a. Element a -> Array a -> Element a
setKeys (Element el) ks = Element (el {keys=ks})
          
          
--some shared functions for graphics
at x y gfx = do
  save
  translate (toNumber x) (toNumber y)
  gfx
  restore
  

colorToStr {r,g,b} = "#" <> (joinWith "" $ map (\x -> (if x < 16 then "0" else "") <> (toStringAs hexadecimal x)) [r,g,b])

setBorder :: Boolean -> Graphics Unit
setBorder bordered = 
  if bordered
    then setStrokeStyle "#000000"
    else pure unit