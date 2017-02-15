module App.Element where

import Prelude
import Data.Array ((!!), insertBy, findIndex, updateAt)
import Data.String (joinWith)
import Data.Int (toNumber, round, toStringAs, hexadecimal)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Halogen (Action)
import Halogen.HTML.Indexed as H

import Math(pi)

import Graphics.Canvas.Free

--this seems like it should work
data Element a = Element { layer :: Int
                         , keys :: Array a
                         , current :: a
                         , reconcile :: a -> a -> Int -> a
                         , render :: a -> Graphics Unit
                         , form :: forall p b. Element a -> (Drawable -> Action b) -> H.HTML p (b Unit)
                         , overlap :: a -> {x :: Number, y :: Number} -> Boolean
                         }
  
data Drawable = Drawable { drawn :: Graphics Unit
                         , layer :: Int
                         , updated :: Unit -> Drawable
                         , setTime :: Int -> Drawable
                         , addMoment :: Unit -> Drawable
                         , formed :: forall p a. (Drawable -> Action a) -> H.HTML p (a Unit)
                         , overlap :: {x :: Number, y :: Number} -> Boolean
                         }
                         
unfoldDrawable (Element el) 
  = Drawable { drawn: el.render el.current
             , layer: el.layer
             , updated: \_ -> unfoldDrawable (advanceFrame el)
             , setTime: \t -> unfoldDrawable (setTime el t)
             , addMoment: \_ -> unfoldDrawable (insertKey el el.current)
             , formed: el.form (Element el)
             , overlap: el.overlap el.current
             }
             
advanceFrame el = setTime el (el.current.time + 1)

setTime el t =
  let ms = findMoment el.keys t
      l' = fromMaybe (el.current) ms.l
      r' = fromMaybe (el.current) ms.r in
  case ((==) t) <$> _.time <$> ms.r of
    Nothing    -> Element $ el {current = l' {time=t}}
    Just false -> Element $ el {current = el.reconcile l' r' t}
    Just true  -> Element $ el {current = r'}    
    
findMoment keys t = go 0 where
  go x = case ((<) t) <$> _.time <$> (keys !! x) of
           Just true  -> {l: (keys !! (x-1)), r: (keys !! x)}
           Just false -> go (x+1)
           Nothing    -> {l: (keys !! (x-1)), r: Nothing}
          
setKeys :: forall a. Element a -> Array a -> Element a
setKeys (Element el) ks = Element (el {keys=ks})
          
insertKey el k = 
  case findIndex (\a -> a.time == k.time) el.keys of
    Just i  -> Element (el {keys = fromMaybe el.keys $ updateAt i k el.keys})
    Nothing -> Element (el {keys = insertBy (comparing _.time) k el.keys})
          
--some shared functions for graphics
at x y gfx = do
  save
  translate (toNumber x) (toNumber y)
  gfx
  restore
  

colorToStr {r,g,b} = "#" <> (joinWith "" $ map (\x -> (if x < 16 then "0" else "") <> (toStringAs hexadecimal x)) [r,g,b])

setBorder :: Boolean -> {r::Int, g::Int, b::Int} -> Graphics Unit
setBorder bordered color = 
  if bordered
    then setStrokeStyle "#000000"
    else setStrokeStyle $ colorToStr color