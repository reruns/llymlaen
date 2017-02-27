module App.Drawable where

import Prelude
import Data.Array ((!!), insertBy, findIndex, updateAt)
import Data.String (joinWith)
import Data.Int (toNumber, round, toStringAs, hexadecimal)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import App.Shape

import Halogen (Action)
import Halogen.HTML.Indexed as H

import Math(pi)

import Graphics.Canvas.Free

--this seems like it should work   

type Drawable = { drawn :: Graphics Unit
                , layer :: Int
                , updated :: Unit -> Drawable
                , setTime :: Int -> Drawable
                , addMoment :: Unit -> Drawable
                , formed :: forall p a. (Drawable -> Action a) -> Array (H.HTML p (a Unit))
                , overlap :: Point -> Boolean
                }

unfoldDrawable :: forall a. (Shape a) => Element a -> Drawable                       
unfoldDrawable el
  = { drawn: renderCanvas el.current
    , layer: el.layer
    , updated: \_ -> unfoldDrawable (advanceFrame el)
    , setTime: \t -> unfoldDrawable (setTime el t)
    , addMoment: \_ -> unfoldDrawable (insertKey el el.current)
    , formed: renderHTML el.current
    , overlap: overlap el.current
    }
             
advanceFrame :: forall a. (Shape a) => Element a -> Element a
advanceFrame el = setTime el (el.current.time + 1)

setTime :: forall a. (Shape a) => Element a -> Int -> Element a
setTime el t =
  let ms = findMoment el.keys t
      l' = fromMaybe (el.current) ms.l
      r' = fromMaybe (el.current) ms.r in
  case ((==) t) <$> _.time <$> ms.r of
    Nothing    -> el {current = l' {time=t}}
    Just false -> el {current = el.reconcile l' r' t}
    Just true  -> el {current = r'}    
    
findMoment :: forall a. (Shape a) => Array (Moment a) -> Int -> {l :: Maybe (Moment a), r :: Maybe (Moment a)}
findMoment keys t = go 0 where
  go x = case ((<) t) <$> _.time <$> (keys !! x) of
           Just true  -> {l: (keys !! (x-1)), r: (keys !! x)}
           Just false -> go (x+1)
           Nothing    -> {l: (keys !! (x-1)), r: Nothing}
            
insertKey :: forall a. (Shape a) => Element a -> Moment a -> Element a
insertKey el k = 
  case findIndex (\a -> a.time == k.time) el.keys of
    Just i  -> (el {keys = fromMaybe el.keys $ updateAt i k el.keys})
    Nothing -> (el {keys = insertBy (comparing _.time) k el.keys})
          
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