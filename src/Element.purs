module App.Element where

import Prelude
import Data.Array ((!!))
import Data.Int (toNumber, round)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)

import Math(pi)

import Graphics.Canvas.Free


data Shape = Circle | Rectangle

type Element = {
                layer :: Int,
                shape :: Shape,
                keys  :: Array Instance,
                current :: Instance,
                pindex :: Int
               }

type Instance = {
                  enabled :: Boolean,
                  time :: Int,
                  angle :: Int,
                  size :: {w::Int, h::Int},
                  pos :: {x ::Int, y::Int},
                  opacity :: Int,
                  color :: { r :: Int, g :: Int, b :: Int }
                }
time x = x.time --this is dumb, but handy

advanceFrame :: Element -> Element    
advanceFrame el =
  let t = el.current.time+1
      r = el.keys !! (el.pindex + 1) 
      r' = fromMaybe (el.current) r in
  case ((>=) t) <$> time <$> r of
    Nothing -> el {current = el.current {time = t}}
    Just false -> el {current = reconcile el.current r'}
    Just true -> el {pindex = el.pindex+1 ,current = r'}

reconcile :: Instance -> Instance -> Instance
reconcile c r = 
  let p = 1.0 / (toNumber (r.time-c.time))
      f :: Int -> Int -> Int
      f a b = round $ (toNumber a) + p* (toNumber (b-a)) in
  {
    enabled: c.enabled,
    time: c.time+1,
    angle: f c.angle r.angle,
    size: {w: f c.size.w r.size.w, h: f c.size.h r.size.h},
    pos: {x: f c.pos.x r.pos.x, y: f c.pos.y r.pos.y},
    opacity: f c.opacity r.opacity,
    color: {r: f c.color.r r.color.r, g: f c.color.g r.color.g, b: f c.color.g r.color.b}
  }

--setTime = undefined
render {current: {enabled: false}} = pure unit

render {shape: Circle, current: c} =
  at c.pos.x c.pos.y do
    beginPath
    setAlpha ((toNumber c.opacity) / 100.0)
    arc {x: 0.0,y: 0.0,r: toNumber c.size.w, start: 0.0, end: 2.0*pi}
    closePath
    stroke
    
render {shape: Rectangle, current: c} =
  at c.pos.x c.pos.y do
    strokeRect {x: 0.0, y:0.0, w: toNumber c.size.w, h: toNumber c.size.h}
  
at x y gfx = do
  save
  translate (toNumber x) (toNumber y)
  gfx
  restore