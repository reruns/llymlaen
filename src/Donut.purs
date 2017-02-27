module App.Donut where

import App.Shape
import App.Element
import App.Static

import Prelude
import Data.Int (toNumber, round)
import Math (pi, pow, sqrt)
import Data.Maybe (Maybe(Just,Nothing))
  
reconcileDonut :: Moment Donut -> Moment Donut -> Int -> Moment Donut
reconcileDonut {time: tl, pl:l} {time: tr, pl:r} t = 
  let p = (toNumber (t-tl)) / (toNumber (tr-tl))
      f :: Int -> Int -> Int
      f a b = a + (round $ p * (toNumber (b-a)))  in
  { time: t, pl: { 
    enabled: l.enabled
    , time: t
    , size: { r1: f l.size.r1 r.size.r1
            , r2: f l.size.r2 r.size.r2
            }
    , pos: { x: f l.pos.x r.pos.x
           , y: f l.pos.y r.pos.y
           }
    , opacity: f l.opacity r.opacity
    , color: { r: f l.color.r r.color.r
             , g: f l.color.g r.color.g
             , b: f l.color.b r.color.b
             }
    }
  }
  
overlap :: Donut -> Point -> Boolean
