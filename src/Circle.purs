module App.Circle where

import App.Shape
import App.Element
import App.Static


import Prelude
import Data.Int (toNumber, round)
import Math (pi, pow, sqrt)
import Data.Maybe (Maybe(Just,Nothing))

reconcileCircle :: Moment Circle -> Moment Circle -> Int -> Moment Circle
reconcileCircle {time: tl, pl:l} {time: tr, pl:r} t = 
  let p = (toNumber (t-tl)) / (toNumber (tr-tl))
      f :: Int -> Int -> Int
      f a b = a + (round $ p * (toNumber (b-a)))  in
  { time: t, pl: { 
    enabled: l.enabled
    , bordered: l.bordered
    , time: t
      , radius: f l.radius r.radius
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