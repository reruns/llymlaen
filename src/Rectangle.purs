module App.Rectangle where

import App.Element
import App.Static

import Prelude
import Data.Int (toNumber, round)
import Math (pi)

import Graphics.Canvas.Free

type RectMoment = { enabled :: Boolean
                  , bordered :: Boolean
                  , time :: Int
                  , angle :: Int
                  , size :: {w::Int, h::Int}
                  , pos :: {x ::Int, y::Int}
                  , opacity :: Int
                  , color :: { r :: Int, g :: Int, b :: Int }
                  }
            
defaultRectMoment = { enabled: false
                    , bordered: false
                    , time: -1
                    , angle: 0
                    , size: { w:0,h:0 }
                    , pos: {x: 0, y: 0}
                    , opacity: 0
                    , color: {r:0, g:0, b:0}
                    }  

rectElem :: Element RectMoment
rectElem = Element { layer: 0
                   , keys: []
                   , render: renderRect
                   , reconcile: reconcileRect
                   , current: defaultRectMoment
                   }

staticRect moment = Static { moment: moment
                           , render: renderRect
                           }                   
                  
renderRect c = at c.pos.x c.pos.y $ do
  setFillStyle $ colorToStr c.color
  setBorder c.bordered
  rotate ((toNumber c.angle) * pi / 180.0)
  fillRect {x: 0.0, y:0.0, w: toNumber c.size.w, h: toNumber c.size.h}
  strokeRect {x: 0.0, y:0.0, w: toNumber c.size.w, h: toNumber c.size.h}
    
reconcileRect :: RectMoment -> RectMoment -> Int -> RectMoment
reconcileRect l r t = 
  let p = (toNumber (t-l.time)) / (toNumber (r.time-l.time))
      f :: Int -> Int -> Int
      f a b = a + (round $ p * (toNumber (b-a)))  in
  { enabled: l.enabled
  , bordered: l.bordered
  , time: t
  , angle: f l.angle r.angle
  , size: { w: f l.size.w r.size.w
          , h: f l.size.h r.size.h
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