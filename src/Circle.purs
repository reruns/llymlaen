module App.Circle where

import App.Element

import Prelude
import Data.Int (toNumber, round)
import Math (pi)

import Graphics.Canvas.Free

import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Properties.Indexed as P

type CircleMoment = { enabled :: Boolean
                    , bordered :: Boolean
                    , time :: Int
                    , radius :: Int
                    , pos :: {x ::Int, y::Int}
                    , opacity :: Int
                    , color :: { r :: Int, g :: Int, b :: Int }
                    }

showData :: forall p i. CircleMoment -> H.HTML p i
showData moment = 
  H.form_ 
    [ H.input [ P.inputType P.InputCheckbox
              , P.title "enabled"
              , P.checked moment.enabled ]
    , H.input [ P.inputType P.InputCheckbox
              , P.title "bordered"
              , P.checked moment.bordered ]
    , H.input [ P.inputType P.InputNumber
              , P.title "radius"
              , P.value $ show moment.radius ]
    , H.input [ P.inputType P.InputNumber
              , P.title "x"
              , P.value $ show moment.pos.x ]
    , H.input [ P.inputType P.InputNumber
              , P.title "y"
              , P.value $ show moment.pos.y ]
    ]

defaultCircleMoment = { enabled: false
                      , bordered: false
                      , time: -1
                      , radius: 0
                      , pos: {x: 0, y: 0}
                      , opacity: 0
                      , color: {r:0,g:0,b:0}
                      }
                      
circleElem :: Element CircleMoment
circleElem = Element { layer: 0
                     , keys: []
                     , render: renderCircle
                     , reconcile: reconcileCircle
                     , current: defaultCircleMoment
                     , form: showData
                     }
                  
renderCircle c = at c.pos.x c.pos.y $ do
  setFillStyle $ colorToStr c.color
  setBorder c.bordered
  setAlpha ((toNumber c.opacity) / 100.0)
  beginPath
  arc {x: 0.0,y: 0.0,r: toNumber c.radius, start: 0.0, end: 2.0*pi}
  closePath
  stroke
  fill

reconcileCircle :: CircleMoment -> CircleMoment -> Int -> CircleMoment
reconcileCircle l r t = 
  let p = (toNumber (t-l.time)) / (toNumber (r.time-l.time))
      f :: Int -> Int -> Int
      f a b = a + (round $ p * (toNumber (b-a))) in
  { enabled: l.enabled
  , bordered: l.bordered
  , time: t
  , radius: f l.radius r.radius
  , pos: { x: f l.pos.x r.pos.x
         , y: f l.pos.y r.pos.y
         }
  , opacity: f l.opacity r.opacity
  , color: { r: f l.color.r r.color.r
           , g: f l.color.g r.color.g
           , b: f l.color.g r.color.b
           }         
  }