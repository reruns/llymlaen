module App.Donut where

import App.Element

import Prelude
import Data.Int (toNumber, round)
import Math (pi, pow, sqrt)

import Graphics.Canvas.Free

import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Properties.Indexed as P


type DonutMoment = { enabled :: Boolean
                   , time :: Int
                   , size :: {r1::Int, r2::Int}
                   , pos :: {x ::Int, y::Int}
                   , opacity :: Int
                   , color :: { r :: Int, g :: Int, b :: Int }
                   }
        
showData :: forall p i. DonutMoment -> H.HTML p i           
showData moment = 
  H.form_ 
    [ H.input [ P.inputType P.InputCheckbox
              , P.title "enabled"
              , P.checked moment.enabled ]
    , H.input [ P.inputType P.InputNumber
              , P.title "Inner radius"
              , P.value $ show moment.size.r1 ]
    , H.input [ P.inputType P.InputNumber
              , P.title "Outer radius"
              , P.value $ show moment.size.r2 ]          
    , H.input [ P.inputType P.InputNumber
              , P.title "x"
              , P.value $ show moment.pos.x ]
    , H.input [ P.inputType P.InputNumber
              , P.title "y"
              , P.value $ show moment.pos.y ]
    ]
    
    
defaultDonutMoment :: DonutMoment
defaultDonutMoment = { enabled: false
                     , time: -1
                     , size: { r1:0, r2:0 }
                     , pos: {x: 0, y: 0}
                     , opacity: 0
                     , color: {r:0, g:0, b:0}
                     }  
                     
donutElem :: Element DonutMoment
donutElem = Element { layer: 0
                    , keys: []
                    , render: renderDonut
                    , reconcile: reconcileDonut
                    , current: defaultDonutMoment
                    , form: showData
                    , overlap: overlap
                    }  
                  
renderDonut {enabled: false} = pure unit                   
renderDonut c = at c.pos.x c.pos.y $ do
  setFillStyle $ colorToStr c.color
  setLineWidth (toNumber (c.size.r2 - c.size.r1))
  beginPath
  arc {x: 0.0, y:0.0, r: toNumber c.size.r1, start: 0.0, end: 2.0*pi}
  closePath
  stroke
  
reconcileDonut :: DonutMoment -> DonutMoment -> Int -> DonutMoment
reconcileDonut l r t = 
  let p = (toNumber (t-l.time)) / (toNumber (r.time-l.time))
      f :: Int -> Int -> Int
      f a b = a + (round $ p * (toNumber (b-a)))  in
  { enabled: l.enabled
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
  
overlap :: DonutMoment -> { x :: Int, y :: Int } -> Boolean
overlap m@{pos:{x:x1,y:y1}} {x:x2,y:y2} = 
  let d = sqrt $ (pow (toNumber x1 - toNumber x2) 2.0) + (pow (toNumber y1 - toNumber y2) 2.0)
  in d >= (toNumber m.size.r1) && d <= (toNumber m.size.r2)