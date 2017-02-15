module App.Circle where

import App.Element
import App.Static
import App.Validators

import Prelude
import Data.Int (toNumber, round)
import Math (pi, pow, sqrt)

import Graphics.Canvas.Free

import Halogen (Action, action)
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

showData :: forall p a. Element CircleMoment -> (Drawable -> Action a) -> H.HTML p (a Unit)
showData (Element el) qr = 
  let moment = el.current in
  H.form_ 
    [ H.input [ P.inputType P.InputCheckbox
              , P.title "enabled"
              , P.checked moment.enabled
              , E.onChecked $ E.input (\b -> qr $ unfoldDrawable $ Element $ el{current=moment{enabled=b}})]
    , H.input [ P.inputType P.InputCheckbox
              , P.title "bordered"
              , P.checked moment.bordered 
              , E.onChecked $ E.input (\b -> qr $ unfoldDrawable $ Element $ el{current=moment{bordered=b}})
              ]
    , H.input [ P.inputType P.InputNumber
              , P.title "radius"
              , P.value $ show moment.radius 
              , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateRadius s (Element el)))
              ]
    , H.input [ P.inputType P.InputNumber
              , P.title "x"
              , P.value $ show moment.pos.x 
              , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateX s (Element el)))
              ]
    , H.input [ P.inputType P.InputNumber
              , P.title "y"
              , P.value $ show moment.pos.y 
              , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateY s (Element el)))
              ]
    ]
    
showStat :: forall p i a. Static CircleMoment ->  (Drawable -> Action a) -> H.HTML p i
showStat (Static s) qr = H.div_ []    

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
                     , overlap: overlap
                     }
              
renderCircle {enabled: false} = pure unit               
renderCircle c = at c.pos.x c.pos.y $ do
  setFillStyle $ colorToStr c.color
  setBorder c.bordered c.color
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
  
overlap :: CircleMoment -> { x :: Number, y :: Number } -> Boolean
overlap m@{pos:{x:x1,y:y1}} {x:x2,y:y2} = 
  let d = sqrt $ (pow (toNumber x1 - x2) 2.0) + (pow (toNumber y1 - y2) 2.0)
  in d <= (toNumber m.radius)