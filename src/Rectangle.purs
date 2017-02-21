module App.Rectangle where

import App.Element
import App.Static
import App.Validators
import App.Element.Rendering

import Prelude
import Data.Int (toNumber, round)
import Math (pi)
import Data.Maybe (Maybe (Just,Nothing))

import Graphics.Canvas.Free

import Halogen (Action, action)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Properties.Indexed as P

import App.Shape

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
             
showData :: forall p a. Element RectMoment -> (Drawable -> Action a) -> Array (H.HTML p (a Unit))
showData elem@(Element el) qr = 
  let moment = el.current in
  [ H.h1_   [ H.text $ show moment.time ]
  , H.h2_   [ H.text $ show moment.opacity ]
  , H.input [ P.inputType P.InputCheckbox
            , P.title "enabled"
            , P.checked moment.enabled
            , E.onChecked $ E.input (\b -> qr $ unfoldDrawable $ Element $ el{current=moment{enabled=b}})
            ]
  , H.input [ P.inputType P.InputCheckbox
            , P.title "bordered"
            , P.checked moment.bordered 
            , E.onChecked $ E.input (\b -> qr $ unfoldDrawable $ Element $ el{current=moment{bordered=b}})
            ]
  , H.input [ P.inputType P.InputRange
            , P.IProp $ H.prop (H.propName "min") (Just $ H.attrName "min") 0
            , P.IProp $ H.prop (H.propName "max") (Just $ H.attrName "max") 360
            , P.title "angle" 
            , P.value $ show moment.angle
            , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateAngle s (Element el)))
            ]
  , H.input [ P.inputType P.InputNumber
            , P.title "width"
            , P.value $ show moment.size.w 
            , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateWidth s (Element el)))
            ]
  , H.input [ P.inputType P.InputNumber
            , P.title "height"
            , P.value $ show moment.size.h 
            , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateHeight s (Element el)))
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
  , H.input [ P.inputType P.InputRange
            , P.title "opacity"
            , P.value $ show moment.opacity
            , P.IProp $ H.prop (H.propName "min") (Just $ H.attrName "min") 0
            , P.IProp $ H.prop (H.propName "max") (Just $ H.attrName "max") 100
            , E.onValueChange (\s -> (map $ (action <<< qr)) <$> (validateOp s (Element el)))
            ]
  , redInput qr elem
  , blueInput qr elem
  , greenInput qr elem
  ]
    
showStat :: forall p i a. Static RectMoment -> (Drawable -> Action a) -> Array (H.HTML p i)
showStat (Static s) qr = []    

rectElem :: Element RectMoment
rectElem = Element { layer: 0
                   , keys: []
                   , render: renderRect
                   , reconcile: reconcileRect
                   , current: defaultRectMoment
                   , form: showData
                   , overlap: overlap
                   }
                   
rectBase = unfoldDrawable rectElem

staticRect moment = Static { moment: moment
                           , render: renderRect
                           , form: showStat
                           }                           
                
renderRect {enabled: false} = pure unit                
renderRect c = at c.pos.x c.pos.y $ do
  setAlpha ((toNumber c.opacity) / 100.0)
  setFillStyle $ colorToStr c.color
  setBorder c.bordered c.color
  rotate ((toNumber c.angle) * pi / 180.0)
  fillRect {x: 0.0, y:0.0, w: toNumber c.size.w, h: toNumber c.size.h}
  strokeRect {x: 0.0, y:0.0, w: toNumber c.size.w, h: toNumber c.size.h}
    
reconcileRect :: RectMoment -> RectMoment -> Int -> RectMoment
reconcileRect l r t = 
  let p = (toNumber (t-l.time)) / (toNumber (r.time-l.time))
      f :: Int -> Int -> Int
      f a b | b == a    = a
            | otherwise = a + (round $ p * (toNumber (b-a)))  in
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
  
overlap :: RectMoment -> { x :: Number, y :: Number } -> Boolean
overlap m {x,y} = 
  let d1 = x-(toNumber m.pos.x)
      d2 = y-(toNumber m.pos.y)
  in d1 >= 0.0 && d1 <= (toNumber m.size.w) && d2 >= 0.0 && d2 <= (toNumber m.size.h)