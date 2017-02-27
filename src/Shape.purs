module App.Shape where

import Prelude

import Halogen (Action, action)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Properties.Indexed as P

type Point = { x :: Int, y :: Int }
setX p v = p {x=v}
setY p v = p {y=v}

type RGB   = { r :: Int, g :: Int, b :: Int }
setR c v = c {r=v}
setG c v = c {g=v}
setB c v = c {b=v}

--a perhaps more practical way of abstracting this would be how Halogen HTML does it
--but I don't quite get how to fit together row-types to do that
data Property = Enabled  Boolean
              | Bordered Boolean
              | Color    RGB
              | Position Point
              | Opacity  Int
              | Angle    Int
              | Circle   Int
              | Rect     Int Int
              | Donut    Int Int


renderHTML :: forall p b. Element -> (Element -> Action b) -> Array (H.HTML p (b Unit))
renderHTML el@{current:{time:t,props:props}} qr = concat $ mapWithIndex renderProp props where
  insertVal  i prop = el {current = current {props = fromMaybe props (updateAt i prop props)}}
  renderProp i (Enabled b)   = [checkBox [P.title "enabled"]  b (\v -> qr $ insertVal i (Enabled v))]
  renderProp i (Bordered b)  = [checkBox [P.title "bordered"] b (\v -> qr $ insertVal i (Bordered v)]
  renderProp i (Color c)     = map (\{v,h} -> slider [] 0 255 v (qr <<< (insertVal i) <<< Color <<< h)) [{v:c.r,h:setR c}, {v:c.g,h:setG c}, {v:c.b,h:setB c}]
  renderProp i (Position p)  = map (\{v,h} -> number [] v (qr <<< (insertVal i) <<< Position <<< h)) [{v:p.x,h:setX p}, {v: p.y, h: setY p}]
  renderProp i (Opacity o)   = [slider [P.title "opacity"] 0 100 o (\v -> qr $ insertVal i (Opacity v))]
  renderProp i (Angle a)     = [slider [P.title "angle"] 0 360 a (\v -> qr $ insertVal i (Angle v))]
  renderProp i (Circle r)    = []
  renderProp i (Rect w h)    = []
  renderProp i (Donut r1 r1) = []
              
              
renderCanvas :: Element -> Graphics Unit
renderCanvas {current:{props}} = save >> (sequence <$> sequence $ map renderProp props) >> restore where
  renderProp (Enabled b)      = if b then Just (pure unit) else Nothing
  renderProp (Bordered b)     = if b then Just (setStrokeStyle "#000000") else Just (pure unit)
  renderProp (Color c)        = Just $ (setFillStyle (colorToStr c)) >> (setStrokeStyle (colorToStr c))
  renderProp (Position {x,y}) = Just $ translate (toNumber x) (toNumber y)
  renderProp (Angle a)        = Just $ rotate ((toNumber a) * pi / 180.0)
  renderProp (Opacity o)      = Just $ setAlpha ((toNumber o) / 100.0)
  renderProp (Circle r)       = Just $ do
    beginPath
    arc {x: 0.0,y: 0.0,r: toNumber r, start: 0.0, end: 2.0*pi}
    closePath
    stroke
    fill
  renderProp (Rect w h)       = Just $ do
    fillRect {x: 0.0, y:0.0, w: toNumber w, h: toNumber h}
    strokeRect {x: 0.0, y:0.0, w: toNumber w, h: toNumber h}
  renderProp (Donut r1 r2)    = Just $ do
    setLineWidth (toNumber (r2 - r1))
    beginPath
    arc {x: 0.0, y:0.0, r: toNumber r1, start: 0.0, end: 2.0*pi}
    closePath
    stroke
  
--note: this will compile but not work correctly if Position comes after the Shape property
overlap :: Element -> Point -> Boolean
overlap {current:{props}} p = _.b $ foldl f {d:p,b:false} props where
  f {d,b} (Position {x,y}) = {d:{x: d.x - x , y: d.y - y}, b: b}
  f {d,b} (Circle r)       = {d:d, b: b || (sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0) <= (toNumber r))}
  f {d,b} (Rect w h)       = {d:d, b: b || (d.x >= 0.0 && d.x <= (toNumber w) && d.y >= 0.0 && d.y <= (toNumber h))}
  f {d,b} (Donut r1 r2)    = let dist = sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)
                             in {d:d, b: b || (dist >= (toNumber r1) && dist <= (toNumber r2))}
  f res _                  = res
  
type Moment = { time :: Int, props :: Array Property }
type Element = { current :: Moment, layer :: Int, keys :: Array Moment } 

checkBox props b h = 
  H.input ([P.inputType P.InputCheckbox
          , P.checked b
          , E.onChecked $ E.input h
          ] <> props)
          
slider props min max v h =
  H.input ([P.inputType P.InputRange
          , P.value (show v)
          , P.IProp $ H.prop (H.propName "min") (Just $ H.attrName "min") min
          , P.IProp $ H.prop (H.propName "max") (Just $ H.attrName "max") max
          , E.onValueChange (\s -> preventDefault $> ((action <<< h) <$> (validateRange s min max)))
          ] <> props) 
          
number props v h =
  H.input ([ P.inputType P.InputNumber
          , P.value (show v)
          , E.onValueChange (\s -> preventDefault $> ((action <<< h) <$> (validateNonNeg s)))
          ] <> props)
