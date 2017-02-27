module App.Element where

import Prelude

import App.Validators

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Array (insertBy, (!!), updateAt, findIndex, concat, zipWith, mapWithIndex)
import Data.Traversable (sequence, sequence_)
import Data.Foldable (foldl)
import Data.Int (toNumber, round, toStringAs, hexadecimal)
import Data.String (joinWith)
import Math (pi, sqrt, pow)

import Graphics.Canvas.Free

import Halogen (Action, action)
import Halogen.HTML.Indexed as H
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Events.Handler as EH
import Halogen.HTML.Properties.Indexed as P
import Halogen.HTML.Events.Handler (preventDefault)


--might need to split this into multiple files at some point
type Point = { x :: Int, y :: Int }
setX p v = p {x=v}
setY p v = p {y=v}

type RGB   = { r :: Int, g :: Int, b :: Int }
setR c v = c {r=v}
setG c v = c {g=v}
setB c v = c {b=v}

--a perhaps more practical way of abstracting this would be how Halogen HTML does components
--but I don't quite get how to fit together row-types to do that
--also, we could perhaps make keys be a list of diffs, rather than whole states?
data Property = Enabled  Boolean
              | Bordered Boolean
              | Color    RGB
              | Position Point
              | Opacity  Int
              | Angle    Int
              | Circle   Int
              | Rect     Int Int
              | Donut    Int Int

type Moment = { time :: Int, props :: Array Property }
type Element = { current :: Moment, layer :: Int, keys :: Array Moment } 

circBase :: Element
circBase = { layer: 0
           , keys: []
           , current: { time: 0
                      , props: [ Enabled false
                               , Bordered false
                               , Color {r:128,g:128,b:128}
                               , Position {x:0,y:0}
                               , Opacity 100
                               , Circle 0
                               ]
                      }
           }

rectBase :: Element
rectBase = { layer: 0
           , keys: []
           , current: { time: 0
                      , props: [ Enabled false
                               , Bordered false
                               , Color {r:128,g:128,b:128}
                               , Position {x:0,y:0}
                               , Opacity 100
                               , Angle 0
                               , Rect 0 0
                               ]
                      }
           }
           
donutBase :: Element
donutBase = { layer: 0
            , keys: []
            , current: { time: 0
                       , props: [ Enabled false
                                , Color {r:128,g:128,b:128}
                                , Position {x:0,y:0}
                                , Opacity 100
                                , Donut 0 1
                                ]
                       }
            }
            
renderHTML :: forall p b. Element -> (Element -> Action b) -> Array (H.HTML p (b Unit))
renderHTML el@{current:{time:t,props:props}} qr = concat $ mapWithIndex renderProp props where
  insertVal  i prop = el {current = el.current {props = fromMaybe props (updateAt i prop props)}}
  renderProp i (Enabled b)   = [checkBox [P.title "enabled"]  b (\v -> qr $ insertVal i (Enabled v))]
  renderProp i (Bordered b)  = [checkBox [P.title "bordered"] b (\v -> qr $ insertVal i (Bordered v))]
  renderProp i (Color c)     = map (\{v,h} -> slider [] 0 255 v (qr <<< (insertVal i) <<< Color <<< h)) [{v:c.r,h:setR c}, {v:c.g,h:setG c}, {v:c.b,h:setB c}]
  renderProp i (Position p)  = map (\{v,h} -> number [] v (qr <<< (insertVal i) <<< Position <<< h)) [{v:p.x,h:setX p}, {v: p.y, h: setY p}]
  renderProp i (Opacity o)   = [slider [P.title "opacity"] 0 100 o (\v -> qr $ insertVal i (Opacity v))]
  renderProp i (Angle a)     = [slider [P.title "angle"] 0 360 a (\v -> qr $ insertVal i (Angle v))]
  renderProp i (Circle r)    = [] --TODO: These
  renderProp i (Rect w h)    = []
  renderProp i (Donut r1 r2) = []
              
              
renderEl :: Element -> Graphics Unit
renderEl el = renderCanvas el.current.props

renderCanvas :: Array Property -> Graphics Unit
renderCanvas props = do save 
                        let gfx = sequence $ map renderProp props
                        case gfx of
                          Nothing -> pure unit
                          Just es -> sequence_ es
                        restore where
  renderProp (Enabled b)      = if b then Just (pure unit) else Nothing
  renderProp (Bordered b)     = if b then Just (setStrokeStyle "#000000") else Just (pure unit)
  renderProp (Color c)        = Just $ (setFillStyle (colorToStr c)) *> (setStrokeStyle (colorToStr c))
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
  
reconcile :: Moment -> Moment -> Int -> Moment
reconcile {time:tl, props: left} {time: tr, props: right} t =
  let p = (toNumber (t-tl)) / (toNumber (tr-tl))
      f a b = a + (round $ p * (toNumber (b-a)))  
      recProp (Enabled b1)  (Enabled b2)    = Just $ Enabled b1
      recProp (Bordered b1) (Bordered b2)   = Just $ Bordered b1
      recProp (Color c1)    (Color c2)      = Just $ Color {r: f c1.r c2.r, g: f c1.g c2.g, b: f c1.b c2.b}
      recProp (Position p1) (Position p2)   = Just $ Position {x: f p1.x p2.x, y: f p1.y p2.y}
      recProp (Angle a1)    (Angle a2)      = Just $ Angle (f a1 a2)
      recProp (Opacity o1)  (Opacity o2)    = Just $ Opacity (f o1 o2)
      recProp (Circle r1)   (Circle r2)     = Just $ Circle (f r1 r2)
      recProp (Rect w1 h1)  (Rect w2 h2)    = Just $ Rect (f w1 w2) (f h1 h2)
      recProp (Donut r1 r2) (Donut r1' r2') = Just $ Donut (f r1 r1') (f r2 r2')
      recProp _             _               = Nothing --Mismatch! Quietly abort.
  in {time: t, props: fromMaybe left $ sequence $ zipWith recProp left right}
  
--note: this will compile but not work correctly if Position comes after the Shape property
overlap :: Element -> Point -> Boolean
overlap {current:{props}} p = _.b $ foldl f {d:p,b:false} props where
  f {d,b} (Position {x,y}) = {d:{x: d.x - x , y: d.y - y}, b: b}
  f {d,b} (Circle r)       = {d:d, b: b || ((sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)) <= (toNumber r))}
  f {d,b} (Rect w h)       = {d:d, b: b || (d.x >= 0 && d.x <= w && d.y >= 0 && d.y <= h)}
  f {d,b} (Donut r1 r2)    = let dist = sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)
                             in {d:d, b: b || (dist >= (toNumber r1) && dist <= (toNumber r2))}
  f res _                  = res

insertKey :: Element -> Moment -> Element
insertKey el k = 
  case findIndex (\a -> a.time == k.time) el.keys of
    Just i  -> (el {keys = fromMaybe el.keys $ updateAt i k el.keys})
    Nothing -> (el {keys = insertBy (comparing _.time) k el.keys})

    
advanceFrame :: Element -> Element
advanceFrame el = setTime el (el.current.time + 1)

setTime :: Element -> Int -> Element
setTime el t =
  let ms = findMoment el.keys t
      l' = fromMaybe (el.current) ms.l
      r' = fromMaybe (el.current) ms.r in
  case ((==) t) <$> _.time <$> ms.r of
    Nothing    -> el {current = l' {time=t}}
    Just false -> el {current = reconcile l' r' t}
    Just true  -> el {current = r'}    
    

findMoment :: Array Moment -> Int -> { l :: Maybe Moment, r :: Maybe Moment }
findMoment keys t = go 0 where
  go x = case ((<) t) <$> _.time <$> (keys !! x) of
           Just true  -> {l: (keys !! (x-1)), r: (keys !! x)}
           Just false -> go (x+1)
           Nothing    -> {l: (keys !! (x-1)), r: Nothing}  
           
--functions for Form elements
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

          
colorToStr {r,g,b} = "#" <> (joinWith "" $ map (\x -> (if x < 16 then "0" else "") <> (toStringAs hexadecimal x)) [r,g,b])