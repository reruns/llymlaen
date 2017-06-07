module App.Property where

import Prelude

import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Traversable (sequence, sequence_)
import Data.Int (toNumber, toStringAs, hexadecimal)
import Data.String (joinWith)
import Data.Array ((!!))
import Data.NonEmpty (NonEmpty(..))
import Data.Either (Either(..))

import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, jsonEmptyObject, (~>), (:=), (.?), decodeJson)
import Data.StrMap as SM

import Math (pi, sqrt, pow, sin, cos)

import Graphics.Canvas.Free

import Halogen (Action, action)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen

type Point = { x :: Int, y :: Int }
setX p v = p {x=v}
setY p v = p {y=v}

encodePoint {x,y}
  =  "x" := encodeJson x
  ~> "y" := encodeJson y
  ~> jsonEmptyObject

showPoint :: Point -> String
showPoint {x,y} = "(" <> (show x) <> "," <> (show y) <> ")"

type RGB   = { r :: Int, g :: Int, b :: Int }
setR c v = c {r=v}
setG c v = c {g=v}
setB c v = c {b=v}

encodeRGB {r,g,b}
  =  "r" := encodeJson r
  ~> "g" := encodeJson g
  ~> "b" := encodeJson b
  ~> jsonEmptyObject

colorToStr :: RGB -> String
colorToStr {r,g,b} = 
  "#" <> 
  (joinWith "" $ map (\x -> (if x < 16 then "0" else "") 
  <> (toStringAs hexadecimal x)) [r,g,b])

data Property = Enabled  Boolean
              | Bordered Boolean
              | Color    RGB
              | Position Point
              | Opacity  Int
              | Angle    Int
              | Circle   Int
              | Rect     Int Int
              | Donut    Int Int

derive instance eqProp :: Eq Property

instance encodeProp :: EncodeJson Property where
  encodeJson  (Enabled b)      = "Enabled"  := b ~> jsonEmptyObject
  encodeJson  (Bordered b)     = "Bordered" := b ~> jsonEmptyObject
  encodeJson  (Color c)        = "Color" := (encodeRGB c) ~> jsonEmptyObject
  encodeJson  (Position p)     = "Position" := (encodePoint p) ~> jsonEmptyObject
  encodeJson  (Angle a)        = "Angle" := a ~> jsonEmptyObject
  encodeJson  (Opacity o)      = "Opacity" := o ~> jsonEmptyObject
  encodeJson  (Circle r)       = "Circle" := r ~> jsonEmptyObject
  encodeJson  (Rect w h)       = "Rect" := ("w" := encodeJson w ~> "h" := encodeJson h ~> jsonEmptyObject) ~> jsonEmptyObject
  encodeJson  (Donut r1 r2)    = "Donut" := ("r1" := encodeJson r1 ~> "r2" := encodeJson r2 ~> jsonEmptyObject) ~> jsonEmptyObject
  
instance decodeProp :: DecodeJson Property where
    decodeJson json = do
      obj <- decodeJson json
      if SM.size obj /= 1
        then Left "Wrong size. Not a prop."
        else case fromMaybe "" ((SM.keys obj) !! 0) of
          "Enabled" -> Enabled <$> obj .? "Enabled"
          "Bordered"-> Bordered <$> obj .? "Bordered"
          "Color"   -> do
                        rgb <- obj .? "Color"
                        r <- rgb .? "r"
                        g <- rgb .? "g"
                        b <- rgb .? "b"
                        pure $ Color {r,g,b}
          "Position"-> do
                        xy <- obj .? "Position"
                        x  <- xy .? "x"
                        y  <- xy .? "y"
                        pure $ Position {x,y}
          "Angle"   -> Angle <$> obj .? "Angle"
          "Opacity" -> Opacity <$> obj .? "Opacity"
          "Circle"  -> Circle <$> obj .? "Circle"
          "Rect"    -> do
                        wh <- obj .? "Rect"
                        w  <- wh .? "w"
                        h  <- wh .? "h"
                        pure $ Rect w h
          "Donut"   -> do
                        rr <- obj .? "Donut"
                        r1 <- rr .? "r1"
                        r2 <- rr .? "r2"
                        pure $ Donut r1 r2
          _         -> Left "Unrecognized key"
      
      
propGens :: Array (Gen Property)
propGens = [ Enabled <$> arbitrary
           , Bordered <$> arbitrary
           , Color <$> ((\r g b -> {r,g,b}) <$> arbitrary <*> arbitrary <*> arbitrary)
           , Position <$> ((\x y -> {x,y}) <$> arbitrary <*> arbitrary)
           , Angle <$> arbitrary
           , Circle <$> arbitrary
           , Rect <$> arbitrary <*> arbitrary
           , Donut <$> arbitrary <*> arbitrary
           ]

instance arbProp :: Arbitrary Property where
  arbitrary = oneOf $ NonEmpty (pure $ Enabled false) propGens
  
instance showProp :: Show Property where
  show  (Enabled b)      = "Enabled: " <> (show b)
  show  (Bordered b)     = "Bordered: " <> (show b)
  show  (Color c)        = "Color: " <> colorToStr c
  show  (Position {x,y}) = "Position: (" <> (show x) <> "," <> (show y) <> ")"
  show  (Angle a)        = "Angle: " <> (show a)
  show  (Opacity o)      = "Opacity: " <> (show o)
  show  (Circle r)       = "Circle: " <> (show r)
  show  (Rect w h)       = "Rectangle: " <> (show w) <> "x" <> (show h)
  show  (Donut r1 r2)    = "Donut: " <> (show r1) <> "-" <> (show r2)

boolProp :: Property -> Boolean
boolProp (Enabled _)  = true
boolProp (Bordered _) = true
boolProp _            = false
           
recProp f (Enabled b1)  (Enabled b2)    = Just $ Enabled b1
recProp f (Bordered b1) (Bordered b2)   = Just $ Bordered b1
recProp f (Color c1)    (Color c2)      = Just $ Color {r: f c1.r c2.r, g: f c1.g c2.g, b: f c1.b c2.b}
recProp f (Position p1) (Position p2)   = Just $ Position {x: f p1.x p2.x, y: f p1.y p2.y}
recProp f (Angle a1)    (Angle a2)      = Just $ Angle (f a1 a2)
recProp f (Opacity o1)  (Opacity o2)    = Just $ Opacity (f o1 o2)
recProp f (Circle r1)   (Circle r2)     = Just $ Circle (f r1 r2)
recProp f (Rect w1 h1)  (Rect w2 h2)    = Just $ Rect (f w1 w2) (f h1 h2)
recProp f (Donut r1 r2) (Donut r1' r2') = Just $ Donut (f r1 r1') (f r2 r2')
recProp f _             _               = Nothing --Mismatch! TODO: Add some kind of fail representation here.


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
    let width = toNumber (r2-r1)
    setLineWidth width
    beginPath
    arc {x: 0.0, y:0.0, r: (toNumber r1) + (width / 2.0), start: 0.0, end: 2.0*pi}
    closePath
    stroke