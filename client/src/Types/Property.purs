module App.Types.Property where

import App.Prelude
import App.Types.RGB
import App.Types.Point

import Data.NonEmpty (NonEmpty(..))
import Data.Either (Either(..))
import Data.StrMap as SM

data Property = Enabled  Boolean
              | Bordered Boolean
              | Color    RGB
              | Position Point
              | Opacity  Int
              | Angle    Int
              | Circle   Int
              | Rect     Int Int
              | Donut    Int Int
              | Arc      Int Int

derive instance eqProp :: Eq Property

instance encodeProp :: EncodeJson Property where
  encodeJson  (Enabled b)      = "Enabled"  := b ~> jsonEmptyObject
  encodeJson  (Bordered b)     = "Bordered" := b ~> jsonEmptyObject
  encodeJson  (Color c)        = "Color" := c ~> jsonEmptyObject
  encodeJson  (Position p)     = "Position" := p ~> jsonEmptyObject
  encodeJson  (Angle a)        = "Angle" := a ~> jsonEmptyObject
  encodeJson  (Opacity o)      = "Opacity" := o ~> jsonEmptyObject
  encodeJson  (Circle r)       = "Circle" := r ~> jsonEmptyObject
  encodeJson  (Rect w h)       = "Rect" := ("w" := w ~> "h" := h ~> jsonEmptyObject) ~> jsonEmptyObject
  encodeJson  (Donut r1 r2)    = "Donut" := ("r1" := r1 ~> "r2" := r2 ~> jsonEmptyObject) ~> jsonEmptyObject
  encodeJson  (Arc   r th)     = "Arc" := ("r":= r ~> "th" := th ~> jsonEmptyObject) ~> jsonEmptyObject
  
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
                        pure $ Color $ RGB {r,g,b}
          "Position"-> do
                        xy <- obj .? "Position"
                        x  <- xy .? "x"
                        y  <- xy .? "y"
                        pure $ Position $ Point {x,y}
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
          "Arc"     -> do
                        arc <- obj .? "Arc"
                        r   <- arc .? "r"
                        th  <- arc .? "th"
                        pure $ Arc r th
          _         -> Left "Unrecognized key"
      
      
propGens :: Array (Gen Property)
propGens = [ Enabled <$> arbitrary
           , Bordered <$> arbitrary
           , Color <$> ((\r g b -> RGB {r,g,b}) <$> arbitrary <*> arbitrary <*> arbitrary)
           , Position <$> ((\x y -> Point {x,y}) <$> arbitrary <*> arbitrary)
           , Angle <$> arbitrary
           , Circle <$> arbitrary
           , Rect <$> arbitrary <*> arbitrary
           , Donut <$> arbitrary <*> arbitrary
           , Arc <$> arbitrary <*> arbitrary
           ]

instance arbProp :: Arbitrary Property where
  arbitrary = oneOf $ NonEmpty (pure $ Enabled false) propGens
  
instance showProp :: Show Property where
  show  (Enabled b)      = "Enabled: " <> (show b)
  show  (Bordered b)     = "Bordered: " <> (show b)
  show  (Color c)        = "Color: " <> show c
  show  (Position p)     = "Position: " <> show p
  show  (Angle a)        = "Angle: " <> (show a)
  show  (Opacity o)      = "Opacity: " <> (show o)
  show  (Circle r)       = "Circle: " <> (show r)
  show  (Rect w h)       = "Rectangle: " <> (show w) <> "x" <> (show h)
  show  (Donut r1 r2)    = "Donut: " <> (show r1) <> "-" <> (show r2)
  show  (Arc r th)       = "Arc: r - " <> (show r) <> " width - " <> (show th) 

--does this property wrap a boolean value?
boolProp :: Property -> Boolean
boolProp (Enabled _)  = true
boolProp (Bordered _) = true
boolProp _            = false

recProp f (Enabled b1)  (Enabled b2)    = Just $ Enabled b1
recProp f (Bordered b1) (Bordered b2)   = Just $ Bordered b1
recProp f (Color (RGB c1)) (Color (RGB c2)) = Just $ Color $ RGB {r: f c1.r c2.r, g: f c1.g c2.g, b: f c1.b c2.b}
recProp f (Position (Point p1)) (Position (Point p2)) = Just $ Position $ Point {x: f p1.x p2.x, y: f p1.y p2.y}
recProp f (Angle a1)    (Angle a2)      = Just $ Angle (f a1 a2)
recProp f (Opacity o1)  (Opacity o2)    = Just $ Opacity (f o1 o2)
recProp f (Circle r1)   (Circle r2)     = Just $ Circle (f r1 r2)
recProp f (Rect w1 h1)  (Rect w2 h2)    = Just $ Rect (f w1 w2) (f h1 h2)
recProp f (Donut r1 r2) (Donut r1' r2') = Just $ Donut (f r1 r1') (f r2 r2')
recProp f (Arc r1 th1)  (Arc r2 th2)    = Just $ Arc (f r1 r2) (f th1 th2)
recProp f _             _               = Nothing --Mismatch!


renderProp :: Property -> Maybe (Graphics Unit)
renderProp (Enabled b)      = if b then Just (pure unit) else Nothing
renderProp (Bordered b)     = if b 
                                then Just $ do 
                                  setStrokeStyle "#000000"
                                  setLineWidth 2.0
                                else Just (pure unit)
renderProp (Color c)        = Just $ (setFillStyle (show c)) *> (setStrokeStyle (show c))
renderProp (Position p)     = (\(Point {x,y}) -> Just $ translate (toNumber x) (toNumber y)) p
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
  setLineWidth 2.0
renderProp (Arc r th)       = Just $ 
  if th == 0
   then pure unit
   else do
      beginPath
      lineTo 0.0 0.0
      lineTo (toNumber r) 0.0
      arc {x: 0.0, y:0.0, r: toNumber r, start: 0.0, end: (toNumber th) / 180.0 * pi}
      closePath
      stroke
      fill