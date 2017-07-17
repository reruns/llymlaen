module App.Types.Keyframe where

import App.Prelude

import App.Types.Property
import App.Types.Point

newtype Keyframe = Keyframe { time :: Int, props :: Array Property }

time :: Keyframe -> Int
time (Keyframe k) = k.time

setTime :: Int -> Keyframe -> Keyframe
setTime t (Keyframe k) = Keyframe $ k {time=t}

props :: Keyframe -> Array Property
props (Keyframe k) = k.props

instance encodeFrame :: EncodeJson Keyframe where
  encodeJson (Keyframe f)
    =  "time"  := f.time
    ~> "props" := f.props
    ~> jsonEmptyObject
  
instance decodeFrame :: DecodeJson Keyframe where
  decodeJson json = do
    obj <- decodeJson json
    time <- obj .? "time"
    props <- obj .? "props"
    pure $ Keyframe {time,props}
  
instance eqFrame :: Eq Keyframe where
  eq (Keyframe a) (Keyframe b) = ( a.time == b.time ) && ( a.props == b.props ) 
  
instance arbFrame :: Arbitrary Keyframe where
  arbitrary = (\t ps -> Keyframe {time:t,props:ps}) <$> arbitrary <*> arbitrary
  
--consider name change: interpolate?
reconcile :: Keyframe -> Keyframe -> Int -> Keyframe
reconcile (Keyframe {time:tl, props: left}) (Keyframe {time: tr, props: right}) t =
  let p = (toNumber (t-tl)) / (toNumber (tr-tl))
      f a b = a + (round $ p * (toNumber (b-a)))  
  in Keyframe {time: t, props: fromMaybe left $ sequence $ zipWith (recProp f) left right}

--note: this will compile but not work correctly if Position comes after the Shape property
overlap :: Keyframe -> Point -> Boolean
overlap (Keyframe {props}) (Point p) = _.b $ foldl f {d:p, a: 0.0 , b:false} props where
  f s@{d}     (Position (Point {x,y})) = 
    s { d= {x: p.x - x , y: p.y - y} }
  f res       (Angle a')       = res { a = -2.0 * pi * (toNumber a') / 360.0 }
  f s@{d,b}   (Circle r)       = s { b = b || ((sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)) <= (toNumber r))}
  f s@{d,a,b} (Rect w h)       = let rot = { x: (toNumber d.x) * (cos a) - (toNumber d.y) * (sin a)
                                       , y: (toNumber d.y) * (cos a) + (toNumber d.x) * (sin a) }
                                 in s { b = b || (rot.x >= 0.0 && rot.x <= (toNumber w) && rot.y >= 0.0 && rot.y <= (toNumber h)) }
  f s@{d,b}   (Donut r1 r2)    = let dist = sqrt $ (pow (toNumber d.x) 2.0) + (pow (toNumber d.y) 2.0)
                                 in s { b = b || (dist >= (toNumber r1) && dist <= (toNumber r2))}
  f res _                      = res
  
renderFrame :: Keyframe -> Graphics Unit
renderFrame (Keyframe {props}) = 
  do 
    save 
    let gfx = sequence $ map renderProp props
    case gfx of
      Nothing -> pure unit
      Just es -> sequence_ es
    restore
    
blankFrame :: Keyframe
blankFrame = Keyframe { time: -1, props: [] }