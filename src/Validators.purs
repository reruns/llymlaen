module App.Validators where

import App.Element

import Prelude
import Data.Maybe (Maybe(Just,Nothing))
import Data.Int (fromString)

import Halogen.HTML.Events.Handler (EventHandler)

validateAngle value (Element el) = do
  pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {angle=v}}) <$> (validateNumber (between 0 360) value)

--ah, I see the reason we would use lenses now. We'll come back to that later, then.
validateWidth value (Element el) = do
  pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {size = el.current.size {w=v}}}) <$> (validateNumber ((<) 0) value)
      
validateHeight value (Element el) = do
   pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {size = el.current.size {h=v}}}) <$> (validateNumber ((<) 0) value)
   
validateRadius value (Element el) = do
   pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {radius=v}}) <$> (validateNumber ((<) 0) value)
   
validateR1 value (Element el) = do
  pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {size = el.current.size {r1=v}}}) <$> (validateNumber (between 1 (el.current.size.r2-1)) value)
  
validateR2 value (Element el) = do
  pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {size = el.current.size {r2=v}}}) <$> (validateNumber ((<) el.current.size.r1) value)
      
validateX value (Element el) = do
  pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {pos = el.current.pos {x=v}}}) <$> (validateNumber ((<) 0) value)
  
validateY value (Element el) = do
  pure $ (\v -> unfoldDrawable $ Element $ el {current=el.current {pos = el.current.pos {y=v}}}) <$> (validateNumber ((<) 0) value)
      
validateNumber p v = 
  let n = fromString v in
  case p <$> n of
    Just true -> n
    _         -> Nothing