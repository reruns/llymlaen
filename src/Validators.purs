module App.Validators where

import Prelude
import Data.Maybe (Maybe(Just,Nothing))
import Data.Int (fromString)

validateRange :: String -> Int -> Int -> Maybe Int
validateRange s min max = validateNumber (between min max) s

validateNonNeg :: String -> Maybe Int
validateNonNeg s = validateNumber ((<=) 0) s

validateSetTime value max = do
  pure $ validateNumber (between 0 max) value

validateNumber p v = 
  let n = fromString v in
  case p <$> n of
    Just true -> n
    _         -> Nothing