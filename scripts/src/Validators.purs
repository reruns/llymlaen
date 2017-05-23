module App.Validators where

import Prelude
import Data.Maybe (Maybe(Just,Nothing))
import Data.Int (fromString)

validateRange :: String -> Int -> Int -> Maybe Int
validateRange s min max = validateNumber (between min max) s

validateNonNeg :: String -> Maybe Int
validateNonNeg s = validateNumber ((<=) 0) s

validateSetTime :: String -> Int -> Maybe Int
validateSetTime value max = validateNumber (between 0 max) value

validateNumber :: (Int -> Boolean) -> String -> Maybe Int
validateNumber p v = 
  let n = fromString v in
  case p <$> n of
    Just true -> n
    _         -> Nothing