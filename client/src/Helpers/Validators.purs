module App.Helpers.Validators where

import App.Prelude
import App.Types.RGB
import Data.String (length) as S

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
    
validateRGB :: String -> Maybe RGB
validateRGB s 
  | S.length s /= 7 = Nothing
  | otherwise = (\r g b -> RGB {r,g,b})
    <$> (fromStringAs hexadecimal $ take 2 $ drop 1 s)
    <*> (fromStringAs hexadecimal $ take 2 $ drop 3 s)
    <*> (fromStringAs hexadecimal $ take 2 $ drop 5 s)