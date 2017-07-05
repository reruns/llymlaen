module App.Types.Diag where

import Prelude

import App.Types.RGB
import App.Types.Element

import Data.Array (insertBy)
import Data.Argonaut

import Test.QuickCheck(class Arbitrary, arbitrary)

newtype Diag = Diag { color :: RGB
                    , length :: Int
                    , elements :: Array Element
                    }
                    
getElements :: Diag -> Array Element
getElements (Diag d) = d.elements

getColor :: Diag -> RGB
getColor (Diag d) = d.color

getLength :: Diag -> Int
getLength (Diag d) = d.length

setElements :: Diag -> Array Element -> Diag
setElements (Diag d) es = Diag $ d {elements = es}

setColor :: RGB -> Diag -> Diag
setColor c (Diag d)  = Diag $ d {color = c}

setLength :: Int -> Diag -> Diag
setLength l (Diag d)  = Diag $ d {length = l}
                    
instance encodeDiag :: EncodeJson Diag where
  encodeJson (Diag {color,length,elements}) = 
    "Color"    := color ~> 
    "Length"   := length ~>
    "Elements" := elements ~> 
    jsonEmptyObject
    
instance decodeDiag :: DecodeJson Diag where
  decodeJson json = do
    obj <- decodeJson json
    color <- obj .? "Color"
    length <- obj .? "Length"
    elements <- obj .? "Elements"
    pure $ Diag {color,length,elements}
  
instance arbDiag :: Arbitrary Diag where
  arbitrary = (\color length elements -> Diag {color,length,elements}) <$> arbitrary <*> arbitrary <*> arbitrary
  
derive instance eqDiag :: Eq Diag
  
addElement :: Diag -> Element -> Diag
addElement (Diag d) el = Diag (d {elements = insertBy (comparing getLayer) el d.elements})