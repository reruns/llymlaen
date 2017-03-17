module Test.Helpers where

import Prelude ((<*>), (<$>))

import Test.QuickCheck.Gen (Gen)
import Data.Tuple (Tuple(Tuple))

pairOf :: forall a. Gen a -> Gen (Tuple a a)
pairOf x = Tuple <$> x <*> x