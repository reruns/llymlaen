module Test.Helpers where

import Prelude ((<*>), (<$>))

import Test.QuickCheck.Gen (Gen)
import Data.Tuple (Tuple(Tuple))
import Test.Unit (Test, failure)

pairOf :: forall a. Gen a -> Gen (Tuple a a)
pairOf x = Tuple <$> x <*> x

notWritten :: forall e. Test e
notWritten = failure "Test not written"