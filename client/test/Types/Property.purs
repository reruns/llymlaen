module Test.Types.Property where

import Prelude

import App.Types.Property
import Test.Helpers

import Data.Tuple
import Data.Either (Either(..))
import Data.Maybe (isJust, fromMaybe, Maybe(..))
import Data.Array (length, (!!), head, tail)
import Data.Foldable (and, foldr)
import Data.NonEmpty (NonEmpty(..))
import Data.Argonaut (encodeJson, decodeJson)

import Test.Unit
import Test.Unit.QuickCheck (quickCheck)

import Test.QuickCheck (Result(), class Arbitrary, arbitrary, (<?>))
import Test.QuickCheck.Gen
 
 
tests = 
  suite "Property" do
    test "reconciling" do
      quickCheck sameSucceeds
      quickCheck mismatchFails
      quickCheck fIsApplied
    test "JSON Instances" do
      quickCheck jsonMatches
      

data MatchProps = Match Property Property
instance arbMatch :: Arbitrary MatchProps where
  arbitrary = (\(Tuple a b) -> Match a b) <$> (oneOf $ NonEmpty (pairOf $ (pure $ Enabled false)) $ map pairOf propGens)
  
sameSucceeds :: MatchProps -> Result
sameSucceeds (Match a b) = (isJust $ recProp const a b)
  <?> ("Matching properties failed to reconcile.")
  
fIsApplied :: MatchProps -> Result
fIsApplied (Match a b) = (and [ (Just a) == (recProp const a b)
                             , (Just b) == (recProp (flip const) a b) || boolProp a
                             ])
 <?> ("Reconciled properties don't look right.")
  
jsonMatches :: Property -> Result
jsonMatches p = ((Right p) == (decodeJson $ encodeJson p)) 
  <?> "Encoded Json didn't appear to match original data"
  

data MismatchProps = Mismatch Property Property
instance arbMismatch :: Arbitrary MismatchProps where
  arbitrary = do 
    ixs <- suchThat (pairOf $ chooseInt 0 $ (length propGens) - 1) (\(Tuple x y) -> x /= y)
    let lookup i = fromMaybe (pure $ Enabled false) $ propGens !! i
    (\(Tuple a b) -> Mismatch <$> (lookup a) <*> (lookup b)) ixs
    
mismatchFails :: MismatchProps -> Result
mismatchFails (Mismatch a b) = (not isJust $ recProp const a b)
  <?> ("Mismatched properties " <> show a <> " and " <> show b <> " illegally reconciled.")
  
  