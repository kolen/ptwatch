{-# LANGUAGE OverloadedStrings #-}

module Ptwatch.ConnectednessSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)
import Data.Maybe (fromJust)
import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HaskellWorks.Hspec.Hedgehog
import qualified OSM
import qualified Ptwatch.Connectedness as C

simpleWaysG :: Gen (NonEmpty (OSM.Way ()))
simpleWaysG = fromJust <$> nonEmpty <$> Gen.list (Range.linear 2 10) ( OSM.way
  <$> (OSM.WayID <$> (Gen.int64 (Range.constant 0 5)))
  <*> Gen.constant Map.empty
  <*> Gen.list (Range.linear 2 3)
               (OSM.NodeID <$> (Gen.int64 (Range.constant 0 5))))

wayG :: Gen (OSM.Way ())
wayG = OSM.way
  <$> (OSM.WayID <$> (Gen.int64 (Range.constant 0 5)))
  <*> direction
  <*> Gen.list (Range.linear 2 3)
               (OSM.NodeID <$> (Gen.int64 (Range.constant 0 5)))
  where
    direction = Gen.choice [ Gen.constant Map.empty
                           , Gen.constant $ Map.fromList [(oneway, "yes")]
                           , Gen.constant $ Map.fromList [(oneway, "reverse")]
                           ]
    oneway = OSM.TagKey "oneway"

simpleMatcherHeadG :: Gen (C.MatcherHead ())
simpleMatcherHeadG = do
  lastNodeID <- OSM.NodeID <$> Gen.int64 (Range.constant 2 10)
  let way = OSM.way (OSM.WayID 1) Map.empty [OSM.NodeID 1, lastNodeID]
  return $ C.MatcherHead [C.WayWithDirection (Just C.Forward) way] (Just lastNodeID)

-- | Extending head with any way gives one or zero results when head
-- has last point
prop_extendsOneOrZeroForNonFirst :: Property
prop_extendsOneOrZeroForNonFirst = withTests 1000 $ property $ do
  head <- forAll simpleMatcherHeadG
  way <- forAll wayG
  let result = C.advanceHead head way
  annotateShow result
  assert $ (length result) == 1 || (length result) == 0

prop_extendsEmptyConsumesNonOneway :: Property
prop_extendsEmptyConsumesNonOneway = withTests 300 $ property $ do
  let matcherHead = C.MatcherHead [] Nothing
  way <- forAll nonOnewayWay
  let result = C.advanceHead matcherHead way

  Set.fromList (headWay <$> result) ===
    if head (OSM.nodeIDs way) == last (OSM.nodeIDs way)
    then Set.fromList [ [C.WayWithDirection Nothing way] ]
    else Set.fromList [ [C.WayWithDirection (Just C.Forward) way]
                      , [C.WayWithDirection (Just C.Backward) way] ]
    where
      headWay (C.MatcherHead w _) = w
      nonOnewayWay :: Gen (OSM.Way ())
      nonOnewayWay = OSM.way
        <$> (OSM.WayID <$> (Gen.int64 (Range.constant 1 5)))
        <*> Gen.constant Map.empty
        <*> Gen.list (Range.linear 2 3)
        (OSM.NodeID <$> (Gen.int64 (Range.constant 1 5)))

prop_firstConnectedWaysAtLeastOneWay :: Property
prop_firstConnectedWaysAtLeastOneWay = property $ do
  ways <- forAll simpleWaysG
  let (variants, remaining) = C.firstConnectedWays ways
  annotateShow variants
  assert $ (length variants) >= 1

prop_firstConnectedWaysReturnRemaining :: Property
prop_firstConnectedWaysReturnRemaining = property $ do
  ways <- forAll simpleWaysG
  let (variants, remaining) = C.firstConnectedWays ways
  annotateShow variants
  annotateShow remaining
  assert $ all (\wds -> length wds + length remaining == length ways) variants

prop_connectedWaysPreservesCount :: Property
prop_connectedWaysPreservesCount = withTests 2000 $ property $ do
  ways <- forAll $ Gen.list (Range.constant 1 6) wayG
  let result = C.connectedWays ways
  sum (length <$> result) === length ways

spec = do
  describe "Ptwatch.Connectedness" $ do
    describe "advanceMatcher" $ do
      context "when starting from single way" $ do
        it "returns result of one or zero choices" $
          require prop_extendsOneOrZeroForNonFirst
      context "when starging from empty head" $ do
        context "for non-oneway ways" $ do
          it "returns result of two variants" $
            require prop_extendsEmptyConsumesNonOneway
    describe "firstConnectedWays" $ do
      context "for nonempty input" $ do
        it "returns list with at least one way" $ do
          require prop_firstConnectedWaysAtLeastOneWay
        it "returns count of ways + remaining the same as input count" $ do
          require prop_firstConnectedWaysReturnRemaining
    describe "connectedWays" $ do
      it "returns the same sum count of ways in results as in input" $ do
        require prop_connectedWaysPreservesCount
