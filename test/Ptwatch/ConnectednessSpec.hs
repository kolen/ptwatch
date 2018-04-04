{-# LANGUAGE OverloadedStrings #-}

module Ptwatch.ConnectednessSpec (spec) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import HaskellWorks.Hspec.Hedgehog
import qualified OSM
import qualified Ptwatch.Connectedness as C

ways :: Gen [OSM.Way ()]
ways = Gen.list (Range.linear 2 10) ( OSM.way
  <$> (OSM.WayID <$> (Gen.int64 (Range.constant 0 5)))
  <*> Gen.constant Map.empty
  <*> Gen.list (Range.linear 2 3)
               (OSM.NodeID <$> (Gen.int64 (Range.constant 0 5))))

way :: Gen (OSM.Way ())
way = OSM.way
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

simpleMatcherHead :: Gen (C.MatcherHead ())
simpleMatcherHead = do
  lastNodeID <- OSM.NodeID <$> Gen.int64 (Range.constant 2 10)
  let way = OSM.way (OSM.WayID 1) Map.empty [OSM.NodeID 1, lastNodeID]
  return $ C.MatcherHead [C.WayWithDirection C.Forward way] (Just lastNodeID)

prop_extendsGivesResult :: Property
prop_extendsGivesResult = property $ do
  head <- forAll simpleMatcherHead
  way <- forAll way
  let result = C.advanceMatcher head way
  assert $ (length result) == 1 || (length result) == 0

prop_extendsEmptyConsumesNonOneway :: Property
prop_extendsEmptyConsumesNonOneway = property $ do
  let head = C.MatcherHead [] Nothing
  way <- forAll nonOnewayWay
  let result = C.advanceMatcher head way
  Set.fromList (headWay <$> result) ===
    Set.fromList [ [C.WayWithDirection C.Forward way]
                 , [C.WayWithDirection C.Backward way] ]
    where
      headWay (C.MatcherHead w _) = w
      nonOnewayWay :: Gen (OSM.Way ())
      nonOnewayWay = OSM.way
        <$> (OSM.WayID <$> (Gen.int64 (Range.constant 1 5)))
        <*> Gen.constant Map.empty
        <*> Gen.list (Range.linear 2 3)
        (OSM.NodeID <$> (Gen.int64 (Range.constant 1 5)))

spec = do
  describe "Ptwatch.Connectedness" $ do
    describe "advanceMatcher" $ do
      context "when starting from single way" $ do
        it "returns result of one or zero choices" $
          require prop_extendsGivesResult
      context "when starging from empty head" $ do
        context "for non-oneway ways" $ do
          it "returns result of two variants" $
            require prop_extendsEmptyConsumesNonOneway
