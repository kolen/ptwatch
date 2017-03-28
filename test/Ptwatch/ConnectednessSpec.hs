module Ptwatch.ConnectednessSpec (spec) where

import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import OSM
import OSM.OSMTestHelper
import Ptwatch.Connectedness
import Debug.Trace

spec :: Spec
spec = do
  describe "Ptwatch.Connectedness" $ do
    describe "waysDirections" $ do
      it "returns [] for []" $ do
        waysDirections [] `shouldBe` []
      it "returns two unknown dir segments for broken route of two ways" $ do
        let way1 = Element (WayID 1) emptyTags [NodeID 1, NodeID 2]
              emptyVersionInfo
            way2 = Element (WayID 2) emptyTags [NodeID 3, NodeID 4]
              emptyVersionInfo
          in waysDirections [way1, way2] `shouldBe`
             [[WayWithUncertainDirection way1 UnknownDirection],
              [WayWithUncertainDirection way2 UnknownDirection]]
      it "returns list containing the same ways as input" $ property $
        changeDepth (const 4) $
        \fakeroute -> let ways = fromFakePathSequence fakeroute in
          (way <$> concat (waysDirections ways)) == ways
    describe "waysDirectionsComponent" $ do
      it "returns ([], []) for []" $ do
        waysDirectionsComponent [] `shouldBe` ([], [])
      it "returns list containing the same ways as input" $ property $
        changeDepth (const 5) $
        \fakeroute -> let ways = fromFakePathSequence fakeroute in
          let (detected, remaining) = waysDirectionsComponent ways in
            (way <$> detected) ++ remaining == ways
      it "detects at least one way" $ property $
        \fakeroute -> let ways = fromFakePathSequence fakeroute in
          let (detected, _) = waysDirectionsComponent ways in
            not (null detected) || null ways
