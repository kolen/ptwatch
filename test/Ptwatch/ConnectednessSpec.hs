module Ptwatch.ConnectednessSpec (spec) where

import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import OSM
import OSM.OSMTestHelper
import Ptwatch.Connectedness

spec :: Spec
spec = do
  describe "Ptwatch.Connectedness" $ do
    describe "waysDirections" $ do
      it "returns list containing the same ways as input" $ property $
        changeDepth (const 3) $
        \fakeroute -> let ways = fromFakePathSequence fakeroute in
          (way <$> concat (waysDirections ways)) == ways
    describe "waysDirectionsComponent" $ do
      it "returns list containing the same ways as input" $ property $
        changeDepth (const 5) $
        \fakeroute -> let ways = fromFakePathSequence fakeroute in
          let (detected, remaining) = waysDirectionsComponent ways in
            (way <$> detected) ++ remaining == ways
      it "detects at least one way" $ property $
        \fakeroute -> let ways = fromFakePathSequence fakeroute in
          let (detected, _) = waysDirectionsComponent ways in
            not (null detected) || null ways
