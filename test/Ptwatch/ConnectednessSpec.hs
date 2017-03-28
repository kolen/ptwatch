module Ptwatch.ConnectednessSpec (spec) where

import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import OSM
import OSM.OSMTest
import Ptwatch.Connectedness

spec :: Spec
spec = do
  describe "Ptwatch.Connectedness" $ do
    describe "waysDirections" $ do
      it "returns list containing the same ways as input" $ property $
        changeDepth (const 3) $
        over simpleWays $
          \ways -> (way <$> concat (waysDirections ways)) == ways
    describe "waysDirectionsComponent" $ do
      it "returns list containing the same ways as input" $ property $
        changeDepth (const 4) $
        over simpleWays $
          \ways -> let (detected, remaining) =
                         waysDirectionsComponent ways in
                     (way <$> detected) ++ remaining == ways
      it "detects at least one way" $ property $
        over simpleWays $
        \ways -> let (detected, _) = waysDirectionsComponent ways in
          not (null detected) || null ways
