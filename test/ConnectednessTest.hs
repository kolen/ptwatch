{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module ConnectednessTest where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.SmallCheck.Series
import Ptwatch.Connectedness
import OSM
import OSMTest

instance Monad m => Serial m WayWithDirection
instance Monad m => Serial m Direction

connectednessProps = localOption (SC.SmallCheckDepth 3) $
  testGroup "Connectedness"
  [ SC.testProperty "waysDirections contain the same ways as input" $
    over simpleWays $ \ways -> (way <$> concat (waysDirections ways)) == ways
  , SC.testProperty "waysDirectionsComponent contain the same ways as input" $
    \ways -> let (detected, remaining) = waysDirectionsComponent ways in
      (way <$> detected) ++ remaining == ways
  , SC.testProperty "waysDirectionsComponent detects at least one way" $
    \ways -> let (detected, _) = waysDirectionsComponent ways in
      not (null detected) || null ways
  ]
