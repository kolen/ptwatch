module Main where

import Ptwatch.Connectedness

import Test.Framework as TF (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain tests

tests :: [TF.Test]
tests = [
        testGroup "Connectedness" [
          testProperty "waysDirections" prop_waysDirections
          ]
        ]