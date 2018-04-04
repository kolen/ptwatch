{-# LANGUAGE OverloadedStrings #-}

module Ptwatch.Connectedness

where

import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)
import qualified OSM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

data Direction = Forward | Backward deriving (Eq, Ord, Show)
data WayWithPossibleDirection v =
  WayWithPossibleDirection (Maybe Direction) (OSM.Way v)
  deriving (Eq, Ord, Show)
data WayWithDirection v =
  WayWithDirection Direction (OSM.Way v) deriving (Eq, Ord, Show)

newtype Matcher s r = Matcher { match :: [s] -> (r, [s]) }
type WaysMatcher v = Matcher (OSM.Way v) [WayWithPossibleDirection v]

connectedWays :: [OSM.Way v] -> [[WayWithPossibleDirection v]]
connectedWays = undefined

data MatcherHead v =
  MatcherHead [WayWithDirection v] (Maybe OSM.NodeID) deriving (Show)

startingMatcherHead :: MatcherHead v
startingMatcherHead = MatcherHead [] Nothing

data Oneway = Oneway | ReverseOneway | NotOneway deriving (Eq)

oneway :: OSM.Way v -> Oneway
oneway = ov . Map.lookup (OSM.TagKey "oneway") . OSM.tags
  where
    ov (Just x) | x `elem` ["yes", "1", "true"] = Oneway
                | x `elem` ["reversible", "-1", "reverse"] = ReverseOneway
                | otherwise = NotOneway
    ov Nothing = NotOneway

advanceMatcherDir :: MatcherHead v -> OSM.Way v -> Direction -> [MatcherHead v]
advanceMatcherDir (MatcherHead oldWays startingNode) way direction =
  case direction of
    Forward  ->
      if (maybe True (\n -> firstNode == n) startingNode) &&
         (oneway' /= ReverseOneway)
      then [MatcherHead
             (oldWays ++ [WayWithDirection Forward way])
             (Just lastNode)]
      else []
    Backward ->
      if (maybe True (\n -> lastNode == n) startingNode) &&
         (oneway' /= Oneway)
      then [MatcherHead
             (oldWays ++ [WayWithDirection Backward way])
             (Just firstNode)]
      else []
  where
    firstNode = head $ OSM.nodes way
    lastNode  = last $ OSM.nodes way
    oneway' = oneway way

advanceMatcher :: MatcherHead v -> OSM.Way v -> [MatcherHead v]
advanceMatcher head way =
  [Forward, Backward] >>= advanceMatcherDir head way

advanceMatchers :: (Show v) => [MatcherHead v] -> OSM.Way v -> [MatcherHead v]
advanceMatchers heads ways = do
  head <- heads
  newHead <- advanceMatcher head ways
  return newHead

advanceMatchersWhilePossible ::
  (Show v) => [MatcherHead v] -> NonEmpty (OSM.Way v)
  -> ([MatcherHead v], [OSM.Way v])
advanceMatchersWhilePossible heads ways@(way:|restWays) =
  case advanceMatchers heads way of
    [] -> (heads, toList ways)
    newHeads -> case nonEmpty restWays of
      Nothing -> (newHeads, [])
      Just restWays' -> advanceMatchersWhilePossible newHeads restWays'

-- | Finds direction of first group of connected ways in list of
-- ways. Returns list of variants of how ways can be connected and
-- remaining ways after first break
firstConnectedWays ::
  (Show v) => NonEmpty (OSM.Way v) -> ([[WayWithDirection v]], [OSM.Way v])
firstConnectedWays ways = (connectVariants, remainingWays)
  where
    connectVariants = wayFromHead <$> finalHeads
    wayFromHead (MatcherHead way _) = way
    (finalHeads, remainingWays) =
      advanceMatchersWhilePossible [startingMatcherHead] ways
