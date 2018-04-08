-- | Checking of connectedness (absence of "gaps", "breaks") of
-- route's ways sequence and detection of directions of ways.
--
-- The main function is 'connectedWays'

{-# LANGUAGE OverloadedStrings #-}

module Ptwatch.Connectedness

where

import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)
import qualified OSM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Direction of movement (in route terms) along OSM way.
data Direction
  = Forward -- ^ From first node to last node
  | Backward  -- ^ From last node to first node
  deriving (Eq, Ord, Show)

-- | OSM way with detected direction. Direction can be uncertain
-- ('Nothing') when way is loopy (starts and ends with the same node)
-- or if connected component of ways has too few ways. For example, if
-- route has only one way, and it is not one-way, then direction is
-- uncertain. All directions must be certain in order for route to be
-- valid.
data WayWithDirection v =
  WayWithDirection (Maybe Direction) (OSM.Way v)
  deriving (Eq, Ord, Show)

newtype Matcher s r = Matcher { match :: [s] -> (r, [s]) }
type WaysMatcher v = Matcher (OSM.Way v) [WayWithDirection v]

-- | Head of ways "parser" consisting of list of matched ways with
-- detected directions and last "head" node
data MatcherHead v =
  MatcherHead [WayWithDirection v] (Maybe OSM.NodeID) deriving (Show)

startingMatcherHead :: MatcherHead v
startingMatcherHead = MatcherHead [] Nothing

-- | Returns true if 'MatcherHead' can connect to given node id
headConnectsTo :: MatcherHead v -> OSM.NodeID -> Bool
headConnectsTo (MatcherHead _ Nothing) _ = True
headConnectsTo (MatcherHead _ (Just hn)) n = n == hn

data Oneway = Oneway | ReverseOneway | NotOneway deriving (Eq)

oneway :: OSM.Way v -> Oneway
oneway = ov . Map.lookup (OSM.TagKey "oneway") . OSM.tags
  where
    ov (Just x) | x `elem` ["yes", "1", "true"] = Oneway
                | x `elem` ["reversible", "-1", "reverse"] = ReverseOneway
                | otherwise = NotOneway
    ov Nothing = NotOneway

-- | Returns true if way is starting and ending with the same node
isLoopWay :: OSM.Way v -> Bool
isLoopWay way = head nids == last nids where nids = OSM.nodeIDs way

firstNode :: OSM.Way v -> OSM.NodeID
firstNode way = head $ OSM.nodeIDs way

lastNode :: OSM.Way v -> OSM.NodeID
lastNode way = last $ OSM.nodeIDs way

advanceHeadWith
  :: MatcherHead v
  -> WayWithDirection v
  -> OSM.NodeID
  -> MatcherHead v
advanceHeadWith (MatcherHead oldWays _) way node =
  MatcherHead (oldWays ++ [way]) (Just node)

advanceHeadForward :: MatcherHead v -> OSM.Way v -> [MatcherHead v]
advanceHeadForward head way =
  if headConnectsTo head (firstNode way)
  && not (isLoopWay way)
  && (oneway way) /= ReverseOneway
  then [advanceHeadWith head (WayWithDirection (Just Forward) way)
        (lastNode way)]
  else []

advanceHeadBackward :: MatcherHead v -> OSM.Way v -> [MatcherHead v]
advanceHeadBackward head way =
  if headConnectsTo head (lastNode way)
  && not (isLoopWay way)
  && (oneway way) /= Oneway
  then [advanceHeadWith head (WayWithDirection (Just Backward) way)
        (firstNode way)]
  else []

-- | Returns possibilities for advancing matcher head with loop way (a
-- way starting and ending with the same node). If it's not a loop
-- way, or can't be attached to MatcherHead, returns @[]@.
advanceHeadLoopWay :: MatcherHead v -> OSM.Way v -> [MatcherHead v]
advanceHeadLoopWay head way =
  if isLoopWay way && headConnectsTo head (firstNode way)
  then [advanceHeadWith head (WayWithDirection Nothing way)
        (firstNode way)]
  else []

advanceHead :: MatcherHead v -> OSM.Way v -> [MatcherHead v]
advanceHead head way = do
  f <- [advanceHeadForward, advanceHeadBackward, advanceHeadLoopWay]
  newHead <- f head way
  return newHead

advanceMatchers :: (Show v) => [MatcherHead v] -> OSM.Way v -> [MatcherHead v]
advanceMatchers heads ways = do
  head <- heads
  newHead <- advanceHead head ways
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
  (Show v)
  => NonEmpty (OSM.Way v)
  -> ([[WayWithDirection v]], [OSM.Way v])
firstConnectedWays ways = (connectVariants, remainingWays)
  where
    connectVariants = wayFromHead <$> finalHeads
    wayFromHead (MatcherHead way _) = way
    (finalHeads, remainingWays) =
      advanceMatchersWhilePossible [startingMatcherHead] ways

-- | For list of OSM ways specified in the same order as in
-- @type=route@ relation, return lists of ways with detected
-- directions. If route has breaks, multiple lists of ways will be
-- returned, each is connected component.
connectedWays :: (Show v) => [OSM.Way v] -> [[WayWithDirection v]]
connectedWays ways =
  case neWays of
    Nothing -> []
    Just ways' ->
      let (connectVariants, remainingWays) = firstConnectedWays ways'
          connectVariants' = multiToPossible connectVariants
      in
      case remainingWays of
        [] -> [connectVariants']
        _ -> connectVariants' : connectedWays remainingWays
  where
    neWays = nonEmpty ways
    multiToPossible ::
      (Show v) => [[WayWithDirection v]] -> [WayWithDirection v]
    multiToPossible [variant1, _variant2] = wayToUncertain <$> variant1
    multiToPossible [variant] = variant
    multiToPossible erroneous = error (show erroneous)
    wayToUncertain (WayWithDirection _ w) =
      WayWithDirection Nothing w
