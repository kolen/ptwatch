-- | Checking of connectedness (absence of "gaps", "breaks") of
-- route's ways sequence and detection of directions of ways.
--
-- The main function is 'connectedWays'

{-# LANGUAGE OverloadedStrings #-}

module Ptwatch.Connectedness
  ( MatcherHead(..)
  , WayIDWithDirection(..)
  , Direction(..)
  , advanceHead
  , firstConnectedWays
  , connectedWays ) where

import Data.List.NonEmpty (NonEmpty(..), toList, nonEmpty)
import qualified OSM
import qualified OSM.OverpassJSON
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Aeson

-- | Direction of movement (in route terms) along OSM way.
data Direction
  = Forward -- ^ From first node to last node
  | Backward  -- ^ From last node to first node
  deriving (Eq, Ord, Show)

-- | OSM way id with detected direction. Direction can be uncertain
-- ('Nothing') when way is loopy (starts and ends with the same node)
-- or if connected component of ways has too few ways. For example, if
-- route has only one way, and it is not one-way, then direction is
-- uncertain. All directions must be certain in order for route to be
-- valid.
data WayIDWithDirection =
  WayIDWithDirection (Maybe Direction) OSM.WayID
  deriving (Eq, Ord, Show)

-- | Used only to define Aeson instances on 'Maybe
-- Direction'. Probably should be used everywhere instead of 'Maybe
-- Direction'.
newtype UncertainDirection =
  UncertainDirection { fromUncertainDirection :: Maybe Direction }

instance ToJSON UncertainDirection where
  toJSON (UncertainDirection d) = case d of
    Just Forward -> "forward"
    Just Backward -> "backward"
    Nothing -> "unknown"
instance FromJSON UncertainDirection where
  parseJSON (String "forward") = return $ UncertainDirection $ Just Forward
  parseJSON (String "backward") = return $ UncertainDirection $ Just Backward
  parseJSON (String "unknown") = return $ UncertainDirection $ Nothing
  parseJSON x = fail $ "Invalid direction " ++ (show x)

instance ToJSON WayIDWithDirection where
  toJSON (WayIDWithDirection dir id) = toJSON (UncertainDirection dir, id)
instance FromJSON WayIDWithDirection where
  parseJSON v = do
    (dir, id) <- parseJSON v
    return $ WayIDWithDirection (fromUncertainDirection dir) id

-- | Head of ways "parser" consisting of list of matched ways with
-- detected directions and last "head" node
data MatcherHead =
  MatcherHead [WayIDWithDirection] (Maybe OSM.NodeID) deriving (Show)

startingMatcherHead :: MatcherHead
startingMatcherHead = MatcherHead [] Nothing

-- | Returns true if 'MatcherHead' can connect to given node id
headConnectsTo :: MatcherHead -> OSM.NodeID -> Bool
headConnectsTo (MatcherHead _ Nothing) _ = True
headConnectsTo (MatcherHead _ (Just hn)) n = n == hn

-- | One-way status of OSM way. See
-- https://wiki.openstreetmap.org/wiki/Key:oneway
data Oneway
  = Oneway -- ^ One-way road with direction from first node to last node
  | ReverseOneway -- ^ One-way with reverse direction (@oneway=-1@,
                  -- etc). Not recommended to use such tagging in OSM,
                  -- just reversing the way is usually better.
  | NotOneway -- ^ Not an one-way road
  deriving (Eq)

-- | Returns one-way status of way
oneway :: OSM.Way v -> Oneway
oneway = ov . Map.lookup (OSM.TagKey "oneway") . OSM.tags
  where
    ov (Just x) | x `elem` ["yes", "1", "true"] = Oneway
                | x `elem` ["-1", "reverse"] = ReverseOneway
                | otherwise = NotOneway
    ov Nothing = NotOneway

-- | Returns true if way is "loopy" or "closed": starting and ending
-- with the same node. Usually it's discouraged to map roads this way,
-- closed ways are for polygon geometry, and in routes such ways are
-- especially nasty.
isLoopWay :: OSM.Way v -> Bool
isLoopWay way = head nids == last nids where nids = OSM.nodeIDs way

firstNode :: OSM.Way v -> OSM.NodeID
firstNode way = head $ OSM.nodeIDs way

lastNode :: OSM.Way v -> OSM.NodeID
lastNode way = last $ OSM.nodeIDs way

advanceHeadWith
  :: MatcherHead
  -> WayIDWithDirection
  -> OSM.NodeID
  -> MatcherHead
advanceHeadWith (MatcherHead oldWays _) way node =
  MatcherHead (oldWays ++ [way]) (Just node)

-- | Returns possibilities for advancing matcher head with way treated
-- as having forward direction.
advanceHeadForward :: MatcherHead -> OSM.Way v -> [MatcherHead]
advanceHeadForward head way =
  if headConnectsTo head (firstNode way)
  && not (isLoopWay way)
  && (oneway way) /= ReverseOneway
  then [advanceHeadWith head (WayIDWithDirection (Just Forward) (OSM.id way))
        (lastNode way)]
  else []

-- | Returns possibilities for advancing matcher head with way treated
-- as having backward direction.
advanceHeadBackward :: MatcherHead -> OSM.Way v -> [MatcherHead]
advanceHeadBackward head way =
  if headConnectsTo head (lastNode way)
  && not (isLoopWay way)
  && (oneway way) /= Oneway
  then [advanceHeadWith head (WayIDWithDirection (Just Backward) (OSM.id way))
        (firstNode way)]
  else []

-- | Returns possibilities for advancing matcher head with loop way (a
-- way starting and ending with the same node). If it's not a loop
-- way, or can't be attached to MatcherHead, returns @[]@.
advanceHeadLoopWay :: MatcherHead -> OSM.Way v -> [MatcherHead]
advanceHeadLoopWay head way =
  if isLoopWay way && headConnectsTo head (firstNode way)
  then [advanceHeadWith head (WayIDWithDirection Nothing (OSM.id way))
        (firstNode way)]
  else []

-- | Returns possibilities of advancing matcher head with a way, it
-- may return zero possibilities (head can't be continued with this
-- way), one (there is one unambigous way to continue matcher head)
-- and two (head can be continued by going through way in two
-- directions).
advanceHead :: MatcherHead -> OSM.Way v -> [MatcherHead]
advanceHead head way = do
  f <- [advanceHeadForward, advanceHeadBackward, advanceHeadLoopWay]
  newHead <- f head way
  return newHead

-- | Advances multiple matcher heads with 'advanceHead', return new
-- list of possible heads extended with given way.
advanceHeads :: (Show v) => [MatcherHead] -> OSM.Way v -> [MatcherHead]
advanceHeads heads ways = do
  head <- heads
  newHead <- advanceHead head ways
  return newHead

-- | Advances multiple matcher heads as far as possible (when at least
-- one match can be made), returns new list of heads and remaining
-- ways (which is similar to classical parser combinators)
advanceHeadsWhilePossible ::
  (Show v) => [MatcherHead] -> NonEmpty (OSM.Way v)
  -> ([MatcherHead], [OSM.Way v])
advanceHeadsWhilePossible heads ways@(way:|restWays) =
  case advanceHeads heads way of
    [] -> (heads, toList ways)
    newHeads -> case nonEmpty restWays of
      Nothing -> (newHeads, [])
      Just restWays' -> advanceHeadsWhilePossible newHeads restWays'

-- | Finds direction of first group of connected ways in list of
-- ways. Returns list of variants of how ways can be connected and
-- remaining ways after first break
firstConnectedWays ::
  (Show v)
  => NonEmpty (OSM.Way v)
  -> ([[WayIDWithDirection]], [OSM.Way v])
firstConnectedWays ways = (connectVariants, remainingWays)
  where
    connectVariants = wayFromHead <$> finalHeads
    wayFromHead (MatcherHead way _) = way
    (finalHeads, remainingWays) =
      advanceHeadsWhilePossible [startingMatcherHead] ways

-- | For list of OSM ways specified in the same order as in
-- @type=route@ relation, return lists of ways with detected
-- directions. If route has breaks, multiple lists of ways will be
-- returned, each is connected component.
connectedWays :: (Show v) => [OSM.Way v] -> [[WayIDWithDirection]]
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
    multiToPossible :: [[WayIDWithDirection]] -> [WayIDWithDirection]
    multiToPossible [variant1, _variant2] = wayToUncertain <$> variant1
    multiToPossible [variant] = variant
    multiToPossible erroneous = error (show erroneous)
    wayToUncertain (WayIDWithDirection _ w) =
      WayIDWithDirection Nothing w
