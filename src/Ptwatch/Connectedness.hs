{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}

module Ptwatch.Connectedness
  ( WayWithDirection
  , WayWithUncertainDirection(..)
  , Direction
  , UncertainDirection(..)
  , waysDirections
  , waysDirectionsComponent
  , way
  )
where

import qualified OSM
import qualified Data.Map.Strict as Map
import Control.Applicative
import Debug.Trace
import Test.SmallCheck.Series
import GHC.Generics

data WayWithDirection = WayWithDirection OSM.Way Direction
  deriving Generic
data WayWithUncertainDirection =
  WayWithUncertainDirection OSM.Way UncertainDirection
  deriving (Show, Generic, Eq)

class WayAndDirection w where
  way :: w -> OSM.Way
instance WayAndDirection WayWithDirection where
  way (WayWithDirection w _) = w
instance WayAndDirection WayWithUncertainDirection where
  way (WayWithUncertainDirection w _) = w

data Direction = Forward | Backward deriving (Eq, Show, Generic)
data UncertainDirection = KnownDirection Direction | UnknownDirection
  deriving (Show, Eq)
data Oneway = Oneway | ReverseOneway | NotOneway

-- | List of possible directions of UncertainDirection
directionVariants :: UncertainDirection -> [Direction]
directionVariants (KnownDirection d) = [d]
directionVariants UnknownDirection   = [Forward, Backward]

onewayDirection :: Oneway -> UncertainDirection
onewayDirection Oneway = KnownDirection Forward
onewayDirection ReverseOneway = KnownDirection Backward
onewayDirection NotOneway = UnknownDirection

oneway :: OSM.Way -> Oneway
oneway = ov . Map.lookup (OSM.TagKey "oneway") . OSM.tags
  where
    ov :: Maybe String -> Oneway
    ov (Just x) | x `elem` ["yes", "1", "true"] = Oneway
                | x `elem` ["reversible", "-1", "reverse"] = ReverseOneway
                | otherwise = NotOneway
    ov Nothing = NotOneway

clarify :: UncertainDirection -> UncertainDirection -> UncertainDirection
clarify (KnownDirection d1) _ = KnownDirection d1
clarify UnknownDirection d2 = d2

wayDirectionByItself :: OSM.Way -> UncertainDirection
wayDirectionByItself = onewayDirection . oneway

startPoint :: WayWithDirection -> OSM.NodeID
startPoint (WayWithDirection w Forward) = head $ OSM.nodeIds w
startPoint (WayWithDirection w Backward) = last $ OSM.nodeIds w

endPoint :: WayWithDirection -> OSM.NodeID
endPoint (WayWithDirection w Forward) = last $ OSM.nodeIds w
endPoint (WayWithDirection w Backward) = head $ OSM.nodeIds w

canBePaired :: WayWithDirection -> WayWithDirection -> Bool
canBePaired w1 w2 = endPoint w1 == startPoint w2

-- | Infer direction of way in connection with previous way
wayDirectionByPair :: Maybe WayWithUncertainDirection -- ^ Previous way if any
                   -> OSM.Way                         -- ^ Current way
                   -> Maybe UncertainDirection
                   -- ^ Inferred direction or 'Nothing' if pair of ways cannot
                   -- be connected
wayDirectionByPair Nothing     _   = Just UnknownDirection
wayDirectionByPair (Just (WayWithUncertainDirection prevW prevD) ) way =
  case (Forward `elem` directions, Backward `elem` directions) of
    (False, False) -> Nothing
    (False, True)  -> Just (KnownDirection Backward)
    (True,  False) -> Just (KnownDirection Forward)
    (True,  True)  -> Just UnknownDirection
  where
    directions = [p | p <- directionVariants prevD, c <- [Forward, Backward],
      canBePaired (WayWithDirection prevW p) (WayWithDirection way c)]

-- | Infer direction of way in connection with adjacent ways
wayDirection :: Maybe WayWithUncertainDirection
             -- ^ Previous way if any ('Nothing' if this way is starting way)
             -> OSM.Way
             -- ^ Current way of which detect direction
             -> [OSM.Way]
             -- ^ List of next ways
             -> Maybe UncertainDirection
             -- ^ Inferred direction or 'Nothing' if way cannot be connected
             -- with previous or next ways
wayDirection prev way nexts = do
  let byItself = wayDirectionByItself way
  byPairPrev <- wayDirectionByPair prev way
  byPairNext <- nextPair way nexts
  return $ byItself `clarify` byPairPrev `clarify` byPairNext
    where
      nextPair :: OSM.Way -> [OSM.Way] -> Maybe UncertainDirection
      nextPair _ [] = Just UnknownDirection
      nextPair w (next1:nexts1) =
        wayDirection (Just (WayWithUncertainDirection w UnknownDirection)) next1 nexts1

-- | Infer directions of ways of first interconnected segment of ways list.
-- Remaining ways after first connection break are returned in remaining list
waysDirectionsComponent :: [OSM.Way]
                        -> ([WayWithUncertainDirection], [OSM.Way])
                        -- ^ List of ways with detected directions;
                        --   Remaining ways after end of interconnected segment
waysDirectionsComponent [] = ([], [])
waysDirectionsComponent ways =
  wdc ways Nothing
  where
    wdc :: [OSM.Way] -> Maybe WayWithUncertainDirection
        -> ([WayWithUncertainDirection], [OSM.Way])
    wdc ws@[]      _    = ([], ws)
    wdc ws@(w:ws') prev = let dir = wayDirection prev w ws'
      in case dir of
        Nothing    -> ([(WayWithUncertainDirection w UnknownDirection)], ws')
        (Just dir') -> let (wwd1, wr1) = wdc ws' (Just wwd)
                           wwd = WayWithUncertainDirection w dir'
                       in (wwd:wwd1, wr1)

-- | Infer direction on list of successive ways. Returns list of connected
-- components, should be one component if route is valid.
waysDirections :: [OSM.Way] -> [[WayWithUncertainDirection]]
waysDirections ways = let (component, remaining) =  waysDirectionsComponent ways
  in case (component, remaining) of
    ([], []) -> []
    (_,  []) -> [component]
    (_,   _) -> component : waysDirections remaining
