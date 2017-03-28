{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module OSM.OSMTestHelper where

import qualified Data.Map.Strict as Map
import Data.List
import Control.Applicative
import OSM
import Test.SmallCheck.Series
import Data.Time.Clock
import Data.Time.Calendar
import Debug.Trace

-- * Instances of Serial for basic OSM types and associated types

instance Monad m => Serial m UTCTime where
  series = cons2 UTCTime
instance Monad m => Serial m DiffTime where
  series = cons1 secondsToDiffTime
instance Monad m => Serial m Day where
  series = cons3 fromGregorian

instance Monad m => Serial m NodeID where
  series = newtypeCons NodeID
instance Monad m => Serial m WayID where
  series = newtypeCons WayID
instance Monad m => Serial m RelationID where
  series = newtypeCons RelationID
instance Monad m => Serial m RelationRole where
  series = newtypeCons RelationRole
instance Monad m => Serial m ElementID
instance Monad m => Serial m RelationMember
instance Monad m => Serial m Latitude where
  series = newtypeCons Latitude
instance Monad m => Serial m Longitude where
  series = newtypeCons Longitude
instance Monad m => Serial m TagKey where
  series = newtypeCons TagKey
instance (Ord k, Serial m k, Serial m v) => Serial m (Map.Map k v) where
  series = cons1 Map.fromList
instance Monad m => Serial m Coordinates
instance Monad m => Serial m VersionInfo

instance Monad m => Serial m Node where
  series = cons4 Element

atLeast2Nodes :: (Serial m a, Monad m) => Series m [a]
atLeast2Nodes = (:) <$> series <~> ((:) <$> series <~> series)

instance Monad m => Serial m Way where
  series = decDepth (
    Element <$> series
            <~> series
            <~> atLeast2Nodes
            <~> series)
instance Monad m => Serial m Relation where
  series = cons4 Element

simpleWay :: Monad m => Series m Way
simpleWay = Element <$> pure (WayID 0) <*> pure Map.empty <*> series <*> pure emptyVersionInfo

simpleWays :: Monad m => Series m [Way]
simpleWays = cons0 [] \/ cons2 (:)

instance Monad m => Serial m Dataset

-- * Generation of simulated routes

-- |Route segment represented by way. First argument is if reversed
-- (except for cycle way), second is if to generate route
-- discontinuity (break) after
data FakePathSegment = NormalFakePathSegment Bool Bool
                     | OnewayFakePathSegment Bool Bool
                     | CycleFakePathSegment Bool
                     deriving Show
instance Monad m => Serial m FakePathSegment where
  series = cons2 NormalFakePathSegment
        \/ cons2 OnewayFakePathSegment
        \/ cons1 CycleFakePathSegment

hasBreak :: FakePathSegment -> Bool
hasBreak (NormalFakePathSegment _ True) = True
hasBreak (OnewayFakePathSegment _ True) = True
hasBreak (CycleFakePathSegment True) = True
hasBreak _ = False

-- |Represents fake route
newtype FakePathSequence = FakePathSequence [FakePathSegment] deriving (Show)
instance Monad m => Serial m FakePathSequence where
  series = newtypeCons FakePathSequence

hasBreaks :: FakePathSequence -> Bool
hasBreaks (FakePathSequence ways) = case findIndex hasBreak ways of
  Nothing -> False
  Just x -> x /= ((length ways) - 1)

fromFakePathSequence :: FakePathSequence -> [Way]
fromFakePathSequence (FakePathSequence fps) = snd $ mapAccumL accum 1 fps
  where
    accum :: Integer -> FakePathSegment -> (Integer, Way)
    accum i (NormalFakePathSegment rev brk) =
      (i + 2 + (b' brk),
       Element (wayid i) Map.empty (nodes i rev) emptyVersionInfo)
    accum i (OnewayFakePathSegment False brk) =
      (i + 2 + (b' brk),
       Element (wayid i) (oneway "yes") (nodes i False) emptyVersionInfo)
    accum i (OnewayFakePathSegment True brk) =
      (i + 2 + (b' brk),
       Element (wayid i) (oneway "-1") (nodes i True) emptyVersionInfo)
    accum i (CycleFakePathSegment brk) =
      (i + 0 + (b' brk),
       Element (wayid i) Map.empty (cycleNodes i) emptyVersionInfo)

    nodes i False = [NodeID i, NodeID (i + 1), NodeID (i + 2)]
    nodes i True  = [NodeID (i + 2), NodeID (i + 1), NodeID i]
    cycleNodes i =  [NodeID i, NodeID (i + 1), NodeID i]
    oneway t = (Map.singleton (TagKey "oneway") t)
    wayid i = (WayID (i + 1))
    b' True = 1
    b' False = 0
