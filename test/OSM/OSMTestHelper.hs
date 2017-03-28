{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module OSM.OSMTestHelper where

import qualified Data.Map.Strict as Map
import Data.List
import Control.Applicative
import OSM
import Test.SmallCheck.Series
import Data.Time.Clock
import Data.Time.Calendar

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

data FakePathSegment = NormalFakePathSegment
                     | OnewayFakePathSegment
                     | ReverseOnewayFakePathSegment
                     | CycleFakePathSegment
                     | FakePathBreak
instance Monad m => Serial m FakePathSegment where
  series = cons0 NormalFakePathSegment
        \/ cons0 OnewayFakePathSegment
        \/ cons0 ReverseOnewayFakePathSegment
        \/ cons0 CycleFakePathSegment
        \/ cons0 FakePathBreak

data FakePathSequence = FakePathSequence [FakePathSegment]

fromFakePathSequence :: FakePathSequence -> [Way]
fromFakePathSequence (FakePathSequence fps) = snd $ mapAccumL accum 1 fps
  where
    accum :: Integer -> FakePathSegment -> (Integer, Way)
    accum i NormalFakePathSegment =
      (i + 2, Element (wayid i) Map.empty (nodes i) emptyVersionInfo)
    accum i OnewayFakePathSegment =
      (i + 2, Element (wayid i) (oneway "yes") (nodes i) emptyVersionInfo)
    accum i ReverseOnewayFakePathSegment =
      (i + 2, Element (wayid i) (oneway "-1") (nodes i) emptyVersionInfo)
    accum i CycleFakePathSegment =
      (i + 1, Element (wayid i) Map.empty (cycleNodes i) emptyVersionInfo)
    accum i FakePathBreak =
      (i + 3, Element (wayid i) Map.empty (nodes i) emptyVersionInfo)

    nodes i = [NodeID i, NodeID (i + 1), NodeID (i + 2)]
    cycleNodes i = [NodeID i, NodeID (i + 1), NodeID i]
    oneway t = (Map.singleton (TagKey "oneway") t)
    wayid i = (WayID (i + 1))
