{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module OSMTest where

import qualified Data.Map.Strict as Map
import OSM
import Test.SmallCheck.Series
import Data.Time.Clock
import Data.Time.Calendar

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
instance Monad m => Serial m Way where
  series = cons4 Element
instance Monad m => Serial m Relation where
  series = cons4 Element

-- instance Monad m => Serial m Node
-- instance Monad m => Serial m Way
-- instance Monad m => Serial m Relation
instance Monad m => Serial m Dataset
