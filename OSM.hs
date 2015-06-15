{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module OSM where

import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map

newtype NodeID = NodeID Integer deriving (Show, Eq, Ord)
newtype WayID = WayID Integer deriving (Show, Eq, Ord)
newtype RelationID = RelationID Integer deriving (Show, Eq, Ord)

newtype RelationRole = RelationRole String deriving (Show, Eq)

data ElementID =
  ElementNodeID NodeID |
  ElementWayID WayID |
  ElementRelationID RelationID
  deriving (Show, Eq)

data RelationMember =
  RelationMember ElementID RelationRole
  deriving (Show, Eq)

newtype Latitude = Latitude Float deriving (Show, Eq)
newtype Longitude = Longitude Float deriving (Show, Eq)
newtype TagKey = TagKey String deriving (Show, Eq, Ord)

type Tags = Map.Map TagKey String


data Coordinates = Coordinates Latitude Longitude deriving (Show, Eq)

data VersionInfo = VersionInfo
  { user :: Maybe String
  , uid :: Maybe Integer
  , timestamp :: Maybe UTCTime
  , visible :: Maybe Bool
  , version :: Maybe Integer
  , changeset :: Maybe Integer
  } deriving (Show, Eq)

data Node = Node NodeID Tags Coordinates VersionInfo deriving (Show, Eq)
data Way = Way WayID Tags [NodeID] VersionInfo deriving (Show, Eq)
data Relation = Relation RelationID Tags [RelationMember] VersionInfo deriving (Show, Eq)

class (Ord id) => Element el id | el -> id where
  getId :: el -> id

instance Element Node NodeID where
  getId (Node i _ _ _) = i

instance Element Way WayID where
  getId (Way i _ _ _) = i

instance Element Relation RelationID where
  getId (Relation i _ _ _) = i

data Dataset = Dataset (Map.Map NodeID Node)
                       (Map.Map WayID Way)
                       (Map.Map RelationID Relation) deriving (Show)
